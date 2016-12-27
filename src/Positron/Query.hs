{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}

module Positron.Query
    ( queryInsert
    , queryUpsert
    , queryGet
    ) where

import Positron.Import

-- data types

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- extra modules

import qualified Database.PostgreSQL.LibPQ as PQ

-- local modules

import Positron.Driver
import Positron.Types
import Positron.Unsafe
import Positron.Util

queryInsert :: String -> String -> Q [Dec]
queryInsert = queryUpsertBase False

queryUpsert :: String -> String -> Q [Dec]
queryUpsert = queryUpsertBase True

queryUpsertBase :: Bool -> String -> String -> Q [Dec]
queryUpsertBase upsert queryStr tableName = getTable tableName >>=
    \ table -> let
        acols = map snd table
        allPKNames = mconcat $ intersperse ", " $
            map (snake . acn) $ filter acp acols
        columnTypes = map columnTypeCon acols
        columnArgs = map (VarP . mkName . ("_" ++) . fst) table
        columnNames = map (snake . fst) table
      in do
        mainContentExp <- [| mconcat
            [ $(return $ LitE $ StringL $
                "insert into " ++ snake tableName ++ " (")
            , mconcat $ intersperse ", " columnNames, ") values ("
            , toByteString $ mconcat $ intersperse ", "
                $(return $ ListE $ map expMake acols)
            , ")"
            , if upsert then mconcat
                [ " ON CONFLICT ("
                , allPKNames
                , ") DO UPDATE SET "
                , mconcat $ intersperse ", " $ flip map columnNames $
                    \cn -> mconcat [cn, " = EXCLUDED.", cn]
                ]
            else ""
            , ";"
            ]
            |]
        resultTypeSignature <- [t| IO (Either ByteString ()) |]
        return
            [ SigD queryName $
                AppT (AppT ArrowT (ConT ''Connection)) $
                    foldr (\x y -> AppT (AppT ArrowT x) y)
                        resultTypeSignature
                        columnTypes
            , FunD queryName
                [ Clause
                    (VarP (mkName "_conn") : columnArgs)
                    (NormalB $ AppE
                        (AppE (VarE 'unsafePlainExec) (VarE $ mkName "_conn"))
                        mainContentExp
                    )
                    []
                ]
            ]
  where
    queryName = mkName queryStr
    expMake AC{..} = case act of
        DBsmallint -> defaultWrapVarE 'B.int16Dec
        DBinteger -> defaultWrapVarE 'B.int32Dec
        DBbigint -> defaultWrapVarE 'B.int64Dec
        DBsmallserial -> defaultWrapVarE 'B.int16Dec
        DBserial -> defaultWrapVarE 'B.int32Dec
        DBbigserial -> defaultWrapVarE 'B.int64Dec
        DBvarchar _ -> textMake
        DBtext -> textMake
        _ -> defaultWrap bShow
      where
        textMake = wrap
            (VarE 'T.encodeUtf8Builder)
            (AppE
                (if acnl then AppE (VarE 'fmap) sanitize else sanitize)
                (VarE $ mkName ("_" ++ acn))
            )
        bShow = InfixE
            (Just (VarE 'B.byteString))
            (VarE '(.))
            (Just (InfixE
                (Just (VarE 'B.pack))
                (VarE '(.))
                (Just (VarE 'show))
            ))
        sanitize = InfixE (Just quote) (VarE '(.)) (Just escape)
        quote = InfixE
            (Just (InfixE (Just (LitE (StringL "'"))) (VarE '(<>)) Nothing))
            (VarE '(.))
            (Just (InfixE Nothing (VarE '(<>)) (Just (LitE (StringL "'")))))
        escape = AppE
            (AppE (VarE 'T.replace) (AppE (VarE 'T.pack) (LitE (StringL "'"))))
            (AppE (VarE 'T.pack) (LitE (StringL "''")))

        defaultWrapVarE = defaultWrap . VarE
        defaultWrap f = wrap f (VarE $ mkName $ "_" ++ acn)
        wrap converter value = if acnl
            then AppE
                (AppE (AppE (VarE 'maybe) (LitE (StringL "null"))) converter)
                value
            else AppE converter value

queryGet :: String -> String -> Q [Dec]
queryGet funcStr tableName = getTable tableName >>= \ columnMap -> do
    let
        columnNames = map (snake . fst) columnMap
        acols = map snd columnMap
        -- FIXME: support composite key (multiple primary keys)
        pk = head $ filter acp acols
        pkType = return $ columnTypeCon pk
    columnTplNames <- mapM newName columnNames
    resultTypeSignature <- [t|
        Connection -> $(pkType) -> IO (Maybe $(return $ ConT capTabName))
        |]
    connArg <- newName "connArg"
    keyArg <- newName "keyArg"
    resultName <- newName "resultName"
    query <- [| toByteString $ mconcat
        [ $( return $ LitE $ StringL $ mconcat
            [ "select ", mconcat $ intersperse ", " columnNames
            , " from ", snake tableName, " where ", acn pk, " = "
            ]
          )
        , dbStore $( return $ VarE keyArg )
        , ";"
        ]
        |]
    queryExp <- [| unsafeRawExec $(return $ VarE connArg) $(return query) >>=
        either (fail . show) return
        |]
    stmts <- forM (zip [0 :: Integer ..] columnTplNames) $ \ (i, fname) -> do
        unstoreExpression <- [| fmap (dbUnstore . fromMaybe undefined)
            (PQ.getvalue' $(return $ VarE resultName) 0 i) |]
        return (BindS (VarP fname) unstoreExpression)
    let
        resultExp = NoBindS $ AppE (VarE 'return) $ AppE (ConE 'Just) $
            foldr (\ x y -> AppE y (VarE x)) (ConE capTabName) columnTplNames
        doExp = DoE $ BindS (VarP resultName) queryExp : stmts ++ [resultExp]
    return
        [ SigD funcName resultTypeSignature
        , FunD funcName
            [ Clause [VarP connArg, VarP keyArg] (NormalB doExp) []
            ]
        ]
  where
    funcName = mkName funcStr
    capTabName = mkName $ cap tableName

getTable :: String -> Q Table
getTable tableName = do
    currentTableMap <- getCurrentTableMap
    return $ fromMaybe (error noTable) $ lookup tableName currentTableMap
  where
    noTable = "Can't find the table: " ++ tableName
