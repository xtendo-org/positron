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
    \ rawTable -> let
        table = filter (not . isSerial . snd) rawTable
        acols = map snd table
        allPKNames :: String
        allPKNames = mconcat $ intersperse ", " $
            map (snake . acn) $ filter acp acols
        columnTypes = map columnTypeCon acols
        columnArgs = map (VarP . mkName . ("_" ++) . fst) table
        columnNames = map (snake . fst) table
        insertHeadPart = LitE $ StringL $ mconcat
            [ "insert into ", snake tableName, " ("
            , mconcat $ intersperse ", " columnNames, ") values ("
            ]
        upsertClause = LitE $ StringL $ if upsert then mconcat
            [ " ON CONFLICT ("
            , allPKNames
            , ") DO UPDATE SET "
            , mconcat $ intersperse ", " $ flip map columnNames $
                \colName -> mconcat [colName, " = EXCLUDED.", colName]
            , ";"
            ]
            else ";"
      in do
        mainContentExp <- [| mconcat
            [ $(return insertHeadPart)
            , toByteString $ mconcat $ intersperse ", "
                $(return $ ListE $ map expMake acols)
            , ")"
            , $(return upsertClause)
            ]
            |]
        resultTypeSignature <- [t| IO (Either ByteString ()) |]
        return
            [ SigD queryName $
                AppT (AppT ArrowT (ConT ''Positron)) $
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
    isSerial AC{..} = case act of
        DBsmallserial -> True
        DBserial -> True
        DBbigserial -> True
        _ -> False

queryGet :: String -> String -> Q [Dec]
queryGet funcStr tableName = getTable tableName >>= \ columnMap -> do
    let
        columnNames = map (snake . fst) columnMap
        acols = map snd columnMap
        -- FIXME: support composite key (multiple primary keys)
        pk = head $ filter acp acols
        pkType = return $ columnTypeCon pk
    resultTypeSignature <- [t|
        Positron -> $(pkType) -> IO (Maybe $(return $ ConT capTabName))
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
    bindPairs <- forM (zip [0 :: Integer ..] acols) $ \ (i, AC{..}) -> do
        fname <- newName acn
        unstoreExp <- if acnl
            then [| fmap dbUnstore |]
            else [| dbUnstore . fromMaybe (error "NOT NULL field is NULL") |]
        getvalueExp <- [| fmap $(return unstoreExp)
            (PQ.getvalue' $(return $ VarE resultName) 0 i) |]
        return (fname, getvalueExp)
    resultExp <- do
        prefix <- [| return . Just |]
        return $ NoBindS $ AppE prefix $
            foldl' (\ x y -> AppE x (VarE y))
                (ConE capTabName) (map fst bindPairs)
    doExp <- [| do
        $(return (VarP resultName)) <- unsafeRawExec
            $(return $ VarE connArg) $(return query) >>=
                either (fail . show) return
        ntuples <- fmap (> 0) (PQ.ntuples $(return $ VarE resultName))
        if ntuples
            then $(return $ DoE $
                [BindS (VarP x) y | (x, y) <- bindPairs] ++ [resultExp])
            else return Nothing
        |]
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
