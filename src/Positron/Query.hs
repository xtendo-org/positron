{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}

module Positron.Query
    ( queryInsert
    , queryUpsert
    ) where

import Positron.Import

-- data types

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- local modules

import Positron.Driver
import Positron.Types
import Positron.Unsafe

queryInsert :: String -> String -> Q [Dec]
queryInsert = queryUpsertBase False

queryUpsert :: String -> String -> Q [Dec]
queryUpsert = queryUpsertBase True

queryUpsertBase :: Bool -> String -> String -> Q [Dec]
queryUpsertBase upsert queryStr tableName = do
    maybeCurrentTableMap <- currentTableMap
    let columnMap = maybeCurrentTableMap >>= lookup tableName
    case columnMap of
        Nothing -> fail "Can't find the table map for the current module"
        Just tableMap -> let
            acols = map snd tableMap
            allPKNames = mconcat $ intersperse ", " $
                map (snake . acn) $ filter acp acols
            columnTypes = map columnTypeCon acols
            columnArgs = map (VarP . mkName . ("_" ++) . fst) tableMap
            columnNames = map (snake . fst) tableMap
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
                                (AppE
                                    (VarE 'unsafePlainExec)
                                    (VarE $ mkName "_conn")
                                )
                                mainContentExp
                            )
                            []
                        ]
                    ]
  where
    resultTypeSignature = AppT (ConT ''IO)
        (AppT
            (AppT
                (ConT ''Either)
                (ConT ''ByteString)
            )
            (TupleT 0)
        )
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

toByteString :: Builder -> ByteString
toByteString = LB.toStrict . B.toLazyByteString
