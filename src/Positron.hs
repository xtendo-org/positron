{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}

module Positron
    ( table
    , Column
    , ColumnType
    , ColumnProp(..)
    , (//)
    , module Positron.Alias
    , mkCreateAll
    , queryInsert
    , queryUpsert

    -- re-export data types
    , Int16
    , Int32
    , Int64
    , Scientific
    , Float
    , Double
    , Word16
    , Word32
    , Word64
    , ByteString

    ) where

-- base modules

import Data.Char (isUpper, toUpper, toLower)
import Data.Int
import Data.List
import Data.Monoid
import Data.Scientific
import Data.Word

-- extra modules

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Text (Text)
import Data.Text.Encoding as T
import Language.Haskell.TH

-- local modules

import Positron.Alias
import Positron.Types
import Positron.Unsafe

mkCreateAll :: Q [Dec]
mkCreateAll = currentTableMap >>= \case
    Nothing -> return []
    Just tm -> [d|
        createAll :: ByteString
        createAll = mconcat $ reverse
            $(return $
                ListE
                    (map (VarE . mkName . ("create" ++) . cap . fst) tm)
            )
        |]

table :: String -> [Column] -> Q [Dec]
table tabName pcols = do
    cols <- mapM analyze pcols
    thisModuleStr <- show <$> thisModule
    addMap thisModuleStr (tabName, [(acn, a) | a@AC{..} <- cols])
    let
        cqName = mkName $ "create" ++ capTabName
        cqSigDec = SigD cqName (ConT ''ByteString)
        recs = for cols $ \ac@AC{..} ->
            (mkName $ tabName ++ cap acn, Unpacked, columnTypeCon ac)
        primaryKeys = map (lowerSnake . acn) $ filter acp cols
        foreignKeys = gatherFKs cols
        indexedKeys = map (lowerSnake . acn) $ filter aci cols
        createQuery = concat
            [ "CREATE TABLE IF NOT EXISTS "
            , snakeTabName
            , " (\n    "
            , concat $ for cols $ \AC{..} -> concat
                [ lowerSnake acn, " ", show act
                , if acnl then " NULL" else " NOT NULL"
                , ",\n    "
                ]
            , "PRIMARY KEY ("
            , intercalate ", " primaryKeys
            , ")"
            , if foreignKeys /= []
                then concatMap (",\n    " ++) foreignKeys
                else ""
            , "\n);\n"
            , if indexedKeys /= []
                then concat $ for indexedKeys $ \colName -> concat
                    [ "CREATE INDEX IF NOT EXISTS ix_"
                    , snakeTabName, "_", colName
                    , " ON ", snakeTabName, " (", colName, ");\n"
                    ]
                else ""
            ]
    cqExp <- [| B.pack $(return $ LitE $ StringL createQuery) |]
    let cqValDec = ValD (VarP cqName) (NormalB cqExp) []
    return
        [ DataD [] dataName [] [RecC dataName recs] []
        , cqSigDec
        , cqValDec
        ]
  where
    snakeTabName = lowerSnake tabName
    capTabName = cap tabName
    dataName = mkName capTabName
    gatherFKs [] = []
    gatherFKs (AC{..} : cs) = case acf of
        Just (tn, cn) -> fmtFK acn tn cn : gatherFKs cs
        Nothing -> gatherFKs cs

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
                map (lowerSnake . acn) $ filter acp acols
            columnTypes = map columnTypeCon acols
            columnArgs = map (VarP . mkName . ("_" ++) . fst) tableMap
            columnNames = map (lowerSnake . fst) tableMap
            in do
                mainContentExp <- [| mconcat
                    [ $(return $ LitE $ StringL $
                        "insert into " ++ lowerSnake tableName ++ " (")
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
                    [ SigD queryName $ foldr (\x y -> AppT (AppT ArrowT x) y)
                        (ConT ''ByteString)
                        columnTypes
                    , FunD queryName
                        [Clause columnArgs (NormalB mainContentExp) []]
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
                (if acnl then AppE (VarE 'fmap) quote else quote)
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
        quote = InfixE
            (Just (InfixE (Just (LitE (StringL "'"))) (VarE '(<>)) Nothing))
            (VarE '(.))
            (Just (InfixE Nothing (VarE '(<>)) (Just (LitE (StringL "'")))))
        defaultWrapVarE = defaultWrap . VarE
        defaultWrap f = wrap f (VarE $ mkName $ "_" ++ acn)
        wrap converter value = if acnl
            then AppE
                (AppE (AppE (VarE 'maybe) (LitE (StringL "null"))) converter)
                value
            else AppE converter value

toByteString :: Builder -> ByteString
toByteString = LB.toStrict . B.toLazyByteString

fmtFK :: String -> String -> String -> String
fmtFK n t c = concat
    [ "FOREIGN KEY(", lowerSnake n, ") REFERENCES "
    , lowerSnake t, " (", lowerSnake c, ")"
    ]

analyze :: Column -> Q AnalyzedColumn
analyze (Column n t pk idx nl) = case t of
    Psmallint -> ret DBsmallint
    Pinteger -> ret DBinteger
    Pbigint -> ret DBbigint
    Pdecimal -> ret DBdecimal
    Pnumeric -> ret DBnumeric
    Preal -> ret DBreal
    Pdouble -> ret DBdouble
    Psmallserial -> ret DBsmallserial
    Pserial -> ret DBserial
    Pbigserial -> ret DBbigserial
    Pvarchar len -> ret $ DBvarchar len
    Ptext -> ret DBtext
    Pforeignkey s -> do
        let
            (tn, dottedColName) = break (== '.') s
            cn = tail dottedColName
        thisModuleStr <- show <$> thisModule
        lookupColumn thisModuleStr tn cn >>= \case
            Nothing -> fail $ concat
                ["Column \"", cn, "\" of Table \"", tn, "\" not found"]
            Just AC{..} -> return $ acBase (plain act) (Just (tn, cn))
  where
    ret dt = return (acBase dt Nothing)
    acBase = AC n pk idx nl
    plain = \case
        DBsmallserial -> DBsmallint
        DBserial -> DBinteger
        DBbigserial -> DBbigint
        x -> x

columnTypeCon :: AnalyzedColumn -> Type
columnTypeCon AC{..} = constructor $ case act of
    DBsmallint -> ''Int16
    DBinteger -> ''Int32
    DBbigint -> ''Int64
    DBdecimal -> ''Scientific
    DBnumeric -> ''Scientific
    DBreal -> ''Float
    DBdouble -> ''Double
    DBsmallserial -> ''Int16
    DBserial -> ''Int32
    DBbigserial -> ''Int64
    DBvarchar _ -> ''Text
    DBtext -> ''Text
  where
    constructor = if acnl
        then AppT (ConT ''Maybe) . ConT
        else ConT

-- utility functions

for :: [a] -> (a -> b) -> [b]
for = flip map

lowerSnake :: String -> String
lowerSnake [] = []
lowerSnake (x : xs)
    | isUpper x = '_' : toLower x : lowerSnake xs
    | otherwise = x : lowerSnake xs

cap :: String -> String
cap [] = []
cap (c : cs) = toUpper c : cs
