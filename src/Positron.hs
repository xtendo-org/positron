{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}

module Positron
    ( table
    , Column
    , ColumnType
    , ColumnProp(..)
    , (//)
    , module Positron.Alias
    , mkCreateAll

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
import Data.Scientific
import Data.Word

-- extra modules

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Language.Haskell.TH

-- local modules

import Positron.Alias
import Positron.Types
import Positron.Unsafe

data AnalyzedColumn = AC
    { acn :: !String -- column name
    , acp :: !Bool -- primary key?
    , aci :: !Bool -- indexed?
    , acnl :: !Bool -- nullable?
    , act :: !DBColumnType
    , acf :: !(Maybe (String, String)) -- foreign key?
    }

mkCreateAll :: Q [Dec]
mkCreateAll = do
    thisModuleStr <- show <$> thisModule
    lookupTableMap thisModuleStr >>= \mt -> case mt of
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
    addMap thisModuleStr (tabName, [(acn, act) | AC{..} <- cols])
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
    cqExp <- [| pack $(return $ LitE $ StringL createQuery) |]
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
        lookupColumn thisModuleStr tn cn >>= \r -> case r of
            Nothing -> fail $ concat
                ["Column \"", cn, "\" of Table \"", tn, "\" not found"]
            Just dt -> return $ acBase (plain dt) (Just (tn, cn))
  where
    ret dt = return (acBase dt Nothing)
    acBase = AC n pk idx nl
    plain dt = case dt of
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
    DBvarchar _ -> ''ByteString
    DBtext -> ''ByteString
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
