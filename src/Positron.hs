{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}

module Positron
    ( table
    , Column
    , ColumnType
    , ColumnProp(Primary)
    , (./)
    , module Positron.Alias

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

import Data.Char (toUpper)
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
    { acn :: {-# UNPACK #-} !String -- column name
    , act :: {-# UNPACK #-} !DBColumnType
    , acp :: {-# UNPACK #-} !Bool -- primary key?
    , acf :: {-# UNPACK #-} !(Maybe (String, String)) -- foreign key?
    }

table :: String -> [Column] -> Q [Dec]
table tabName pcols = do
    cols <- mapM analyze pcols
    addTableMap tabName [(acn, act) | AC{..} <- cols]
    let
        cqName = mkName $ "create" ++ capTbName
        cqSigDec = SigD cqName (ConT ''ByteString)
        recs = for cols $ \AC{..} ->
            (mkName $ tabName ++ cap acn, Unpacked, columnTypeCon act)
        primaryKeys = map acn $ filter acp cols
        foreignKeys = gatherFKs cols
        createQuery = concat
            [ "CREATE TABLE "
            , tabName
            , " (\n    "
            , concat $ for cols $ \AC{..} ->
                acn ++ " " ++ columnTypeStmt act ++ ",\n    "
            , "PRIMARY KEY ("
            , intercalate ", " primaryKeys
            , ")"
            , if foreignKeys /= []
                then concatMap (",\n    " ++) foreignKeys
                else ""
            , "\n);\n"
            ]
    cqExp <- [| pack $(return $ LitE $ StringL createQuery) |]
    let cqValDec = ValD (VarP cqName) (NormalB cqExp) []
    return
        [ DataD [] dataName [] [RecC dataName recs] []
        , cqSigDec
        , cqValDec
        ]
  where
    capTbName = cap tabName
    dataName = mkName capTbName
    cap [] = []
    cap (c : cs) = toUpper c : cs
    gatherFKs [] = []
    gatherFKs (AC{..} : cs) = case acf of
        Just (tn, cn) -> fmtFK acn tn cn : gatherFKs cs
        Nothing -> gatherFKs cs

fmtFK :: String -> String -> String -> String
fmtFK n t c = concat
    ["FOREIGN KEY(", n, ") REFERENCES ", t, " (", c, ")"]

analyze :: Column -> Q AnalyzedColumn
analyze (Column n t pk) = case t of
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
    Pforeignkey s -> do
        let
            (tn, dottedColName) = break (== '.') s
            cn = tail dottedColName
        lookupTableMap tn cn >>= \r -> case r of
            Nothing -> fail $ concat
                ["Column ", cn, " of Table ", tn, " not found"]
            Just dt -> return $ AC n dt pk (Just (tn, cn))
  where
    ret dt = return (AC n dt pk Nothing)

columnTypeCon :: DBColumnType -> Type
columnTypeCon t = ConT $ case t of
    DBsmallint -> ''Int16
    DBinteger -> ''Int32
    DBbigint -> ''Int64
    DBdecimal -> ''Scientific
    DBnumeric -> ''Scientific
    DBreal -> ''Float
    DBdouble -> ''Double
    DBsmallserial -> ''Word16
    DBserial -> ''Word32
    DBbigserial -> ''Word64
    DBvarchar _ -> ''ByteString

columnTypeStmt :: DBColumnType -> String
columnTypeStmt t = case t of
    DBsmallint -> "smallint"
    DBinteger -> "integer"
    DBbigint -> "bigint"
    DBdecimal -> "decimal"
    DBnumeric -> "numeric"
    DBreal -> "real"
    DBdouble -> "double"
    DBsmallserial -> "smallserial"
    DBserial -> "serial"
    DBbigserial -> "bigserial"
    DBvarchar len -> "varchar(" ++ show len ++ ")"

-- utility functions

for :: [a] -> (a -> b) -> [b]
for = flip map
