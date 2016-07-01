{-# language TemplateHaskell #-}

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

table :: String -> [Column] -> Q [Dec]
table tabName pcols = do
    cols <- mapM inspect pcols
    addTableMap tabName [(n, t) | (n, t, _) <- cols]
    let
        cqName = mkName $ "create" ++ capTbName
        cqSigDec = SigD cqName (ConT ''ByteString)
        recs = flip map cols $ \(n, t, _) ->
            (mkName $ tabName ++ cap n, Unpacked, columnTypeCon t)
        pks = map (\(n, _, _) -> n) $
            filter (\(_, _, p) -> Primary `elem` p) cols
        createQuery = concat
            [ "CREATE TABLE "
            , tabName
            , " (\n    "
            , concat
                [ n ++ " " ++ columnTypeStmt dt ++ ",\n    "
                | (n, dt, _) <- cols
                ]
            , "PRIMARY KEY ("
            , intercalate ", " pks
            , ")\n);\n"
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

inspect :: Column -> Q (String, DBColumnType, [ColumnProp])
inspect (Column n t p) = case t of
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
            (tabName, dottedColName) = break (== '.') s
            colName = tail dottedColName
        lookupTableMap tabName colName >>= \r -> case r of
            Nothing -> fail $ concat
                ["Column ", colName, " of Table ", tabName, " not found"]
            Just x -> return (n, x, ForeignKey tabName colName : p)
  where
    ret dt = return (n, dt, p)

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
