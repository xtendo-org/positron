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

import Control.Monad
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
table tbName pcols = do
    cols <- forM pcols $ \(Column n t p) ->
        inspect t >>= \dt -> return (n, dt, p)
    runIO $ addTableMap tbName [(n, t) | (n, t, _) <- cols]
    let
        cqName = mkName $ "create" ++ capTbName
        cqSigDec = SigD cqName (ConT ''ByteString)
        recs = flip map cols $ \(n, t, _) ->
            (mkName $ tbName ++ cap n, Unpacked, columnTypeCon t)
        (colStmts, pks) = analyzeCols pcols
        createQuery = concat
            [ "CREATE TABLE "
            , tbName
            , " (\n    "
            , concatMap (++ ",\n    ") colStmts
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
    capTbName = cap tbName
    dataName = mkName capTbName
    cap [] = []
    cap (c : cs) = toUpper c : cs

analyzeCols :: [Column] -> ([String], [String])
analyzeCols [] = ([], [])
analyzeCols (Column n t p : cs) = if Primary `elem` p
    then (newColStmts, n : pks)
    else (newColStmts, pks)
  where
    newColStmts = concat [n, " ", show t] : colStmts
    (colStmts, pks) = analyzeCols cs

inspect :: ColumnType -> Q DBColumnType
inspect pct = case pct of
    Psmallint -> return DBsmallint
    Pinteger -> return DBinteger
    Pbigint -> return DBbigint
    Pdecimal -> return DBdecimal
    Pnumeric -> return DBnumeric
    Preal -> return DBreal
    Pdouble -> return DBdouble
    Psmallserial -> return DBsmallserial
    Pserial -> return DBserial
    Pbigserial -> return DBbigserial
    Pvarchar n -> return $ DBvarchar n

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
