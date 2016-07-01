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

table :: String -> [Column] -> Q [Dec]
table tbName cols = do
    cqExp <- [| pack $(return $ LitE $ StringL createQuery) |]
    let
        cqName = mkName $ "create" ++ capTbName
        cqSigDec = SigD cqName (ConT ''ByteString)
        cqValDec = ValD (VarP cqName) (NormalB cqExp) []
    return
        [ DataD [] dataName [] [RecC dataName (map col cols)] []
        , cqSigDec
        , cqValDec
        ]
  where
    capTbName = cap tbName
    dataName = mkName $ capTbName
    col (Column n t _) =
        ( mkName $ tbName ++ cap n
        , Unpacked
        , columnTypeCon $ inspect t
        )
    cap [] = []
    cap (c : cs) = toUpper c : cs
    createQuery = concat
        [ "CREATE TABLE "
        , tbName
        , " (\n    "
        , concat $ map (++ ",\n    ") colStmts
        , "PRIMARY KEY ("
        , intercalate ", " $ pks
        , ")\n);\n"
        ]
    (colStmts, pks) = analyzeCols cols


analyzeCols :: [Column] -> ([String], [String])
analyzeCols [] = ([], [])
analyzeCols (Column n t p : cs) = if Primary `elem` p
    then (newColStmts, n : pks)
    else (newColStmts, pks)
  where
    newColStmts = concat [n, " ", show t] : colStmts
    (colStmts, pks) = analyzeCols cs
