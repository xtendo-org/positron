{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}

module Positron
    ( table
    , Column
    , ColumnType
    , ColumnProp(..)
    , smallint
    , integer
    , bigint
    , decimal
    , numeric
    , real
    , double
    , serial
    , bigserial
    , varchar
    , (./)

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
import Data.String
import Data.Word

-- extra modules

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))

data Column = Column
    { columnName :: String
    , columnType :: ColumnType
    , columnProps :: [ColumnProp]
    } deriving Show

instance Lift Column where
    lift (Column n t p) = [| $(return $ ConE 'Column) n t p |]

data ColumnType
    = DBsmallint
    | DBinteger
    | DBbigint
    | DBdecimal
    | DBnumeric
    | DBreal
    | DBdouble
    | DBsmallserial
    | DBserial
    | DBbigserial
    | DBvarchar Integer

instance Show ColumnType where
    show t = case t of
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
        DBvarchar n -> "varchar(" ++ show n ++ ")"

instance Lift ColumnType where
    lift t = case t of
        DBsmallint -> ret 'DBsmallint
        DBinteger -> ret 'DBinteger
        DBbigint -> ret 'DBbigint
        DBdecimal -> ret 'DBdecimal
        DBnumeric -> ret 'DBnumeric
        DBreal -> ret 'DBreal
        DBdouble -> ret 'DBdouble
        DBsmallserial -> ret 'DBsmallserial
        DBserial -> ret 'DBserial
        DBbigserial -> ret 'DBbigserial
        DBvarchar n -> [| $(ret 'DBvarchar) (n :: Integer) |]
      where
        ret = return . ConE

columnTypeCon :: ColumnType -> Type
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

smallint :: ColumnType
smallint = DBsmallint
integer :: ColumnType
integer = DBinteger
bigint :: ColumnType
bigint = DBbigint
decimal :: ColumnType
decimal = DBdecimal
numeric :: ColumnType
numeric = DBnumeric
real :: ColumnType
real = DBreal
double :: ColumnType
double = DBdouble
serial :: ColumnType
serial = DBserial
bigserial :: ColumnType
bigserial = DBbigserial
varchar :: Integer -> ColumnType
varchar = DBvarchar

instance IsString (ColumnType -> Column) where
    fromString s = \t -> Column
        { columnName = s
        , columnType = t
        , columnProps = []
        }

data ColumnProp
    = PrimaryKey
    deriving (Eq, Show)

instance Lift ColumnProp where
    lift p = return $ ConE $ case p of
        PrimaryKey -> 'PrimaryKey

class Property v p | v -> p, p -> v where
    (./) :: v -> p -> v

instance Property Column ColumnProp where
    f ./ p = f { columnProps = p : columnProps f }

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
    col (Column n t _) = (mkName $ tbName ++ cap n, Unpacked, columnTypeCon t)
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
analyzeCols (Column n t p : cs) = if PrimaryKey `elem` p
    then (newColStmts, n : pks)
    else (newColStmts, pks)
  where
    newColStmts = concat [n, " ", show t] : colStmts
    (colStmts, pks) = analyzeCols cs
