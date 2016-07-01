{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}

module Positron.Types
    ( Column(..)
    , ColumnProp(..)
    , ColumnType(..)
    , DBColumnType(..)
    , (./)
    ) where

import Data.String

data Column = Column
    { columnName :: String
    , columnType :: ColumnType
    , columnPrimary :: Bool
    } deriving Show

data ColumnType
    = Psmallint
    | Pinteger
    | Pbigint
    | Pdecimal
    | Pnumeric
    | Preal
    | Pdouble
    | Psmallserial
    | Pserial
    | Pbigserial
    | Pvarchar Integer
    | Pforeignkey String

data DBColumnType
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
        Psmallint -> "smallint"
        Pinteger -> "integer"
        Pbigint -> "bigint"
        Pdecimal -> "decimal"
        Pnumeric -> "numeric"
        Preal -> "real"
        Pdouble -> "double"
        Psmallserial -> "smallserial"
        Pserial -> "serial"
        Pbigserial -> "bigserial"
        Pvarchar n -> "varchar(" ++ show n ++ ")"
        Pforeignkey s -> "foreignkey (" ++ s ++ ")"

data ColumnProp
    = Primary
    deriving Show

class Property v p | v -> p, p -> v where
    (./) :: v -> p -> v

instance Property Column ColumnProp where
    c ./ p = case p of
        Primary -> c { columnPrimary = True }

instance IsString (ColumnType -> Column) where
    fromString s t = Column
        { columnName = s
        , columnType = t
        , columnPrimary = False
        }
