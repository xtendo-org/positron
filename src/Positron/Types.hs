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
    , columnProps :: [ColumnProp]
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
    -- | Pforeignkey String

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

data ColumnProp
    = Primary
    -- | ForeignKey String
    deriving (Eq, Show)

class Property v p | v -> p, p -> v where
    (./) :: v -> p -> v

instance Property Column ColumnProp where
    f ./ p = f { columnProps = p : columnProps f }

instance IsString (ColumnType -> Column) where
    fromString s = \t -> Column
        { columnName = s
        , columnType = t
        , columnProps = []
        }

