{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}

module Positron.Types
    ( Column(..)
    , ColumnProp(..)
    , ColumnType(..)
    , DBColumnType(..)
    , AnalyzedColumn(..)
    , columnTypeCon
    , (//)
    ) where

import Positron.Import

import Language.Haskell.TH

data Column = Column
    { columnName :: String
    , columnType :: ColumnType
    , columnPrimary :: Bool
    , columnIndexed :: Bool
    , columnNullable :: Bool
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
    | Ptext
    | Pforeignkey String
    deriving Show

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
    | DBtext
    | DBvarchar Integer

instance Show DBColumnType where
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
        DBvarchar len -> "varchar(" ++ show len ++ ")"
        DBtext -> "text"

data ColumnProp
    = Primary
    | Indexed
    | Nullable
    deriving Show

class Property v p | v -> p, p -> v where
    (//) :: v -> p -> v

instance Property Column ColumnProp where
    c // p = case p of
        Primary -> c { columnPrimary = True }
        Indexed -> c { columnIndexed = True }
        Nullable -> c { columnNullable = True }

instance IsString (ColumnType -> Column) where
    fromString s t = Column
        { columnName = s
        , columnType = t
        , columnPrimary = False
        , columnIndexed = False
        , columnNullable = False
        }

data AnalyzedColumn = AC
    { acn :: !String -- column name
    , acp :: !Bool -- primary key?
    , aci :: !Bool -- indexed?
    , acnl :: !Bool -- nullable?
    , act :: !DBColumnType
    , acf :: !(Maybe (String, String)) -- foreign key?
    }

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
