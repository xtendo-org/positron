{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}

module Positron.Types
    ( Positron(..)
    , Column(..)
    , ColumnProp(..)
    , ColumnType(..)
    , DBColumnType(..)
    , AnalyzedColumn(..)
    , columnTypeCon
    , (//)
    , PositronError(..)
    , Query(..)
    , Condition(..)
    , whose
    , DBC(..)
    ) where

import Positron.Import

class Positron p where
    pConn :: p -> Connection
    pLock :: p -> MVar ()
    pMake :: Connection -> IO p

data Column = Column
    { columnName :: String
    , columnType :: ColumnType
    , columnPrimary :: Bool
    , columnIndexed :: Bool
    , columnNullable :: Bool
    , columnUnique :: Bool
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
    | Unique
    deriving Show

class Property v p | v -> p, p -> v where
    (//) :: v -> p -> v

instance Property Column ColumnProp where
    c // p = case p of
        Primary -> c { columnPrimary = True }
        Indexed -> c { columnIndexed = True }
        Nullable -> c { columnNullable = True }
        Unique -> c { columnUnique = True }

instance IsString (ColumnType -> Column) where
    fromString s t = Column
        { columnName = s
        , columnType = t
        , columnPrimary = False
        , columnIndexed = False
        , columnNullable = False
        , columnUnique = False
        }

data AnalyzedColumn = AC
    { acn :: !String -- column name
    , acFullName :: !String -- column name prefixed with table name
    , acp :: !Bool -- primary key?
    , aci :: !Bool -- indexed?
    , acnl :: !Bool -- nullable?
    , acUnique :: !Bool -- unique?
    , act :: !DBColumnType
    , acf :: !(Maybe (String, String)) -- foreign key?
    } deriving Show

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

data PositronError
    = DuplicateKey
        { duplicateKey :: Text
        , duplicateKeyValue :: Text
        }
    | UnknownPositronError Text
    deriving Show

data Query
    = Insert String
    | Select
        { selectFields :: [String]
        , selectConditions :: [Condition]
        }
    deriving Show

-- This function may look like it has runtime errors, but it is in fact only
-- used in the Template Haskell stage. All errors are therefore compile-time.
whose :: Query -> [Condition] -> Query
whose q conds = case q of
    Insert name -> error (name ++ ": Insert cannot have conditions")
    s@(Select _ _) -> s { selectConditions = selectConditions s ++ conds }

data Condition
    = ParamEqual String
    | FixedEqual String DBC
    deriving Show

-- DBC: database capsule. Anything that can be stored in the database can be
-- stored in this data type.
data DBC
    = DBCInt16 Int16
    | DBCInt32 Int32
    | DBCInt64 Int64
    | DBCText Text
    deriving Show
