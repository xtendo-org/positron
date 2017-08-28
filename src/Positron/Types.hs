module Positron.Types
    ( Positron(..)
    , Column(..)
    , ColumnType(..)
    , DBColumnType(..)
    , Property(..)
    , (//)
    , defaultColumn
    , AnalyzedColumn(..)
    , columnTypeCon
    , PositronError(..)
    , textShow
    , Query(..)
    , SelectTarget(..)
    , whose
    , orderBy
    , Parameter(..)
    , Condition(..)
    , (.==)
    , SetValue(..)
    , (?=)
    , OrderBy(..)
    ) where

import Positron.Import

import Positron.UUID

import Positron.Types.MissingMethods

class Positron p where
    pConn :: p -> Connection
    pLock :: p -> MVar ()
    pPrepareds :: p -> [(ByteString, ByteString)]
    -- NOTE: Later we might be able to make this faster by using a better
    -- container type than a list
    pCreateQueries :: p -> ByteString
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
    | Puuid
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
    | DBuuid

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
        DBuuid -> "uuid"

data Property
    = Primary
    | Indexed
    | Nullable
    | Unique
    deriving Show

(//) :: Column -> [Property] -> Column
(//)  = foldl' f
  where
    f c p = case p of
        Primary -> c { columnPrimary = True }
        Indexed -> c { columnIndexed = True }
        Nullable -> c { columnNullable = True }
        Unique -> c { columnUnique = True }

defaultColumn :: ColumnType -> String -> Column
defaultColumn t s = Column
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
    DBuuid -> ''UUID
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

textShow :: PositronError -> Text
textShow (DuplicateKey k v) = fold
    ["Value ", v, " already exists for ", k]
textShow (UnknownPositronError t) = t

data Query
    = Insert String
    | Upsert String
    | Select
        { selectTarget :: SelectTarget
        , selectConditions :: [Condition]
        , selectOrderBys :: [OrderBy]
        }
    | GetModel
        { getModelTarget :: String
        , getModelOrderBys :: [OrderBy]
        }
    | Update
        { updateTable :: String
        , updateColumns :: [SetValue]
        , updateConditions :: [Condition]
        }
    deriving Show

data SelectTarget
    = SelectModel String
    | SelectFields [String]
    deriving Show

-- These functions may look like they have runtime errors, but in fact only
-- used in the Template Haskell stage. All errors are therefore compile-time.
whose :: Query -> [Condition] -> Query
whose q conds = case q of
    Insert name -> error (name ++ ": Insert cannot have conditions")
    Upsert name -> error (name ++ ": Insert cannot have conditions")
    s@Select {} -> s { selectConditions = selectConditions s ++ conds }
    GetModel{..} -> error
        (getModelTarget ++ ": GetModel cannot have conditions")
    u@Update {} -> u { updateConditions = updateConditions u ++ conds }

orderBy :: Query -> [OrderBy] -> Query
orderBy q orderBys = case q of
    s@Select {} -> s { selectOrderBys = selectOrderBys s ++ orderBys }
    g@GetModel {} -> g { getModelOrderBys = getModelOrderBys g ++ orderBys }
    _ -> error "only select query can have \"order by\" clause"

data Condition = Condition String Parameter
    deriving Show

(.==) :: String -> Parameter -> Condition
(.==) = Condition

newtype SetValue = SetValue { unSetValue :: Condition }
    deriving Show

(?=) :: String -> Parameter -> SetValue
name ?= parameter = SetValue (Condition name parameter)

data OrderBy = Asc String | Desc String
    deriving Show
