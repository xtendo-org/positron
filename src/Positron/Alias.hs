module Positron.Alias
    ( smallint
    , integer
    , bigint
    , decimal
    , numeric
    , real
    , double
    , serial
    , bigserial
    , varchar
    , foreignkey
    , text
    , insert
    , selectModel
    , getModel
    , parameter
    ) where

import Positron.Types

smallint :: ColumnType
smallint = Psmallint
integer :: ColumnType
integer = Pinteger
bigint :: ColumnType
bigint = Pbigint
decimal :: ColumnType
decimal = Pdecimal
numeric :: ColumnType
numeric = Pnumeric
real :: ColumnType
real = Preal
double :: ColumnType
double = Pdouble
serial :: ColumnType
serial = Pserial
bigserial :: ColumnType
bigserial = Pbigserial
varchar :: Integer -> ColumnType
varchar = Pvarchar
foreignkey :: String -> ColumnType
foreignkey = Pforeignkey
text :: ColumnType
text = Ptext

insert :: String -> Query
insert = Insert

selectModel :: String -> Query
selectModel name = Select (SelectModel name) []

getModel :: String -> Query
getModel = GetModel

parameter :: Parameter
parameter = Parameter
