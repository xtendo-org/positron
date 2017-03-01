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
    , update
    , parameter
    ) where

import Positron.Types

smallint :: String -> Column
smallint = defaultColumn Psmallint
integer :: String -> Column
integer = defaultColumn Pinteger
bigint :: String -> Column
bigint = defaultColumn Pbigint
decimal :: String -> Column
decimal = defaultColumn Pdecimal
numeric :: String -> Column
numeric = defaultColumn Pnumeric
real :: String -> Column
real = defaultColumn Preal
double :: String -> Column
double = defaultColumn Pdouble
serial :: String -> Column
serial = defaultColumn Pserial
bigserial :: String -> Column
bigserial = defaultColumn Pbigserial
varchar :: Integer -> String -> Column
varchar n = defaultColumn (Pvarchar n)
foreignkey :: String -> String -> Column
foreignkey target = defaultColumn (Pforeignkey target)
text :: String -> Column
text = defaultColumn Ptext

insert :: String -> Query
insert = Insert

selectModel :: String -> Query
selectModel name = Select (SelectModel name) []

getModel :: String -> Query
getModel = GetModel

update :: String -> [SetValue] -> Query
update name setValues = Update name setValues []

parameter :: Parameter
parameter = Parameter
