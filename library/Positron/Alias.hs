module Positron.Alias
    ( bool
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
    , foreignkey
    , text
    , uuid
    , insert
    , upsert
    , selectModel
    , getModel
    , update
    , parameter
    ) where

import Positron.Types

bool :: String -> Column
bool = defaultColumn Pbool
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
uuid :: String -> Column
uuid = defaultColumn Puuid

insert :: String -> Query
insert n = Insert n Nothing []

upsert :: String -> Query
upsert = Upsert

selectModel :: String -> Query
selectModel name = Select (SelectModel name) [] []

getModel :: String -> Query
getModel name = GetModel name []

update :: String -> [SetValue] -> Query
update name setValues = Update name setValues []

parameter :: Parameter
parameter = Parameter
