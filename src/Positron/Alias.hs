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
