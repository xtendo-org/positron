module Positron.Unsafe where

-- base modules

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- local modules

import Positron.Types

type ColumnMap = [(String, ColumnType)]
type TableMap = [(String, ColumnMap)]

{-# NOINLINE tableMap #-}
tableMap :: IORef TableMap
tableMap = unsafePerformIO $ newIORef []

modify :: Eq k => k -> (v -> v) -> [(k, v)] -> [(k, v)]
modify k f kmap = case lookup k kmap of
    Just v -> (k, f v) : filter (\(k1, _) -> k1 /= k) kmap
    Nothing -> kmap

add :: Eq k => k -> v -> [(k, [v])] -> [(k, [v])]
add k v kmap = case lookup k kmap of
    Just vs -> (k, v : vs) : filter (\(k1, _) -> k1 /= k) kmap
    Nothing -> (k, [v]) : kmap

inspect :: ColumnType -> DBColumnType
inspect pct = case pct of
    Psmallint -> DBsmallint
    Pinteger -> DBinteger
    Pbigint -> DBbigint
    Pdecimal -> DBdecimal
    Pnumeric -> DBnumeric
    Preal -> DBreal
    Pdouble -> DBdouble
    Psmallserial -> DBsmallserial
    Pserial -> DBserial
    Pbigserial -> DBbigserial
    Pvarchar n -> DBvarchar n
