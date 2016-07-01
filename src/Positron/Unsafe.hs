module Positron.Unsafe
    ( TableMap
    , ColumnMap
    , addTableMap
    , lookupTableMap
    ) where

-- base modules

import Control.Monad
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- extra modules

import Language.Haskell.TH (Q, runIO)

-- local modules

import Positron.Types

type ColumnMap = [(String, DBColumnType)]
type TableMap = [(String, ColumnMap)]

{-# NOINLINE tableMap #-}
tableMap :: IORef TableMap
tableMap = unsafePerformIO $ newIORef []

addTableMap :: String -> ColumnMap -> Q ()
addTableMap key value = runIO $ modifyIORef tableMap (set key value)

lookupTableMap :: String -> String -> Q (Maybe DBColumnType)
lookupTableMap tabName colName = runIO $
    (lookup tabName >=> lookup colName) <$> readIORef tableMap

set :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
set k v kmap = (k, v) : filter (\(k1, _) -> k1 /= k) kmap
