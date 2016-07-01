module Positron.Unsafe
    ( TableMap
    , ColumnMap
    , addTableMap
    ) where

-- base modules

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- local modules

import Positron.Types

type ColumnMap = [(String, DBColumnType)]
type TableMap = [(String, ColumnMap)]

{-# NOINLINE tableMap #-}
tableMap :: IORef TableMap
tableMap = unsafePerformIO $ newIORef []

addTableMap :: String -> ColumnMap -> IO ()
addTableMap key value = modifyIORef tableMap (modify key (const value))

-- lookupTableMap :: String -> String -> IO (Maybe DBColumnType)

modify :: Eq k => k -> (v -> v) -> [(k, v)] -> [(k, v)]
modify k f kmap = case lookup k kmap of
    Just v -> (k, f v) : filter (\(k1, _) -> k1 /= k) kmap
    Nothing -> kmap

add :: Eq k => k -> v -> [(k, [v])] -> [(k, [v])]
add k v kmap = case lookup k kmap of
    Just vs -> (k, v : vs) : filter (\(k1, _) -> k1 /= k) kmap
    Nothing -> (k, [v]) : kmap
