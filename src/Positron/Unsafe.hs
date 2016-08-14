module Positron.Unsafe
    ( TableMap
    , ColumnMap
    , addMap
    , lookupColumn
    , lookupTableMap
    , currentTableMap
    ) where

import Positron.Import

-- base modules

import System.IO.Unsafe (unsafePerformIO)

-- local modules

import Positron.Types

type ColumnMap = [(String, AnalyzedColumn)]
type TableMap = [(String, ColumnMap)]
type ModuleMap = [(String, TableMap)]

{-# NOINLINE moduleMap #-}
moduleMap :: IORef ModuleMap
moduleMap = unsafePerformIO $ newIORef []

addMap :: String -> (String, ColumnMap) -> Q ()
addMap modName tbMap = runIO $ modifyIORef moduleMap $
    add modName tbMap

lookupColumn :: String -> String -> String -> Q (Maybe AnalyzedColumn)
lookupColumn modName tabName colName = runIO $
    (lookup modName >=> lookup tabName >=> lookup colName) <$>
        readIORef moduleMap

lookupTableMap :: String -> Q (Maybe TableMap)
lookupTableMap modName = runIO $ lookup modName <$> readIORef moduleMap

currentTableMap :: Q (Maybe TableMap)
currentTableMap = (show <$> thisModule) >>= lookupTableMap

add :: Eq k => k -> v -> [(k, [v])] -> [(k, [v])]
add k v kmap = case lookup k kmap of
    Just vs -> (k, v : vs) : filter (\(i, _) -> i /= k) kmap
    Nothing -> (k, [v]) : kmap
