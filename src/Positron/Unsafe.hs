module Positron.Unsafe
    ( TableMap
    , Table
    , addMap
    , lookupColumn
    , lookupTableMap
    , getCurrentTableMap
    ) where

import Positron.Import

-- base modules

import System.IO.Unsafe (unsafePerformIO)

-- local modules

import Positron.Types

type Table = [(String, AnalyzedColumn)]
type TableMap = [(String, Table)]
type ModuleMap = [(String, TableMap)]

{-# NOINLINE moduleMap #-}
moduleMap :: IORef ModuleMap
moduleMap = unsafePerformIO $ newIORef []

addMap :: String -> (String, Table) -> Q ()
addMap modName tbMap = runIO $ modifyIORef moduleMap $
    add modName tbMap

lookupColumn :: String -> String -> String -> Q (Maybe AnalyzedColumn)
lookupColumn modName tabName colName = runIO $
    (lookup modName >=> lookup tabName >=> lookup colName) <$>
        readIORef moduleMap

lookupTableMap :: String -> Q (Maybe TableMap)
lookupTableMap modName = runIO $ lookup modName <$> readIORef moduleMap

getCurrentTableMap :: Q TableMap
getCurrentTableMap = do
    name <- show <$> thisModule
    fromMaybe (error $ noModuleMsg ++ name) <$> lookupTableMap name
  where
    noModuleMsg = "Can't find the table map for the current module: "

add :: Eq k => k -> v -> [(k, [v])] -> [(k, [v])]
add k v kmap = case lookup k kmap of
    Just vs -> (k, v : vs) : filter (\(i, _) -> i /= k) kmap
    Nothing -> (k, [v]) : kmap
