module Positron.Unsafe
    ( TableMap
    , Table
    , addTable
    , lookupColumn
    , lookupTableMap
    , getCurrentTableMap
    , addPrepared
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

addTable :: String -> (String, Table) -> Q ()
addTable moduleName tablePair@(tableName, _) = runIO $
    modifyIORef moduleMap modifier
  where
    modifier :: ModuleMap -> ModuleMap
    modifier oldModuleMap = case lookup moduleName oldModuleMap of
        Just oldTableMap -> let
            newTableMap = tablePair :
                filter (\(i, _) -> i /= tableName) oldTableMap
          in (moduleName, newTableMap) :
            filter (\(i, _) -> i /= moduleName) oldModuleMap
        Nothing -> (moduleName, [tablePair]) : oldModuleMap

lookupColumn :: String -> String -> String -> Q (Maybe AnalyzedColumn)
lookupColumn moduleName tabName colName = runIO $
    (lookup moduleName >=> lookup tabName >=> lookup colName) <$>
        readIORef moduleMap

lookupTableMap :: String -> Q (Maybe TableMap)
lookupTableMap moduleName = runIO $ lookup moduleName <$> readIORef moduleMap

getCurrentTableMap :: Q TableMap
getCurrentTableMap = do
    name <- show <$> thisModule
    fromMaybe (error $ noModuleMsg ++ name) <$> lookupTableMap name
  where
    noModuleMsg = "Can't find the table map for the current module: "

{-# NOINLINE prepareds #-}
prepareds :: IORef [(ByteString, ByteString)]
prepareds = unsafePerformIO $ newIORef []

addPrepared :: (ByteString, ByteString) -> Q ()
addPrepared pair = runIO $ modifyIORef prepareds (pair :)
