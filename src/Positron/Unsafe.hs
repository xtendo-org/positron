module Positron.Unsafe
    ( TableMap
    , ColumnMap
    , addMap
    , lookupColumn
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
type ModuleMap = [(String, TableMap)]

{-# NOINLINE moduleMap #-}
moduleMap :: IORef ModuleMap
moduleMap = unsafePerformIO $ newIORef []

addMap :: String -> (String, ColumnMap) -> Q ()
addMap modName tbMap = runIO $ modifyIORef moduleMap $
    add modName tbMap

lookupColumn :: String -> String -> String -> Q (Maybe DBColumnType)
lookupColumn modName tabName colName = runIO $
    (lookup modName >=> lookup tabName >=> lookup colName) <$>
        readIORef moduleMap

lookupTableMap :: String -> Q (Maybe TableMap)
lookupTableMap modName = runIO $ lookup modName <$> readIORef moduleMap

add :: Eq k => k -> v -> [(k, [v])] -> [(k, [v])]
add k v kmap = case lookup k kmap of
    Just vs -> (k, v : vs) : filter (\(i, _) -> i /= k) kmap
    Nothing -> (k, [v]) : kmap
