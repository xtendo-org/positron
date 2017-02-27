module Positron.Import
    ( module Module
    , ByteString
    , Builder
    , Text
    , Connection
    , snake
    , cap
    , decap
    ) where

import Control.Concurrent.MVar as Module
import Control.Monad as Module
import Data.Bits as Module
import Data.Char as Module
import Data.Foldable as Module
import Data.IORef as Module
import Data.Int as Module
import Data.List as Module
import Data.Maybe as Module
import Data.Monoid as Module
import Data.Scientific as Module
import Data.String as Module
import Data.Word as Module
import Language.Haskell.TH as Module
import Language.Haskell.TH.Syntax as Module

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Text (Text)
import Database.PostgreSQL.LibPQ (Connection)

import Positron.Instance as Module ()

snake :: String -> String
snake [] = []
snake (x : xs)
    | isUpper x = '_' : toLower x : snake xs
    | otherwise = x : snake xs

cap :: String -> String
cap [] = []
cap (c : cs) = toUpper c : cs

decap :: String -> String
decap [] = []
decap (c : cs) = toLower c : cs
