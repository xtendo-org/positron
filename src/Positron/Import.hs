module Positron.Import
    ( module Module
    , ByteString
    , Builder
    , Text
    , snake
    ) where

import Control.Monad as Module
import Data.Char as Module
import Data.Int as Module
import Data.IORef as Module
import Data.List as Module
import Data.Maybe as Module
import Data.Monoid as Module
import Data.Scientific as Module
import Data.String as Module
import Data.Word as Module
import Language.Haskell.TH as Module

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Text (Text)

snake :: String -> String
snake [] = []
snake (x : xs)
    | isUpper x = '_' : toLower x : snake xs
    | otherwise = x : snake xs
