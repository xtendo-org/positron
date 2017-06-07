module Positron.Util
    ( toByteString
    , for
    ) where

import Positron.Import

-- data types

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB (toStrict)

toByteString :: Builder -> ByteString
toByteString = LB.toStrict . B.toLazyByteString

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap
