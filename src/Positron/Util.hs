module Positron.Util
    ( toByteString
    , DBStorable(..)
    , BinaryStorable(..)
    , for
    ) where

import Positron.Import

-- data types

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Text as T
import Data.Text.Encoding as T

-- local modules

import Positron.Binary

decimal :: Integral n => ByteString -> n
-- Warning: This function is intentionally partial and should not be exposed
-- outside the library.
decimal = B.foldl' step 0 . B.takeWhile isDigit
  where step a c = a * 10 + fromIntegral (ord c - 48)

toByteString :: Builder -> ByteString
toByteString = LB.toStrict . B.toLazyByteString

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

-- Convert the result from the database to a Haskell type.
class DBStorable c where
    dbStore :: c -> Builder
    dbUnstore :: ByteString -> c

instance DBStorable Int16 where
    dbStore = B.int16Dec
    dbUnstore = decimal
instance DBStorable Int32 where
    dbStore = B.int32Dec
    dbUnstore = decimal
instance DBStorable Int64 where
    dbStore = B.int64Dec
    dbUnstore = decimal
instance DBStorable Text where
    dbStore = T.encodeUtf8Builder . sanitize
      where
        sanitize = quote . escape
        quote = ("'" <>) . (<> "'")
        escape = T.replace "'" "''"
    dbUnstore = T.decodeUtf8

-- Encode a Haskell value to a data in PostgreSQL's binary format.
class BinaryStorable c where
    binaryStore :: c -> ByteString
    binaryUnstore :: ByteString -> c

instance BinaryStorable Int16 where
    binaryStore = toByteString . B.int16BE
    binaryUnstore = unsafeDecode
instance BinaryStorable Int32 where
    binaryStore = toByteString . B.int32BE
    binaryUnstore = unsafeDecode
instance BinaryStorable Int64 where
    binaryStore = toByteString . B.int64BE
    binaryUnstore = unsafeDecode
instance BinaryStorable Text where
    binaryStore = T.encodeUtf8
    binaryUnstore = T.decodeUtf8

unsafeDecode :: BigEndian t => ByteString -> t
unsafeDecode = fromMaybe (error msg) . decode
  where
    msg = "ByteString decoding fail due to insufficient length " <>
        "(perhaps type mismatch?)"
