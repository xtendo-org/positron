module Positron.Util
    ( decimal
    , toByteString
    , DBStorable(..)
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB (toStrict)

import Data.Text as T
import Data.Text.Encoding as T

import Positron.Import

decimal :: Integral n => ByteString -> n
-- Warning: This function is intentionally partial and should not be exposed
-- outside the library.
decimal = B.foldl' step 0 . B.takeWhile isDigit
  where step a c = a * 10 + fromIntegral (ord c - 48)

toByteString :: Builder -> ByteString
toByteString = LB.toStrict . B.toLazyByteString

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
