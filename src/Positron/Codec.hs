module Positron.Codec
    ( DBStorable(..)
    ) where

import Data.Text.Encoding as T

import Positron.Import
import Positron.Util

-- Convert the result from the database to a Haskell type.
class DBStorable c where
    dbUnstore :: ByteString -> c

instance DBStorable Int16 where
    dbUnstore = decimal
instance DBStorable Int32 where
    dbUnstore = decimal
instance DBStorable Int64 where
    dbUnstore = decimal
instance DBStorable Text where
    dbUnstore = T.decodeUtf8
