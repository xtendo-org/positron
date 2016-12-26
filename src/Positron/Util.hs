module Positron.Util
    ( decimal
    ) where

import Data.ByteString.Char8 as B

import Positron.Import

decimal :: Integral n => ByteString -> n
-- Warning: This function is intentionally partial and should not be exposed
-- outside the library.
decimal = B.foldl' step 0 . B.takeWhile isDigit
  where step a c = a * 10 + fromIntegral (ord c - 48)
