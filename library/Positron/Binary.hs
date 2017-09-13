module Positron.Binary where

import Positron.Import

import qualified Data.ByteString as B

class BigEndian b where
    decode :: ByteString -> Maybe b
instance BigEndian Int16 where
    decode = readN 2
instance BigEndian Int32 where
    decode = readN 4
instance BigEndian Int64 where
    decode = readN 8

readN :: Integral result => Int -> ByteString -> Maybe result
readN maxLength b
    | maxLength > B.length b = Nothing
    | otherwise = Just (bigEndian (B.take maxLength b))

bigEndian :: Integral i => ByteString -> i
bigEndian = bigEndian' 0
  where
    bigEndian' :: Integral i => i -> ByteString -> i
    bigEndian' acc b = case B.uncons b of
        Nothing -> acc
        Just (w, leftover) -> bigEndian' (acc * 256 + fromIntegral w) leftover
