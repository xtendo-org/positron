module Positron.UUID
    ( UUID(..)
    , fromByteString
    , unsafeFromByteString
    , fromText
    ) where

import Positron.Import

-- external modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- local modules

import Positron.Util

data UUID = UUID
    { uuidByteString :: ByteString
    , uuidText :: Text
    }
    deriving Eq

instance Show UUID where
    show u = T.unpack $ "UUID <" <> uuidText u <> ">"

fromByteString :: ByteString -> Maybe UUID
fromByteString b
    | B.length b == 16 = Just $ UUID b text
    | otherwise = Nothing
  where
    text = T.decodeLatin1 $
        fold [hex1, "-", hex2, "-", hex3, "-", hex4, "-", hex5]
    hex1 = B.take 8 hexed
    hex2 = B.take 4 $ B.drop 8 hexed
    hex3 = B.take 4 $ B.drop 12 hexed
    hex4 = B.take 4 $ B.drop 16 hexed
    hex5 = B.drop 20 hexed
    hexed = toByteString $ B.byteStringHex b

unsafeFromByteString :: ByteString -> UUID
unsafeFromByteString = fromMaybe err .  fromByteString
  where
    err = error "UUID ByteString is not 16 bytes long"

fromText :: Text -> Maybe UUID
fromText t
    | T.length t /= 36 = Nothing
    | not (B.all isValidHexWord8 packed) = Nothing
    | otherwise = Just $ UUID (B.pack $ forPairs (B.unpack packed) unhex) t
  where
    rawBytes = T.encodeUtf8 t
    packed = fold [hex1, hex2, hex3, hex4, hex5]
    hex1 = B.take 8 rawBytes
    hex2 = B.take 4 $ B.drop (8 + 1) rawBytes
    hex3 = B.take 4 $ B.drop (8 + 1 + 4 + 1) rawBytes
    hex4 = B.take 4 $ B.drop (8 + 1 + 4 + 1 + 4 + 1) rawBytes
    hex5 = B.drop (8 + 1 + 4 + 1 + 4 + 1 + 4 + 1) rawBytes
    isValidHexWord8 w = (0x29 < w && w < 0x40) || (0x60 < w && w < 0x67)

unhex :: Word8 -> Word8 -> Word8
unhex upper lower = (trueValue upper `shiftL` 4) .|. trueValue lower
  where
    trueValue x
        | x < 0x40 = x - 0x30 -- x is a digit; subtract the ord of 0
        | otherwise = 0xa + x - 0x61 -- x is an alphabet; subtract the ord of a

forPairs :: [a] -> (a -> a -> b) -> [b]
forPairs [] _ = []
forPairs [_] _ = []
forPairs (x : y : xs) f = f x y : forPairs xs f
