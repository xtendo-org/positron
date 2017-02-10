{-# language FlexibleInstances #-}

module Positron.Glue
    ( g
    , printG
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

data GlueContainer
    = GlueText Text
    | GlueByteString ByteString
    | GlueString String

class Glue g where
    toGlue :: g -> GlueContainer
    fromGlue :: GlueContainer -> g
    printG :: g -> IO ()
instance Glue Text where
    toGlue = GlueText
    fromGlue (GlueText t) = t
    fromGlue (GlueByteString b) = T.decodeUtf8 b
    fromGlue (GlueString s) = T.pack s
    printG = T.putStrLn
instance Glue ByteString where
    toGlue = GlueByteString
    fromGlue (GlueText t) = T.encodeUtf8 t
    fromGlue (GlueByteString b) = b
    fromGlue (GlueString s) = T.encodeUtf8 $ T.pack s
    printG = B.putStrLn
instance Glue String where
    toGlue = GlueString
    fromGlue (GlueText t) = T.unpack t
    fromGlue (GlueByteString b) = T.unpack $ T.decodeUtf8 b
    fromGlue (GlueString s) = s
    printG = putStrLn

g :: (Glue g1, Glue g2) => g1 -> g2
g = fromGlue . toGlue
