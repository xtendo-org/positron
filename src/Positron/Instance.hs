{-# OPTIONS_GHC -fno-warn-orphans #-}

module Positron.Instance where

import Language.Haskell.TH.Syntax
import Data.ByteString
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

instance Lift ByteString where
    lift b = [| T.encodeUtf8 (T.pack $(lift $ T.unpack $ T.decodeUtf8 b)) |]
