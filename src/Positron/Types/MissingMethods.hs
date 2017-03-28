{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Positron.Types.MissingMethods where

import Positron.Import

data Parameter
    = Parameter
    | FixedString String
    | FixedNum Integer
    deriving Show

instance IsString Parameter where
    fromString = FixedString
instance Num Parameter where
    fromInteger = FixedNum
