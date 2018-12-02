{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigits4(
  DecDigits4(..)
, module Digit
) where

import Data.Digit as Digit
import GHC.Generics
import Prelude

data DecDigits4 = 
  DecDigits4
    DecDigit
    DecDigit
    DecDigit
    DecDigit
  deriving (Eq, Ord, Show, Generic)
