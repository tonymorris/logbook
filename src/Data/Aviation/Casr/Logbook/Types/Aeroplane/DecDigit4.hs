{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.DecDigit4 where

import Data.Digit
import GHC.Generics
import Prelude

data DecDigit4 = 
  DecDigit4
    DecDigit
    DecDigit
    DecDigit
    DecDigit
  deriving (Eq, Ord, Show, Generic)
