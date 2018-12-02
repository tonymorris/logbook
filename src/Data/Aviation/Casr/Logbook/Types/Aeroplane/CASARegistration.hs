{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.CASARegistration where

import Data.Char.Alpha
import GHC.Generics
import Prelude

data CASARegistration =
  CASARegistration
    Upper
    Upper
    Upper
  deriving (Eq, Ord, Show, Generic)
