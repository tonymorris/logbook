{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.CASARegistration where

import Data.Char.Alpha
import GHC.Generics
import Prelude

data CASARegistration =
  CASARegistration
    Upper
    Upper
    Upper
  deriving (Eq, Ord, Show, Generic)
