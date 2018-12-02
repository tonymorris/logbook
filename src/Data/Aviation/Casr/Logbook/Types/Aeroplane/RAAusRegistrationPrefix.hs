{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.RAAusRegistrationPrefix where

import GHC.Generics
import Prelude

data RAAusRegistrationPrefix =
  RAAusRegistrationPrefix10
  | RAAusRegistrationPrefix19
  | RAAusRegistrationPrefix24
  | RAAusRegistrationPrefix28
  | RAAusRegistrationPrefix32
  | RAAusRegistrationPrefix55
  deriving (Eq, Ord, Show, Generic)
