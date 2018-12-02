{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.RAAusRegistrationType where

import GHC.Generics
import Prelude

data RAAusRegistrationType =
  RAAusRegistrationTypeProvisional
  | RAAusRegistrationTypeFull
  | RAAusRegistrationTypeSuspended
  | RAAusRegistrationTypeDeregistered
  deriving (Eq, Ord, Show, Generic)
