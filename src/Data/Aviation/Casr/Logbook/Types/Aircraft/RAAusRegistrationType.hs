{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType where

import Control.Lens
import GHC.Generics
import Prelude

data RAAusRegistrationType =
  RAAusRegistrationTypeProvisional
  | RAAusRegistrationTypeFull
  | RAAusRegistrationTypeSuspended
  | RAAusRegistrationTypeDeregistered
  deriving (Eq, Ord, Show, Generic)

makeClassy ''RAAusRegistrationType
makeClassyPrisms ''RAAusRegistrationType