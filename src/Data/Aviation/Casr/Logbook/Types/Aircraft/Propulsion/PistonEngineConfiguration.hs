{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineConfiguration(
  PistonEngineConfiguration(..)
, HasPistonEngineConfiguration(..)
) where

import Control.Lens
import GHC.Generics
import Prelude

data PistonEngineConfiguration =
  Parallel
  | VConfiguration
  | WConfiguration
  | XConfiguration
  | Opposed
  | Radial
  deriving (Eq, Ord, Show, Generic)

makeClassy ''PistonEngineConfiguration
makeClassyPrisms ''PistonEngineConfiguration
