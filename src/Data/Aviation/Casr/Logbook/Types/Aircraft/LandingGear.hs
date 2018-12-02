{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear where

import GHC.Generics
import Prelude

data LandingGear =
  LandingGearFixedTricycle
  | LandingGearFixedTailWheel
  | LandingGearRetractableTricycle
  | LandingGearRetractableTailWheel
  deriving (Eq, Ord, Show, Generic)
 