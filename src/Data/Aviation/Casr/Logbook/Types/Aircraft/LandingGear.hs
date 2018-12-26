{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear where

import Control.Lens
import GHC.Generics
import Prelude

data LandingGear =
  LandingGearFixedTricycle
  | LandingGearFixedTailWheel
  | LandingGearRetractableTricycle
  | LandingGearRetractableTailWheel
  deriving (Eq, Ord, Show, Generic)
 
makeClassy ''LandingGear
makeClassyPrisms ''LandingGear
