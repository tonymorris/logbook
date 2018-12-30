{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data AeroplaneDesignFeature =
  -- gas turbine and multi-engine centre-line is covered by Propulsion
  -- tail-wheel, retractable is covered by LangingGear
  ManualPropellorPitchControlAeroplaneDesignFeature
  | PressurisedAeroplaneDesignFeature
  | FloatplaneAeroplaneDesignFeature
  | FloatingHullAeroplaneDesignFeature
  | SkiGearAeroplaneDesignFeature
  deriving (Eq, Ord, Show, Generic)

makeClassy ''AeroplaneDesignFeature
makeClassyPrisms ''AeroplaneDesignFeature
