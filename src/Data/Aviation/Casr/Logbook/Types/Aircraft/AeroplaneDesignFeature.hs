{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeature where

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
