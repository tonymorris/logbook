{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeature where

import GHC.Generics
import Prelude

data HelicopterDesignFeature =
  -- gas turbine is covered by propulsion
  FloatAlightingGearHelicopterDesignFeature
  | RetractableGearHelicopterDesignFeature
  deriving (Eq, Ord, Show, Generic)
