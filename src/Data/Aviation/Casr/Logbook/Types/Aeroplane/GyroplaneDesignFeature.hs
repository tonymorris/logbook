{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.GyroplaneDesignFeature where

import GHC.Generics
import Prelude

data GyroplaneDesignFeature =
  -- gas turbine is covered by propulsion
  PressurisedGyroplaneDesignFeature
  | RetractableGearGyroplaneDesignFeature
  deriving (Eq, Ord, Show, Generic)
