{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.AirshipDesignFeature where

import GHC.Generics
import Prelude

data AirshipDesignFeature =
  -- gas turbine is covered by propulsion
  PressurisedAirshipDesignFeature
  deriving (Eq, Ord, Show, Generic)
