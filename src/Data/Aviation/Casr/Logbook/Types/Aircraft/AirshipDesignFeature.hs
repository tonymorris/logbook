{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data AirshipDesignFeature =
  -- gas turbine is covered by propulsion
  PressurisedAirshipDesignFeature
  deriving (Eq, Ord, Show, Generic)

class AsAirshipDesignFeature a where
  _AirshipDesignFeature ::
    Prism' a AirshipDesignFeature
  _PressurisedAirshipDesignFeature ::
    Prism' a ()
  
instance AsAirshipDesignFeature AirshipDesignFeature where
  _AirshipDesignFeature =
    id
  _PressurisedAirshipDesignFeature =
    prism'
      (\() -> PressurisedAirshipDesignFeature)
      (\case
        PressurisedAirshipDesignFeature ->
          Just ())
  