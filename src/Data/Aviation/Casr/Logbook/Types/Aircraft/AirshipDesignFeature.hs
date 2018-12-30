{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data AirshipDesignFeature =
  -- gas turbine is covered by propulsion
  PressurisedAirshipDesignFeature
  deriving (Eq, Ord, Show, Generic)

makeClassy ''AirshipDesignFeature

class AsAirshipDesignFeature a where
  _AirshipDesignFeature ::
    Prism' a AirshipDesignFeature

instance AsAirshipDesignFeature AirshipDesignFeature where
  _AirshipDesignFeature =
    id
