{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
makeClassyPrisms ''AirshipDesignFeature
