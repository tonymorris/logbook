{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data HelicopterDesignFeature =
  -- gas turbine is covered by propulsion
  FloatAlightingGearHelicopterDesignFeature
  | RetractableGearHelicopterDesignFeature
  deriving (Eq, Ord, Show, Generic)

makeClassy ''HelicopterDesignFeature
makeClassyPrisms ''HelicopterDesignFeature
