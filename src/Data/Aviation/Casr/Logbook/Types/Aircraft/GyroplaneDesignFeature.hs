{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data GyroplaneDesignFeature =
  -- gas turbine is covered by propulsion
  PressurisedGyroplaneDesignFeature
  | RetractableGearGyroplaneDesignFeature
  deriving (Eq, Ord, Show, Generic)

makeClassy ''GyroplaneDesignFeature
makeClassyPrisms ''GyroplaneDesignFeature
