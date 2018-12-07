{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data GyroplaneDesignFeature =
  -- gas turbine is covered by propulsion
  PressurisedGyroplaneDesignFeature
  | RetractableGearGyroplaneDesignFeature
  deriving (Eq, Ord, Show, Generic)

class AsGyroplaneDesignFeature a where
  _GyroplaneDesignFeature ::
    Prism' a GyroplaneDesignFeature
  _PressurisedGyroplaneDesignFeature ::
    Prism' a ()
  _RetractableGearGyroplaneDesignFeature ::
    Prism' a ()
  
instance AsGyroplaneDesignFeature GyroplaneDesignFeature where
  _GyroplaneDesignFeature =
    id
  _PressurisedGyroplaneDesignFeature =
    prism'
      (\() -> PressurisedGyroplaneDesignFeature)
      (\case
        PressurisedGyroplaneDesignFeature ->
          Just ()
        _ ->
          Nothing)
  _RetractableGearGyroplaneDesignFeature =
    prism'
      (\() -> RetractableGearGyroplaneDesignFeature)
      (\case
        RetractableGearGyroplaneDesignFeature ->
          Just ()
        _ ->
          Nothing)
