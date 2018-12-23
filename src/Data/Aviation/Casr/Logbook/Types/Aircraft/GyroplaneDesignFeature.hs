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

class ManyGyroplaneDesignFeature a where
  _GyroplaneDesignFeature_ ::
    Traversal' a GyroplaneDesignFeature

instance ManyGyroplaneDesignFeature GyroplaneDesignFeature where
  _GyroplaneDesignFeature_ =
    id
    
class ManyGyroplaneDesignFeature a => HasGyroplaneDesignFeature a where
  gyroplaneDesignFeature ::
    Lens' a GyroplaneDesignFeature

instance HasGyroplaneDesignFeature GyroplaneDesignFeature where
  gyroplaneDesignFeature =
    id

class ManyGyroplaneDesignFeature a => AsGyroplaneDesignFeature a where
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
