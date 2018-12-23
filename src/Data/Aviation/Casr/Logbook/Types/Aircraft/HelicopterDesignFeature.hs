{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data HelicopterDesignFeature =
  -- gas turbine is covered by propulsion
  FloatAlightingGearHelicopterDesignFeature
  | RetractableGearHelicopterDesignFeature
  deriving (Eq, Ord, Show, Generic)

class ManyHelicopterDesignFeature a where
  _HelicopterDesignFeature_ ::
    Traversal' a HelicopterDesignFeature

instance ManyHelicopterDesignFeature HelicopterDesignFeature where
  _HelicopterDesignFeature_ =
    id
    
class ManyHelicopterDesignFeature a => HasHelicopterDesignFeature a where
  helicopterDesignFeature ::
    Lens' a HelicopterDesignFeature

instance HasHelicopterDesignFeature HelicopterDesignFeature where
  helicopterDesignFeature =
    id

class ManyHelicopterDesignFeature a => AsHelicopterDesignFeature a where
  _HelicopterDesignFeature ::
    Prism' a HelicopterDesignFeature
  _FloatAlightingGearHelicopterDesignFeature ::
    Prism' a ()
  _RetractableGearHelicopterDesignFeature ::
    Prism' a ()
  
instance AsHelicopterDesignFeature HelicopterDesignFeature where
  _HelicopterDesignFeature =
    id
  _FloatAlightingGearHelicopterDesignFeature =
    prism'
      (\() -> FloatAlightingGearHelicopterDesignFeature)
      (\case
        FloatAlightingGearHelicopterDesignFeature ->
          Just ()
        _ ->
          Nothing)
  _RetractableGearHelicopterDesignFeature =
    prism'
      (\() -> RetractableGearHelicopterDesignFeature)
      (\case
        RetractableGearHelicopterDesignFeature ->
          Just ()
        _ ->
          Nothing)
