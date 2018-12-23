{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data AeroplaneDesignFeature =
  -- gas turbine and multi-engine centre-line is covered by Propulsion
  -- tail-wheel, retractable is covered by LangingGear
  ManualPropellorPitchControlAeroplaneDesignFeature
  | PressurisedAeroplaneDesignFeature
  | FloatplaneAeroplaneDesignFeature
  | FloatingHullAeroplaneDesignFeature
  | SkiGearAeroplaneDesignFeature
  deriving (Eq, Ord, Show, Generic)

class ManyAeroplaneDesignFeature a where
  _AeroplaneDesignFeature_ ::
    Traversal' a AeroplaneDesignFeature

instance ManyAeroplaneDesignFeature AeroplaneDesignFeature where
  _AeroplaneDesignFeature_ =
    id

class ManyAeroplaneDesignFeature a => HasAeroplaneDesignFeature a where
  aeroplaneDesignFeature ::
    Lens' a AeroplaneDesignFeature

instance HasAeroplaneDesignFeature AeroplaneDesignFeature where
  aeroplaneDesignFeature =
    id

class ManyAeroplaneDesignFeature a => AsAeroplaneDesignFeature a where
  _AeroplaneDesignFeature ::
    Prism' a AeroplaneDesignFeature
  _ManualPropellorPitchControlAeroplaneDesignFeature ::
    Prism' a ()
  _PressurisedAeroplaneDesignFeature ::
    Prism' a ()
  _FloatplaneAeroplaneDesignFeature ::
    Prism' a ()
  _FloatingHullAeroplaneDesignFeature ::
    Prism' a ()
  _SkiGearAeroplaneDesignFeature ::
    Prism' a ()
  
instance AsAeroplaneDesignFeature AeroplaneDesignFeature where
  _AeroplaneDesignFeature =
    id
  _ManualPropellorPitchControlAeroplaneDesignFeature =
    prism'
      (\() -> ManualPropellorPitchControlAeroplaneDesignFeature)
      (\case
        ManualPropellorPitchControlAeroplaneDesignFeature ->
          Just ()
        _ ->
          Nothing)
  _PressurisedAeroplaneDesignFeature =
    prism'
      (\() -> PressurisedAeroplaneDesignFeature)
      (\case
        PressurisedAeroplaneDesignFeature ->
          Just ()
        _ ->
          Nothing)
  _FloatplaneAeroplaneDesignFeature =
    prism'
      (\() -> FloatplaneAeroplaneDesignFeature)
      (\case
        FloatplaneAeroplaneDesignFeature ->
          Just ()
        _ ->
          Nothing)
  _FloatingHullAeroplaneDesignFeature =
    prism'
      (\() -> FloatingHullAeroplaneDesignFeature)
      (\case
        FloatingHullAeroplaneDesignFeature ->
          Just ()
        _ ->
          Nothing)
  _SkiGearAeroplaneDesignFeature =
    prism'
      (\() -> SkiGearAeroplaneDesignFeature)
      (\case
        SkiGearAeroplaneDesignFeature ->
          Just ()
        _ ->
          Nothing)
