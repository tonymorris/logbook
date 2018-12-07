{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear where

import Control.Lens
import GHC.Generics
import Prelude

data LandingGear =
  LandingGearFixedTricycle
  | LandingGearFixedTailWheel
  | LandingGearRetractableTricycle
  | LandingGearRetractableTailWheel
  deriving (Eq, Ord, Show, Generic)
 
class AsLandingGear a where
  _LandingGear ::
    Prism' a LandingGear
  _LandingGearFixedTricycle ::
    Prism' a ()
  _LandingGearFixedTailWheel ::
    Prism' a ()
  _LandingGearRetractableTricycle ::
    Prism' a ()
  _LandingGearRetractableTailWheel ::
    Prism' a ()
  
instance AsLandingGear LandingGear where
  _LandingGear =
    id
  _LandingGearFixedTricycle =
    prism'
      (\() -> LandingGearFixedTricycle)
      (\case
        LandingGearFixedTricycle ->
          Just ()
        _ ->
          Nothing)
  _LandingGearFixedTailWheel =
    prism'
      (\() -> LandingGearFixedTailWheel)
      (\case
        LandingGearFixedTailWheel ->
          Just ()
        _ ->
          Nothing)
  _LandingGearRetractableTricycle =
    prism'
      (\() -> LandingGearRetractableTricycle)
      (\case
        LandingGearRetractableTricycle ->
          Just ()
        _ ->
          Nothing)
  _LandingGearRetractableTailWheel =
    prism'
      (\() -> LandingGearRetractableTailWheel)
      (\case
        LandingGearRetractableTailWheel ->
          Just ()
        _ ->
          Nothing)
