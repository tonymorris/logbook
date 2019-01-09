{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineConfiguration where

import Control.Lens(Lens', Prism', prism')
import GHC.Generics(Generic)
import Prelude

data PistonEngineConfiguration =
  Parallel
  | VConfiguration
  | WConfiguration
  | XConfiguration
  | Opposed
  | Radial
  deriving (Eq, Ord, Show, Generic)

class HasPistonEngineConfiguration a where
  pistonEngineConfiguration ::
    Lens' a PistonEngineConfiguration

instance HasPistonEngineConfiguration PistonEngineConfiguration where
  pistonEngineConfiguration =
    id

class AsPistonEngineConfiguration a where
  _PistonEngineConfiguration ::
    Prism' a PistonEngineConfiguration
  _Parallel ::
    Prism' a ()
  _VConfiguration ::
    Prism' a ()
  _WConfiguration ::
    Prism' a ()
  _XConfiguration ::
    Prism' a ()
  _Opposed ::
    Prism' a ()
  _Radial ::
    Prism' a ()

instance AsPistonEngineConfiguration PistonEngineConfiguration where
  _PistonEngineConfiguration =
    id
  _Parallel =
    prism'
      (\() -> Parallel)
      (
        \case
          Parallel ->
            Just ()
          _ ->
            Nothing
      )
  _VConfiguration =
    prism'
      (\() -> VConfiguration)
      (
        \case
          VConfiguration ->
            Just ()
          _ ->
            Nothing
      )
  _WConfiguration =
    prism'
      (\() -> WConfiguration)
      (
        \case
          WConfiguration ->
            Just ()
          _ ->
            Nothing
      )
  _XConfiguration =
    prism'
      (\() -> XConfiguration)
      (
        \case
          XConfiguration ->
            Just ()
          _ ->
            Nothing
      )
  _Opposed =
    prism'
      (\() -> Opposed)
      (
        \case
          Opposed ->
            Just ()
          _ ->
            Nothing
      )
  _Radial =
    prism'
      (\() -> Radial)
      (
        \case
          Radial ->
            Just ()
          _ ->
            Nothing
      )
  