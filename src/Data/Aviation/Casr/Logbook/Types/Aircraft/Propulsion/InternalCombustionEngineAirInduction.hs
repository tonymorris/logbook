{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction where

import Control.Lens(Lens', Prism', prism')
import GHC.Generics(Generic)
import Prelude

data InternalCombustionEngineAirInduction =
  Supercharged
  | Turbocharged
  | SuperTurbocharged
  | NaturalInduction
  deriving (Eq, Ord, Show, Generic)

class HasInternalCombustionEngineAirInduction a where
  internalCombustionEngineAirInduction ::
    Lens' a InternalCombustionEngineAirInduction

instance HasInternalCombustionEngineAirInduction InternalCombustionEngineAirInduction where
  internalCombustionEngineAirInduction =
    id

class AsInternalCombustionEngineAirInduction a where
  _InternalCombustionEngineAirInduction ::
    Prism' a InternalCombustionEngineAirInduction
  _Supercharged ::
    Prism' a ()
  _Turbocharged ::
    Prism' a ()
  _SuperTurbocharged ::
    Prism' a ()
  _NaturalInduction ::
    Prism' a ()

instance AsInternalCombustionEngineAirInduction InternalCombustionEngineAirInduction where
  _InternalCombustionEngineAirInduction =
    id
  _Supercharged =
    prism'
      (\() -> Supercharged)
      (
        \case
          Supercharged ->
            Just ()
          _ ->
            Nothing
      )
  _Turbocharged =
    prism'
      (\() -> Turbocharged)
      (
        \case
          Turbocharged ->
            Just ()
          _ ->
            Nothing
      )
  _SuperTurbocharged =
    prism'
      (\() -> SuperTurbocharged)
      (
        \case
          SuperTurbocharged ->
            Just ()
          _ ->
            Nothing
      )
  _NaturalInduction =
    prism'
      (\() -> NaturalInduction)
      (
        \case
          NaturalInduction ->
            Just ()
          _ ->
            Nothing
      )
