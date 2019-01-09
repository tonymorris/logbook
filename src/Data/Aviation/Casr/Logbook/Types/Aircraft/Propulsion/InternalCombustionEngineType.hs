{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType where

import Control.Lens(Lens', Prism', prism')
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine
import GHC.Generics(Generic)
import Prelude

data InternalCombustionEngineType =
  PistonEngineType PistonEngine
  | RotaryEngineType RotaryEngine
  deriving (Eq, Ord, Show, Generic)

class HasInternalCombustionEngineType a where
  internalCombustionEngineType ::
    Lens' a InternalCombustionEngineType

instance HasInternalCombustionEngineType InternalCombustionEngineType where
  internalCombustionEngineType =
    id
 
class AsInternalCombustionEngineType a where
  _InternalCombustionEngineType ::
    Prism' a InternalCombustionEngineType
  _PistonEngineType ::
    Prism' a PistonEngine
  _RotaryEngineType ::
    Prism' a RotaryEngine

instance AsInternalCombustionEngineType InternalCombustionEngineType where
  _InternalCombustionEngineType =
    id
  _PistonEngineType =
    prism'
      PistonEngineType
      (
        \case
          PistonEngineType t ->
            Just t
          _ ->
            Nothing
      )
  _RotaryEngineType =
    prism'
      RotaryEngineType
      (
        \case
          RotaryEngineType t ->
            Just t
          _ ->
            Nothing
      )

----

instance AsPistonEngine InternalCombustionEngineType where
  _PistonEngine =
    prism'
      PistonEngineType
      (
        \case
          PistonEngineType t ->
            Just t
          _ ->
            Nothing
      )

instance AsRotaryEngine InternalCombustionEngineType where
  _RotaryEngine =
    prism'
      RotaryEngineType
      (
        \case
          RotaryEngineType t ->
            Just t
          _ ->
            Nothing
      )
