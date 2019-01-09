{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineType where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.ElectricType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.JetType
import GHC.Generics
import Prelude

data EngineType =
  InternalCombustionEngineEngineType InternalCombustionEngine
  | Electric ElectricType
  | Jet JetType
  | Rocket
  deriving (Eq, Ord, Show, Generic)

class HasEngineType a where
  engineType ::
    Lens' a EngineType

instance HasEngineType EngineType where
  engineType =
    id

class AsEngineType a where
  _EngineType ::
    Prism' a EngineType
  _InternalCombustionEngineEngineType ::
    Prism' a InternalCombustionEngine
  _Electric ::
    Prism' a ElectricType
  _Rocket ::
    Prism' a ()
  _Jet ::
    Prism' a JetType
    
instance AsEngineType EngineType where
  _EngineType =
    id
  _InternalCombustionEngineEngineType =
    prism'
      InternalCombustionEngineEngineType
      (
        \case
          InternalCombustionEngineEngineType t ->
            Just t
          _ ->
            Nothing
      )
  _Electric =
    prism'
      Electric
      (
        \case
          Electric t ->
            Just t
          _ ->
            Nothing
      )
  _Rocket =
    prism'
      (\() -> Rocket)
      (
        \case
          Rocket ->
            Just ()
          _ ->
            Nothing
      )
  _Jet =
    prism'
      Jet
      (
        \case
          Jet t ->
            Just t
          _ ->
            Nothing
      )
