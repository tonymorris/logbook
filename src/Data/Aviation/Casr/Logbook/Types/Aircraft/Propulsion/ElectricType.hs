{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.ElectricType where

import Control.Lens(Lens', Prism', prism')
import GHC.Generics(Generic)
import Prelude

data ElectricType =
  BrushlessAC
  | BrushedAC
  | BrushlessDC
  | BrushedDC
  | UniversalACDC
  | SwitchedReluctance
  | DirectDrive
  | Linear
  deriving (Eq, Ord, Show, Generic)

class HasElectricType a where
  electricType ::
    Lens' a ElectricType

instance HasElectricType ElectricType where
  electricType =
    id
  
class AsElectricType a where
  _ElectricType ::
    Prism' a ElectricType
  _BrushlessAC ::
    Prism' a ()
  _BrushedAC ::
    Prism' a ()
  _BrushlessDC ::
    Prism' a ()
  _BrushedDC ::
    Prism' a ()
  _UniversalACDC ::
    Prism' a ()
  _SwitchedReluctance ::
    Prism' a ()
  _DirectDrive ::
    Prism' a ()
  _Linear ::
    Prism' a ()

instance AsElectricType ElectricType where
  _ElectricType =
    id
  _BrushlessAC =
    prism'
      (\() -> BrushlessAC)
      (
        \case
          BrushlessAC ->
            Just ()
          _ ->
            Nothing
      )
  _BrushedAC =
    prism'
      (\() -> BrushedAC)
      (
        \case
          BrushedAC ->
            Just ()
          _ ->
            Nothing
      )
  _BrushlessDC =
    prism'
      (\() -> BrushlessDC)
      (
        \case
          BrushlessDC ->
            Just ()
          _ ->
            Nothing
      )
  _BrushedDC =
    prism'
      (\() -> BrushedDC)
      (
        \case
          BrushedDC ->
            Just ()
          _ ->
            Nothing
      )
  _UniversalACDC =
    prism'
      (\() -> UniversalACDC)
      (
        \case
          UniversalACDC ->
            Just ()
          _ ->
            Nothing
      )
  _SwitchedReluctance =
    prism'
      (\() -> SwitchedReluctance)
      (
        \case
          SwitchedReluctance ->
            Just ()
          _ ->
            Nothing
      )
  _DirectDrive =
    prism'
      (\() -> DirectDrive)
      (
        \case
          DirectDrive ->
            Just ()
          _ ->
            Nothing
      )
  _Linear =
    prism'
      (\() -> Linear)
      (
        \case
          Linear ->
            Just ()
          _ ->
            Nothing
      )
