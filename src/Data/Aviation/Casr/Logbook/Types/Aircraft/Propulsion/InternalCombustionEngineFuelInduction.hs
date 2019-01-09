{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction where

import Control.Lens(Lens', Prism', prism')
import GHC.Generics(Generic)
import Prelude

data InternalCombustionEngineFuelInduction =
  Carburettor
  | FuelInjection
  deriving (Eq, Ord, Show, Generic)

class HasInternalCombustionEngineFuelInduction a where
  internalCombustionEngineFuelInduction ::
    Lens' a InternalCombustionEngineFuelInduction

instance HasInternalCombustionEngineFuelInduction InternalCombustionEngineFuelInduction where
  internalCombustionEngineFuelInduction =
    id

class AsInternalCombustionEngineFuelInduction a where
  _InternalCombustionEngineFuelInduction ::
    Prism' a InternalCombustionEngineFuelInduction
  _Carburettor ::
    Prism' a ()
  _FuelInjection ::
    Prism' a ()

instance AsInternalCombustionEngineFuelInduction InternalCombustionEngineFuelInduction where
  _InternalCombustionEngineFuelInduction =
    id
  _Carburettor =
    prism'
      (\() -> Carburettor)
      (
        \case
          Carburettor ->
            Just ()
          _ ->
            Nothing
      )
  _FuelInjection =
    prism'
      (\() -> FuelInjection)
      (
        \case
          FuelInjection ->
            Just ()
          _ ->
            Nothing
      )
