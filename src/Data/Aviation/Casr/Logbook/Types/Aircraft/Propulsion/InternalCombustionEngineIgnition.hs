{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition where

import Control.Lens(Lens', Prism', prism')
import GHC.Generics(Generic)
import Prelude

data InternalCombustionEngineIgnition=
  Diesel
  | Spark
  deriving (Eq, Ord, Show, Generic)

class HasInternalCombustionEngineIgnition a where
  internalCombustionEngineIgnition ::
    Lens' a InternalCombustionEngineIgnition

instance HasInternalCombustionEngineIgnition InternalCombustionEngineIgnition where
  internalCombustionEngineIgnition =
    id

class AsInternalCombustionEngineIgnition a where
  _InternalCombustionEngineIgnition ::
    Prism' a InternalCombustionEngineIgnition
  _Diesel ::
    Prism' a ()
  _Spark ::
    Prism' a ()

instance AsInternalCombustionEngineIgnition InternalCombustionEngineIgnition where
  _InternalCombustionEngineIgnition =
    id
  _Diesel =
    prism'
      (\() -> Diesel)
      (
        \case
          Diesel ->
            Just ()
          _ ->
            Nothing
      )
  _Spark =
    prism'
      (\() -> Spark)
      (
        \case
          Spark ->
            Just ()
          _ ->
            Nothing
      )
