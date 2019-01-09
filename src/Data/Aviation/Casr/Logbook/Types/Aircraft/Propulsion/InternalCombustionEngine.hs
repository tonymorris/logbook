{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction( InternalCombustionEngineAirInduction, HasInternalCombustionEngineAirInduction(internalCombustionEngineAirInduction))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction(InternalCombustionEngineFuelInduction, HasInternalCombustionEngineFuelInduction(internalCombustionEngineFuelInduction))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition(InternalCombustionEngineIgnition, HasInternalCombustionEngineIgnition(internalCombustionEngineIgnition))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType(InternalCombustionEngineType, HasInternalCombustionEngineType(internalCombustionEngineType))
import GHC.Generics(Generic)
import Prelude

data InternalCombustionEngine =
  InternalCombustionEngine
    InternalCombustionEngineAirInduction
    InternalCombustionEngineFuelInduction
    InternalCombustionEngineIgnition
    InternalCombustionEngineType
  deriving (Eq, Ord, Show, Generic)

class HasInternalCombustionEngine a where
  internalCombustionEngine ::
    Lens' a InternalCombustionEngine

instance HasInternalCombustionEngine InternalCombustionEngine where
  internalCombustionEngine =
    id

class AsInternalCombustionEngine a where
  _InternalCombustionEngine ::
    Prism' a InternalCombustionEngine

instance AsInternalCombustionEngine InternalCombustionEngine where
  _InternalCombustionEngine =
    id

----

instance HasInternalCombustionEngineAirInduction InternalCombustionEngine where
  internalCombustionEngineAirInduction f (InternalCombustionEngine a l i t) =
    fmap (\a' -> InternalCombustionEngine a' l i t) (f a)

instance HasInternalCombustionEngineFuelInduction InternalCombustionEngine where
  internalCombustionEngineFuelInduction f (InternalCombustionEngine a l i t) =
    fmap (\l' -> InternalCombustionEngine a l' i t) (f l)

instance HasInternalCombustionEngineIgnition InternalCombustionEngine where
  internalCombustionEngineIgnition f (InternalCombustionEngine a l i t) =
    fmap (\i' -> InternalCombustionEngine a l i' t) (f i)

instance HasInternalCombustionEngineType InternalCombustionEngine where
  internalCombustionEngineType f (InternalCombustionEngine a l i t) =
    fmap (\t' -> InternalCombustionEngine a l i t') (f t)
