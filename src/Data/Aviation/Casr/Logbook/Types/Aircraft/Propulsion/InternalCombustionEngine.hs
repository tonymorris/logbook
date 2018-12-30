{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine(
  InternalCombustionEngine(..)
, HasInternalCombustionEngine(..)
, AsInternalCombustionEngine(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType
import GHC.Generics
import Prelude

data InternalCombustionEngine =
  InternalCombustionEngine
    InternalCombustionEngineAirInduction
    InternalCombustionEngineFuelInduction
    InternalCombustionEngineIgnition
    InternalCombustionEngineType
  deriving (Eq, Ord, Show, Generic)

makeClassy ''InternalCombustionEngine

class AsInternalCombustionEngine a where
  _InternalCombustionEngine ::
    Prism' a InternalCombustionEngine

instance AsInternalCombustionEngine InternalCombustionEngine where
  _InternalCombustionEngine =
    id
    