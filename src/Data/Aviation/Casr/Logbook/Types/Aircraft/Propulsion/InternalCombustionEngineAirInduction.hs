{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction(
  InternalCombustionEngineAirInduction(..)
, HasInternalCombustionEngineAirInduction(..)
, AsInternalCombustionEngineAirInduction(..)
) where

import Control.Lens
import GHC.Generics
import Prelude

data InternalCombustionEngineAirInduction =
  Supercharged
  | Turbocharged
  | SuperTurbocharged
  | NaturalInduction
  deriving (Eq, Ord, Show, Generic)

makeClassy ''InternalCombustionEngineAirInduction
makeClassyPrisms ''InternalCombustionEngineAirInduction
