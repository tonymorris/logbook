{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction(
  InternalCombustionEngineFuelInduction(..)
, HasInternalCombustionEngineFuelInduction(..)
, AsInternalCombustionEngineFuelInduction(..)
) where

import Control.Lens
import GHC.Generics
import Prelude

data InternalCombustionEngineFuelInduction =
  Carburettor
  | FuelInjection
  deriving (Eq, Ord, Show, Generic)

makeClassy ''InternalCombustionEngineFuelInduction
makeClassyPrisms ''InternalCombustionEngineFuelInduction
