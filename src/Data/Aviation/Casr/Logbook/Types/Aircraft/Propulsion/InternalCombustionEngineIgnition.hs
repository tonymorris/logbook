{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition(
  InternalCombustionEngineIgnition(..)
, HasInternalCombustionEngineIgnition(..)
, AsInternalCombustionEngineIgnition(..)
) where

import Control.Lens
import GHC.Generics
import Prelude

data InternalCombustionEngineIgnition =
  Diesel
  | Spark
  deriving (Eq, Ord, Show, Generic)

makeClassy ''InternalCombustionEngineIgnition
makeClassyPrisms ''InternalCombustionEngineIgnition
