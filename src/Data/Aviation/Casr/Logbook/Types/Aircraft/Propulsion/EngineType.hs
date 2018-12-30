{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineType where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.ElectricType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.JetType
import GHC.Generics
import Prelude

data EngineType =
  InternalCombustionEngineEngineType InternalCombustionEngine
  | Eletric ElectricType
  | Jet JetType
  | Rocket
  deriving (Eq, Ord, Show, Generic)

makeClassy ''EngineType
makeClassyPrisms ''EngineType
