{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine
import GHC.Generics
import Prelude

data InternalCombustionEngineType =
  PistonEngineType PistonEngine
  | RotaryEngineType RotaryEngine
  deriving (Eq, Ord, Show, Generic)

makeClassy ''InternalCombustionEngineType
makeClassyPrisms ''InternalCombustionEngineType
