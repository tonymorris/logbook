{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.ElectricType where

import Control.Lens
import GHC.Generics
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

makeClassy ''ElectricType
makeClassyPrisms ''ElectricType
