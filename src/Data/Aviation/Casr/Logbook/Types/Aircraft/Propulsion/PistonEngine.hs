{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine(
  PistonEngine(..)
, HasPistonEngine(..)
, AsPistonEngine(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineConfiguration
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle
import GHC.Generics
import Natural
import Prelude

data PistonEngine =
  PistonEngine
    PistonEngineConfiguration
    PistonEngineCycle
    Positive -- cylinders
    Positive -- capacity
    deriving (Eq, Ord, Show, Generic)

makeClassy ''PistonEngine

class AsPistonEngine a where
  _PistonEngine ::
    Prism' a PistonEngine

instance AsPistonEngine PistonEngine where
  _PistonEngine =
    id
    