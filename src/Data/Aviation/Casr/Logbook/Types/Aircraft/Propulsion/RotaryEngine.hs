{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine(
  RotaryEngine(..)
, HasRotaryEngine(..)
, AsRotaryEngine(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors
import GHC.Generics
import Prelude

data RotaryEngine =
  RotaryEngine
    Rotors
    EngineDisplacement
  deriving (Eq, Ord, Show, Generic)

makeClassy ''RotaryEngine

class AsRotaryEngine a where
  _RotaryEngine ::
    Prism' a RotaryEngine

instance AsRotaryEngine RotaryEngine where
  _RotaryEngine =
    id
