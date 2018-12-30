{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine(
  RotaryEngine(..)
, HasRotaryEngine(..)
, AsRotaryEngine(..)
) where

import Control.Lens
import GHC.Generics
import Natural
import Prelude

data RotaryEngine =
  RotaryEngine
    Positive -- rotors
    Positive -- capacity
  deriving (Eq, Ord, Show, Generic)

makeClassy ''RotaryEngine

class AsRotaryEngine a where
  _RotaryEngine ::
    Prism' a RotaryEngine

instance AsRotaryEngine RotaryEngine where
  _RotaryEngine =
    id
