{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement(EngineDisplacement, HasEngineDisplacement(engineDisplacement))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors(Rotors, HasRotors(rotors))
import GHC.Generics(Generic)
import Prelude

data RotaryEngine =
  RotaryEngine
    Rotors
    EngineDisplacement
  deriving (Eq, Ord, Show, Generic)

class HasRotaryEngine a where
  rotaryEngine ::
    Lens' a RotaryEngine

instance HasRotaryEngine RotaryEngine where
  rotaryEngine =
    id

class AsRotaryEngine a where
  _RotaryEngine ::
    Prism' a RotaryEngine
 
instance AsRotaryEngine RotaryEngine where
  _RotaryEngine =
    id

----

instance HasRotors RotaryEngine where
  rotors f (RotaryEngine r d) =
    fmap (\r' -> RotaryEngine r' d) (f r)

instance HasEngineDisplacement RotaryEngine where
  engineDisplacement f (RotaryEngine r d) =
    fmap (\d' -> RotaryEngine r d') (f d)
