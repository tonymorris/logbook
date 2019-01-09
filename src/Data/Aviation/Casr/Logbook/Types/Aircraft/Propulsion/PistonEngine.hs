{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineConfiguration
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle
import GHC.Generics
import Natural
import Prelude

type Cylinders =
  Positive

data PistonEngine =
  PistonEngine
    PistonEngineConfiguration
    PistonEngineCycle
    Cylinders
    EngineDisplacement
    deriving (Eq, Ord, Show, Generic)
    
class HasPistonEngine a where
  pistonEngine ::
    Lens' a PistonEngine

instance HasPistonEngine PistonEngine where
  pistonEngine =
    id

class AsPistonEngine a where
  _PistonEngine ::
    Prism' a PistonEngine

instance AsPistonEngine PistonEngine where
  _PistonEngine =
    id

----

instance HasPistonEngineConfiguration PistonEngine where
  pistonEngineConfiguration f (PistonEngine c y l d) =
    fmap (\c' -> PistonEngine c' y l d) (f c)

instance HasPistonEngineCycle PistonEngine where
  pistonEngineCycle f (PistonEngine c y l d) =
    fmap (\y' -> PistonEngine c y' l d) (f y)

instance HasPositive PistonEngine where
  positive f (PistonEngine c y l d) =
    fmap (\l' -> PistonEngine c y l' d) (f l)

instance HasEngineDisplacement PistonEngine where
  engineDisplacement f (PistonEngine c y l d) =
    fmap (\d' -> PistonEngine c y l d') (f d)
