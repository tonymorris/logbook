{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Engine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineType(EngineType, HasEngineType(engineType))
import GHC.Generics(Generic)
import Prelude

type Manufacturer =
  String

type Designation =
  String

data Engine =
  Engine
    Manufacturer
    Designation
    EngineType
  deriving (Eq, Ord, Show, Generic)

class HasEngine a where
  engine ::
    Lens' a Engine
  manufacturer ::
    Lens' a Manufacturer
  manufacturer =
    engine . manufacturer    
  designation ::
    Lens' a Designation
  designation =
    engine . designation

instance HasEngine Engine where
  engine =
    id

class AsEngine a where
  _Engine ::
    Prism' a Engine

instance AsEngine Engine where
  _Engine =
    id

----

instance HasEngineType Engine where
  engineType f (Engine m d t) =
    fmap (\t' -> Engine m d t') (f t) 
