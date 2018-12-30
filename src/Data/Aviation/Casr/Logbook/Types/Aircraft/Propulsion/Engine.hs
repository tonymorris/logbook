{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Engine(
  Engine(..)
, HasEngine(..)
, AsEngine(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineType
import GHC.Generics
import Prelude

data Engine =
  Engine
    String -- manufacturer
    String -- designation
    EngineType
  deriving (Eq, Ord, Show, Generic)

makeClassy ''Engine

class AsEngine a where
  _Engine ::
    Prism' a Engine

instance AsEngine Engine where
  _Engine =
    id
    