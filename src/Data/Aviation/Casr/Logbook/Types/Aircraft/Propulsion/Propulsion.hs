{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsion(
  Propulsion(..)
, HasPropulsion(..)
, AsPropulsion(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Engine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PropulsionPosition
import GHC.Generics
import Prelude

data Propulsion =
  Propulsion
    Engine
    PropulsionPosition
  deriving (Eq, Ord, Show, Generic)

makeClassy ''Propulsion

class AsPropulsion a where
  _Propulsion ::
    Prism' a Propulsion

instance AsPropulsion Propulsion where
  _Propulsion =
    id
    