{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion(
  module Bool
, Propulsion(..)
, AsPropulsion(..)
, HasPropulsion(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
import GHC.Generics
import Data.Bool as Bool(Bool(False, True))
import Prelude

data Propulsion =
  Propulsion {
    _propulsion_type ::
      PropulsionType
  , _propulsion_position ::
      PropulsionPosition
  , _vtol ::
      Bool
  } deriving (Eq, Ord, Show, Generic)

makeClassy ''Propulsion

class AsPropulsion a where
  _Propulsion ::
    Prism' a Propulsion

instance AsPropulsion Propulsion where
  _Propulsion =
    id
