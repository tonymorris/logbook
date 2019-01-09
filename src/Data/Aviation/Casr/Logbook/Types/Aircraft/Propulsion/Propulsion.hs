{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsion where

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

class HasPropulsion a where
  propulsion ::
    Lens' a Propulsion

instance HasPropulsion Propulsion where
  propulsion =
    id

class AsPropulsion a where
  _Propulsion ::
    Prism' a Propulsion

instance AsPropulsion Propulsion where
  _Propulsion =
    id

----

instance HasEngine Propulsion where
  engine f (Propulsion e p) =
    fmap (\e' -> Propulsion e' p) (f e)

instance HasPropulsionPosition Propulsion where
  propulsionPosition f (Propulsion e p) =
    fmap (\p' -> Propulsion e p') (f p)
