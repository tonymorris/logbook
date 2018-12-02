{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion where

import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
import Data.Functor.Identity
import GHC.Generics
import Prelude

data Propulsion cylinders displacement jettype position vtol =
  Propulsion
    (PropulsionType cylinders displacement jettype)
    (position PropulsionPosition)
    (vtol Bool)
  deriving Generic

type Propulsion' a =
  Propulsion a a a a a
  
type PropulsionI =
  Propulsion' Identity
  