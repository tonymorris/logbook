{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion(
  module Bool
, Propulsion(..)
, Propulsion'
, PropulsionI
, propulsionI
) where

import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
import Data.Functor.Identity
import GHC.Generics
import Data.Bool as Bool(Bool(False, True))
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

propulsionI ::
  PropulsionTypeI
  -> PropulsionPosition
  -> Bool
  -> PropulsionI
propulsionI propulsiontype propulsionposition vtol =
  Propulsion propulsiontype (Identity propulsionposition) (Identity vtol)
