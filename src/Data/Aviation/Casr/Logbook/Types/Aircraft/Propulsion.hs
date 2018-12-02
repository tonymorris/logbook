{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion(
  module Bool
, Propulsion(..)
, Propulsion'
, PropulsionI
, propulsionI
) where

import Control.Applicative(Applicative(pure))
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
import Data.Functor.Identity
import GHC.Generics
import Data.Bool as Bool(Bool(False, True))

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
  (Applicative position, Applicative vtol) =>
  PropulsionType cylinders displacement jettype
  -> PropulsionPosition
  -> Bool
  -> Propulsion cylinders displacement jettype position vtol
propulsionI propulsiontype propulsionposition vtol =
  Propulsion propulsiontype (pure propulsionposition) (pure vtol)
