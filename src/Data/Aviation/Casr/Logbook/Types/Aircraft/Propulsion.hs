{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion(
  module Bool
, Propulsion(..)
, Propulsion'
, PropulsionI
, propulsionI
) where

import Control.Applicative(Applicative(pure))
import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
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
  (Applicative position, Applicative vtol) =>
  PropulsionType cylinders displacement jettype
  -> PropulsionPosition
  -> Bool
  -> Propulsion cylinders displacement jettype position vtol
propulsionI propulsiontype propulsionposition vtol =
  Propulsion propulsiontype (pure propulsionposition) (pure vtol)

deriving instance (Eq (cylinders Positive), Eq (displacement Positive), Eq (jettype JetType), Eq (position PropulsionPosition), Eq (vtol Bool)) => Eq (Propulsion cylinders displacement jettype position vtol)

deriving instance (Ord (cylinders Positive), Ord (displacement Positive), Ord (jettype JetType), Ord (position PropulsionPosition), Ord (vtol Bool)) => Ord (Propulsion cylinders displacement jettype position vtol)

deriving instance (Show (cylinders Positive), Show (displacement Positive), Show (jettype JetType), Show (position PropulsionPosition), Show (vtol Bool)) => Show (Propulsion cylinders displacement jettype position vtol)
