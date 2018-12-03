{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory where

import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Functor.Identity
import GHC.Generics
import Natural
import Prelude
 
data RPACategory cylinders displacement jettype position vtol rotors =
  RPAAeroplane (Propulsions1 cylinders displacement jettype position vtol)
  | RPACopter (Propulsions1 cylinders displacement jettype position vtol) (rotors Positive)
  | RPAAirship (Propulsions1 cylinders displacement jettype position vtol)
  | RPAPoweredLift (Propulsions1 cylinders displacement jettype position vtol)
  deriving Generic

type RPACategory' a =
  RPACategory a a a a a a

type RPACategoryI =
  RPACategory' Identity

deriving instance (Eq (cylinders Positive), Eq (displacement (Positive)), Eq (jettype JetType), Eq (position PropulsionPosition), Eq (vtol Bool), Eq (rotors Positive)) => Eq (RPACategory cylinders displacement jettype position vtol rotors)

deriving instance (Ord (cylinders Positive), Ord (displacement (Positive)), Ord (jettype JetType), Ord (position PropulsionPosition), Ord (vtol Bool), Ord (rotors Positive)) => Ord (RPACategory cylinders displacement jettype position vtol rotors)

deriving instance (Show (cylinders Positive), Show (displacement (Positive)), Show (jettype JetType), Show (position PropulsionPosition), Show (vtol Bool), Show (rotors Positive)) => Show (RPACategory cylinders displacement jettype position vtol rotors)
