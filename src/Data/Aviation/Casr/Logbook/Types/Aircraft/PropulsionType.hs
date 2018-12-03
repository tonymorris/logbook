{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType(
  module Natural
, PropulsionType(..)
, PropulsionType'
, PropulsionTypeI
, pistonPropulsionTypeI
, jetPropulsionTypeI
) where

import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import GHC.Generics
import Natural as Natural
import Prelude
import Data.Functor.Identity

data PropulsionType cylinders displacement jettype =
  Piston (cylinders Positive) (displacement Positive) -- cc
  | Jet (jettype JetType)
  | Electric
  | Rocket
  deriving Generic

type PropulsionType' a =
  PropulsionType a a a

type PropulsionTypeI =
  PropulsionType' Identity

pistonPropulsionTypeI ::
  (Applicative cylinders, Applicative displacement) =>
  Positive
  -> Positive
  -> PropulsionType cylinders displacement jettype
pistonPropulsionTypeI cylinders displacement =
  Piston (pure cylinders) (pure displacement)

jetPropulsionTypeI ::
  (Applicative jettype) =>
  JetType
  -> PropulsionType cylinders displacement jettype
jetPropulsionTypeI jettype =
  Jet (pure jettype)

deriving instance (Eq (cylinders Positive), Eq (displacement Positive), Eq (jettype JetType)) => Eq (PropulsionType cylinders displacement jettype)

deriving instance (Ord (cylinders Positive), Ord (displacement Positive), Ord (jettype JetType)) => Ord (PropulsionType cylinders displacement jettype)

deriving instance (Show (cylinders Positive), Show (displacement Positive), Show (jettype JetType)) => Show (PropulsionType cylinders displacement jettype)
