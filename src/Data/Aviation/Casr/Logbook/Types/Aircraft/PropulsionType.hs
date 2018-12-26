{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType(
  module Natural
, PropulsionType(..)
, HasPropulsionType(..)
, AsPropulsionType(..)
, PropulsionType'
, PropulsionTypeI
, __Piston
, __Jet
, __Electric
, __Rocket
, pistonPropulsionTypeI
, jetPropulsionTypeI
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import GHC.Generics
import Natural as Natural
import Prelude

data PropulsionType cylinders displacement jettype =
  Piston (cylinders Positive) (displacement Positive) -- cc
  | Jet (jettype JetType)
  | Electric
  | Rocket
  deriving Generic

makeClassy ''PropulsionType
makeClassyPrisms ''PropulsionType

type PropulsionType' a =
  PropulsionType a a a

type PropulsionTypeI =
  PropulsionType' Identity

__Piston ::
  Prism (PropulsionType cylinders displacement jettype) (PropulsionType cylinders' displacement' jettype) (cylinders Positive, displacement Positive) (cylinders' Positive, displacement' Positive)
__Piston =
  prism
    (\(c, d) -> Piston c d)
    (\case
      Piston c d -> Right (c, d)
      Jet p -> Left (Jet p)
      Electric -> Left Electric
      Rocket -> Left Rocket)

__Jet ::
  Prism (PropulsionType cylinders displacement jettype) (PropulsionType cylinders displacement jettype') (jettype JetType) (jettype' JetType)
__Jet =
  prism
    Jet
    (\case
      Jet p -> Right p
      Piston c d -> Left (Piston c d)
      Electric -> Left Electric
      Rocket -> Left Rocket)

__Electric ::
  Prism (PropulsionType cylinders displacement jettype) (PropulsionType cylinders displacement jettype) () ()
__Electric =
  prism
    (\() -> Electric)
    (\case
      Electric -> Right ()
      Jet p -> Left (Jet p)
      Piston c d -> Left (Piston c d)
      Rocket -> Left Rocket)

__Rocket ::
  Prism (PropulsionType cylinders displacement jettype) (PropulsionType cylinders displacement jettype) () ()
__Rocket =
  prism
    (\() -> Rocket)
    (\case
      Rocket -> Right ()
      Jet p -> Left (Jet p)
      Piston c d -> Left (Piston c d)
      Electric -> Left Electric)
    
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
