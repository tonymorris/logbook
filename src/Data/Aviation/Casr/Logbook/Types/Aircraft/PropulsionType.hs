{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType(
  module Natural
, PropulsionType(..)
, PropulsionType'
, PropulsionTypeI
, ManyPropulsionType(..)
, AsPropulsionType(..)
, HasPropulsionType(..)
, pistonPropulsionTypeI
, jetPropulsionTypeI
) where

import Control.Lens
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

class ManyPropulsionType t where
  _PropulsionType_ ::
    Traversal (t (cylinders :: * -> *) (displacement :: * -> *) (jettype :: * -> *)) (t cylinders' displacement' jettype') (t cylinders displacement jettype) (t cylinders' displacement' jettype') 

instance ManyPropulsionType PropulsionType where
  _PropulsionType_ =
    id

class ManyPropulsionType t => AsPropulsionType t where
  _PropulsionType ::
    Prism (t cylinders displacement (jettype :: * -> *)) (t cylinders' displacement' jettype') (t cylinders displacement jettype) (t cylinders' displacement' jettype')
  _Piston ::
    Prism (t cylinders displacement (jettype :: * -> *)) (t cylinders' displacement' jettype) (cylinders Positive, displacement Positive) (cylinders' Positive, displacement' Positive)
  _Jet ::
    Prism (t (cylinders :: * -> *) (displacement :: * -> *) jettype) (t cylinders displacement jettype') (jettype JetType) (jettype' JetType)
  _Electric ::
    Prism (t (cylinders :: * -> *) (displacement :: * -> *) (jettype :: * -> *)) (t cylinders displacement jettype) () ()
  _Rocket ::
    Prism (t (cylinders :: * -> *) (displacement :: * -> *) (jettype :: * -> *)) (t cylinders displacement jettype) () ()

instance AsPropulsionType PropulsionType where
  _PropulsionType =
    id
  _Piston =
    prism
      (\(c, d) -> Piston c d)
      (\case
        Piston c d -> Right (c, d)
        Jet p -> Left (Jet p)
        Electric -> Left Electric
        Rocket -> Left Rocket)
  _Jet =
    prism
      Jet
      (\case
        Jet p -> Right p
        Piston c d -> Left (Piston c d)
        Electric -> Left Electric
        Rocket -> Left Rocket)
  _Electric =
    prism
      (\() -> Electric)
      (\case
        Electric -> Right ()
        Jet p -> Left (Jet p)
        Piston c d -> Left (Piston c d)
        Rocket -> Left Rocket)
  _Rocket =
    prism
      (\() -> Rocket)
      (\case
        Rocket -> Right ()
        Jet p -> Left (Jet p)
        Piston c d -> Left (Piston c d)
        Electric -> Left Electric)

class ManyPropulsionType t => HasPropulsionType t where
  propulsionType ::
    Lens (t (cylinders :: * -> *) (displacement :: * -> *) (jettype :: * -> *)) (t cylinders' displacement' jettype') (t cylinders displacement jettype) (t cylinders' displacement' jettype') 

instance HasPropulsionType PropulsionType where
  propulsionType =
    id
    
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
