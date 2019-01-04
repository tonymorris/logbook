{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineType where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.ElectricType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.JetType
import Data.Void(Void, absurd)
import GHC.Generics
import Prelude
import Util

type family XInternalCombustionEngineEngineType x
type family XElectric x
type family XJet x
type family XRocket x
type family XEngineType x

data EngineType_ x =
  InternalCombustionEngineEngineType_ !(XInternalCombustionEngineEngineType x) InternalCombustionEngine
  | Electric_ !(XElectric x) ElectricType
  | Jet_ !(XJet x) JetType
  | Rocket_ !(XRocket x)
  | EngineType_ !(XEngineType x)
  deriving Generic

deriving instance (Eq (XInternalCombustionEngineEngineType x), Eq (XElectric x), Eq (XJet x), Eq (XRocket x), Eq (XEngineType x)) =>
  Eq (EngineType_ x)

deriving instance (Ord (XInternalCombustionEngineEngineType x), Ord (XElectric x), Ord (XJet x), Ord (XRocket x), Ord (XEngineType x)) =>
  Ord (EngineType_ x)

deriving instance (Show (XInternalCombustionEngineEngineType x), Show (XElectric x), Show (XJet x), Show (XRocket x), Show (XEngineType x)) =>
  Show (EngineType_ x)

class HasEngineType a e | a -> e where
  engineType ::
    Lens' a (EngineType_ e)
  xEngineType ::
    (
      XInternalCombustionEngineEngineType e ~ x
    , XElectric e ~ x
    , XRocket e ~ x
    , XJet e ~ x
    , XEngineType e ~ Void
    ) =>
    Lens' a x
  default xEngineType ::
    (
      XInternalCombustionEngineEngineType () ~ x
    , XElectric () ~ x
    , XRocket () ~ x
    , XJet () ~ x
    , XEngineType e ~ Void
    ) =>
    Lens' a x
  xEngineType f a =
    fmap (\() -> a) (f ())

instance HasEngineType (EngineType_ e) e where
  engineType =
    id
  xEngineType f (InternalCombustionEngineEngineType_ x t) =
    fmap (\x' -> InternalCombustionEngineEngineType_ x' t) (f x)
  xEngineType f (Electric_ x t) =
    fmap (\x' -> Electric_ x' t) (f x)
  xEngineType f (Rocket_ x) =
    fmap (\x' -> Rocket_ x') (f x)
  xEngineType f (Jet_ x t) =
    fmap (\x' -> Jet_ x' t) (f x)
  xEngineType _ (EngineType_ x) =
    absurd x

class AsEngineType a e | a -> e where
  _EngineType ::
    Prism' a (EngineType_ e)
  _InternalCombustionEngineEngineType ::
    Prism' a (XInternalCombustionEngineEngineType e, InternalCombustionEngine)
  _InternalCombustionEngineEngineType' ::
    XInternalCombustionEngineEngineType e ~ () =>
    Prism' a InternalCombustionEngine
  _InternalCombustionEngineEngineType' =
    _InternalCombustionEngineEngineType . unproduct
  _Electric ::
    Prism' a (XElectric e, ElectricType)
  _Electric' ::
    XElectric e ~ () =>
    Prism' a ElectricType
  _Electric' =
    _Electric . unproduct
  _Rocket ::
    Prism' a (XRocket e)
  _Jet ::
    Prism' a (XJet e, JetType)
  _Jet' ::
    XJet e ~ () =>
    Prism' a JetType
  _Jet' =
    _Jet . unproduct
  _XEngineType ::
    Prism' a (XEngineType e)
    
instance AsEngineType (EngineType_ e) e where
  _EngineType =
    id
  _InternalCombustionEngineEngineType =
    prism'
      (\(x, t) -> InternalCombustionEngineEngineType_ x t)
      (
        \case
          InternalCombustionEngineEngineType_ x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _Electric =
    prism'
      (\(x, t) -> Electric_ x t)
      (
        \case
          Electric_ x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _Rocket =
    prism'
      Rocket_
      (
        \case
          Rocket_ x ->
            Just x
          _ ->
            Nothing
      )
  _Jet =
    prism'
      (\(x, t) -> Jet_ x t)
      (
        \case
          Jet_ x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _XEngineType =
    prism'
      EngineType_
      (
        \case
          EngineType_ x ->
            Just x
          _ ->
            Nothing
      )

type EngineType =
  EngineType_ ()

type instance XInternalCombustionEngineEngineType () =
  ()
type instance XElectric () =
  ()
type instance XRocket () =
  ()
type instance XJet () =
  ()
type instance XEngineType () =
  Void

pattern InternalCombustionEngineEngineType ::
  InternalCombustionEngine
  -> EngineType
pattern InternalCombustionEngineEngineType t <- InternalCombustionEngineEngineType_ _ t
  where InternalCombustionEngineEngineType t = InternalCombustionEngineEngineType_ () t

pattern Electric ::
  ElectricType
  -> EngineType
pattern Electric t <- Electric_ _ t
  where Electric t = Electric_ () t

pattern Rocket ::
  EngineType
pattern Rocket <- Rocket_ _
  where Rocket = Rocket_ ()

pattern Jet ::
  JetType
  -> EngineType
pattern Jet t <- Jet_ _ t
  where Jet t = Jet_ () t

pattern EngineType ::
  Void
  -> EngineType
pattern EngineType v <- EngineType_ v
  where EngineType v = EngineType_ v
