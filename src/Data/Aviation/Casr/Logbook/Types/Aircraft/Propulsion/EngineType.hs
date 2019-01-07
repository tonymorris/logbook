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

data EngineType_ x internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype =
  InternalCombustionEngineEngineType_ !(XInternalCombustionEngineEngineType x) (InternalCombustionEngine_ internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  | Electric_ !(XElectric x) (ElectricType_ xelectrictype)
  | Jet_ !(XJet x) (JetType_ xjettype)
  | Rocket_ !(XRocket x)
  | EngineType_ !(XEngineType x)
  deriving Generic

deriving instance (Eq (XInternalCombustionEngineEngineType x), Eq (InternalCombustionEngine_ internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine), Eq (XElectric x), Eq (ElectricType_ xelectrictype), Eq (XJet x), Eq (JetType_ xjettype), Eq (XRocket x), Eq (XEngineType x)) =>
  Eq (EngineType_ x internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

deriving instance (Ord (XInternalCombustionEngineEngineType x), Ord (InternalCombustionEngine_ internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine), Ord (XElectric x), Ord (ElectricType_ xelectrictype), Ord (XJet x), Ord (JetType_ xjettype), Ord (XRocket x), Ord (XEngineType x)) =>
  Ord (EngineType_ x internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

deriving instance (Show (XInternalCombustionEngineEngineType x), Show (InternalCombustionEngine_ internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine), Show (XElectric x), Show (ElectricType_ xelectrictype), Show (XJet x), Show (JetType_ xjettype), Show (XRocket x), Show (XEngineType x)) =>
  Show (EngineType_ x internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

class HasEngineType a e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype | a -> e, a -> internalcombustionengine, a -> internalcombustionengineairinduction, a -> internalcombustionenginefuelinduction, a -> internalcombustionengineignition, a -> internalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine, a -> xelectrictype, a -> xjettype where
  engineType ::
    Lens' a (EngineType_ e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)
  xEngineType ::
    Lens' a (XInternalCombustionEngineEngineType e)
  xEngineType =
    engineType . xEngineType

instance HasEngineType (EngineType_ e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype where
  engineType =
    id

xEngineType' ::
  (
    XInternalCombustionEngineEngineType e ~ x
  , XElectric e ~ x
  , XRocket e ~ x
  , XJet e ~ x
  , XEngineType e ~ Void
  ) =>
  Lens' (EngineType_ e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) x
xEngineType' f (InternalCombustionEngineEngineType_ x t) =
  fmap (\x' -> InternalCombustionEngineEngineType_ x' t) (f x)
xEngineType' f (Electric_ x t) =
  fmap (\x' -> Electric_ x' t) (f x)
xEngineType' f (Rocket_ x) =
  fmap (\x' -> Rocket_ x') (f x)
xEngineType' f (Jet_ x t) =
  fmap (\x' -> Jet_ x' t) (f x)
xEngineType' _ (EngineType_ x) =
  absurd x

class AsEngineType a e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype | a -> e, a -> internalcombustionengine, a -> internalcombustionengineairinduction, a -> internalcombustionenginefuelinduction, a -> internalcombustionengineignition, a -> internalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine, a -> xelectrictype, a -> xjettype where
  _EngineType ::
    Prism' a (EngineType_ e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)
  _InternalCombustionEngineEngineType ::
    Prism' a (XInternalCombustionEngineEngineType e, InternalCombustionEngine_ internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  _InternalCombustionEngineEngineType' ::
    XInternalCombustionEngineEngineType e ~ () =>
    Prism' a (InternalCombustionEngine_ internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  _InternalCombustionEngineEngineType' =
    _InternalCombustionEngineEngineType . unproduct
  _Electric ::
    Prism' a (XElectric e, (ElectricType_ xelectrictype))
  _Electric' ::
    XElectric e ~ () =>
    Prism' a (ElectricType_ xelectrictype)
  _Electric' =
    _Electric . unproduct
  _Rocket ::
    Prism' a (XRocket e)
  _Jet ::
    Prism' a (XJet e, JetType_ xjettype)
  _Jet' ::
    XJet e ~ () =>
    Prism' a (JetType_ xjettype)
  _Jet' =
    _Jet . unproduct
  _XEngineType ::
    Prism' a (XEngineType e)
    
instance AsEngineType (EngineType_ e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) e internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype where
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
  InternalCombustionEngine_ internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
  -> EngineType internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern InternalCombustionEngineEngineType t <- InternalCombustionEngineEngineType_ _ t
  where InternalCombustionEngineEngineType t = InternalCombustionEngineEngineType_ () t

pattern Electric ::
  ElectricType_ xelectrictype
  -> EngineType internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern Electric t <- Electric_ _ t
  where Electric t = Electric_ () t

pattern Rocket ::
  EngineType internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern Rocket <- Rocket_ _
  where Rocket = Rocket_ ()

pattern Jet ::
  JetType_ xjettype
  -> EngineType internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern Jet t <- Jet_ _ t
  where Jet t = Jet_ () t

pattern EngineType ::
  Void
  -> EngineType internalcombustionengine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern EngineType v <- EngineType_ v
  where EngineType v = EngineType_ v
