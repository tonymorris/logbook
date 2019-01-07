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

data EngineType x xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype =
  InternalCombustionEngineEngineType !(XInternalCombustionEngineEngineType x) (InternalCombustionEngine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  | Electric !(XElectric x) (ElectricType xelectrictype)
  | Jet !(XJet x) (JetType xjettype)
  | Rocket !(XRocket x)
  | EngineType !(XEngineType x)
  deriving Generic

deriving instance (Eq (XInternalCombustionEngineEngineType x), Eq (InternalCombustionEngine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine), Eq (XElectric x), Eq (ElectricType xelectrictype), Eq (XJet x), Eq (JetType xjettype), Eq (XRocket x), Eq (XEngineType x)) =>
  Eq (EngineType x xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

deriving instance (Ord (XInternalCombustionEngineEngineType x), Ord (InternalCombustionEngine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine), Ord (XElectric x), Ord (ElectricType xelectrictype), Ord (XJet x), Ord (JetType xjettype), Ord (XRocket x), Ord (XEngineType x)) =>
  Ord (EngineType x xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

deriving instance (Show (XInternalCombustionEngineEngineType x), Show (InternalCombustionEngine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine), Show (XElectric x), Show (ElectricType xelectrictype), Show (XJet x), Show (JetType xjettype), Show (XRocket x), Show (XEngineType x)) =>
  Show (EngineType x xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

class HasEngineType a e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype | a -> e, a -> xinternalcombustionengine, a -> xinternalcombustionengineairinduction, a -> xinternalcombustionenginefuelinduction, a -> xinternalcombustionengineignition, a -> xinternalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine, a -> xelectrictype, a -> xjettype where
  engineType ::
    Lens' a (EngineType e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)
  xEngineType ::
    Lens' a (XInternalCombustionEngineEngineType e)
  xEngineType =
    engineType . xEngineType

instance HasEngineType (EngineType e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype where
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
  Lens' (EngineType e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) x
xEngineType' f (InternalCombustionEngineEngineType x t) =
  fmap (\x' -> InternalCombustionEngineEngineType x' t) (f x)
xEngineType' f (Electric x t) =
  fmap (\x' -> Electric x' t) (f x)
xEngineType' f (Rocket x) =
  fmap (\x' -> Rocket x') (f x)
xEngineType' f (Jet x t) =
  fmap (\x' -> Jet x' t) (f x)
xEngineType' _ (EngineType x) =
  absurd x

class AsEngineType a e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype | a -> e, a -> xinternalcombustionengine, a -> xinternalcombustionengineairinduction, a -> xinternalcombustionenginefuelinduction, a -> xinternalcombustionengineignition, a -> xinternalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine, a -> xelectrictype, a -> xjettype where
  _EngineType ::
    Prism' a (EngineType e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)
  _InternalCombustionEngineEngineType ::
    Prism' a (XInternalCombustionEngineEngineType e, InternalCombustionEngine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  _InternalCombustionEngineEngineType' ::
    XInternalCombustionEngineEngineType e ~ () =>
    Prism' a (InternalCombustionEngine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  _InternalCombustionEngineEngineType' =
    _InternalCombustionEngineEngineType . unproduct
  _Electric ::
    Prism' a (XElectric e, (ElectricType xelectrictype))
  _Electric' ::
    XElectric e ~ () =>
    Prism' a (ElectricType xelectrictype)
  _Electric' =
    _Electric . unproduct
  _Rocket ::
    Prism' a (XRocket e)
  _Jet ::
    Prism' a (XJet e, JetType xjettype)
  _Jet' ::
    XJet e ~ () =>
    Prism' a (JetType xjettype)
  _Jet' =
    _Jet . unproduct
  _XEngineType ::
    Prism' a (XEngineType e)
    
instance AsEngineType (EngineType e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) e xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype where
  _EngineType =
    id
  _InternalCombustionEngineEngineType =
    prism'
      (\(x, t) -> InternalCombustionEngineEngineType x t)
      (
        \case
          InternalCombustionEngineEngineType x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _Electric =
    prism'
      (\(x, t) -> Electric x t)
      (
        \case
          Electric x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _Rocket =
    prism'
      Rocket
      (
        \case
          Rocket x ->
            Just x
          _ ->
            Nothing
      )
  _Jet =
    prism'
      (\(x, t) -> Jet x t)
      (
        \case
          Jet x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _XEngineType =
    prism'
      EngineType
      (
        \case
          EngineType x ->
            Just x
          _ ->
            Nothing
      )

type EngineType_ =
  EngineType ()

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

pattern InternalCombustionEngineEngineType_ ::
  InternalCombustionEngine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
  -> EngineType_ xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern InternalCombustionEngineEngineType_ t <- InternalCombustionEngineEngineType _ t
  where InternalCombustionEngineEngineType_ t = InternalCombustionEngineEngineType () t

pattern Electric_ ::
  ElectricType xelectrictype
  -> EngineType_ xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern Electric_ t <- Electric _ t
  where Electric_ t = Electric () t

pattern Rocket_ ::
  EngineType_ xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern Rocket_ <- Rocket _
  where Rocket_ = Rocket ()

pattern Jet_ ::
  JetType xjettype
  -> EngineType_ xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern Jet_ t <- Jet _ t
  where Jet_ t = Jet () t

pattern EngineType_ ::
  Void
  -> EngineType_ xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern EngineType_ v <- EngineType v
  where EngineType_ v = EngineType v
