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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType where

import Control.Lens(Lens', Prism', prism')
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine(PistonEngine, AsPistonEngine(_PistonEngine))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine(RotaryEngine, AsRotaryEngine(_RotaryEngine))
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude
import Util

type family XPistonEngineType x
type family XRotaryEngineType x
type family XInternalCombustionEngineType x

data InternalCombustionEngineType x xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine =
  PistonEngineType !(XPistonEngineType x) (PistonEngine xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine)
  | RotaryEngineType !(XRotaryEngineType x) (RotaryEngine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  | InternalCombustionEngineType !(XInternalCombustionEngineType x)
  deriving Generic

deriving instance (Eq (XPistonEngineType x), Eq (PistonEngine xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine), Eq (XRotaryEngineType x), Eq (RotaryEngine xrotaryengine xrotors xenginedisplacement_rotaryengine), Eq (XInternalCombustionEngineType x)) =>
  Eq (InternalCombustionEngineType x xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

deriving instance (Ord (XPistonEngineType x), Ord (PistonEngine xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine), Ord (XRotaryEngineType x), Ord (RotaryEngine xrotaryengine xrotors xenginedisplacement_rotaryengine), Ord (XInternalCombustionEngineType x)) =>
  Ord (InternalCombustionEngineType x xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

deriving instance (Show (XPistonEngineType x), Show (PistonEngine xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine), Show (XRotaryEngineType x), Show (RotaryEngine xrotaryengine xrotors xenginedisplacement_rotaryengine), Show (XInternalCombustionEngineType x)) =>
  Show (InternalCombustionEngineType x xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

class HasInternalCombustionEngineType a e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine | a -> e, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine where
  internalCombustionEngineType ::
    Lens' a (InternalCombustionEngineType e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  xInternalCombustionEngineType ::
    Lens' a (XInternalCombustionEngineType e)
  xInternalCombustionEngineType =
    internalCombustionEngineType . xInternalCombustionEngineType

instance HasInternalCombustionEngineType (InternalCombustionEngineType e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  internalCombustionEngineType =
    id
 
xInternalCombustionEngineType' ::
  (
    XPistonEngineType e ~ x
  , XRotaryEngineType e ~ x
  , XInternalCombustionEngineType e ~ Void
  ) =>
  Lens' (InternalCombustionEngineType e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) x 
xInternalCombustionEngineType' f (PistonEngineType x t) =
  fmap (\x' -> PistonEngineType x' t) (f x)
xInternalCombustionEngineType' f (RotaryEngineType x t) =
  fmap (\x' -> RotaryEngineType x' t) (f x)
xInternalCombustionEngineType' _ (InternalCombustionEngineType x) =
  absurd x

class AsInternalCombustionEngineType a e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine | a -> e, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine where
  _InternalCombustionEngineType ::
    Prism' a (InternalCombustionEngineType e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  _PistonEngineType ::
    Prism' a (XPistonEngineType e, (PistonEngine xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine))
  _PistonEngineType' ::
    XPistonEngineType e ~ () =>
    Prism' a (PistonEngine xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine)
  _PistonEngineType' =
    _PistonEngineType . unproduct
  _RotaryEngineType ::
    Prism' a (XRotaryEngineType e, (RotaryEngine xrotaryengine xrotors xenginedisplacement_rotaryengine))
  _RotaryEngineType' ::
    XRotaryEngineType e ~ () =>
    Prism' a (RotaryEngine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  _RotaryEngineType' =
    _RotaryEngineType . unproduct
  _XInternalCombustionEngineType ::
    Prism' a (XInternalCombustionEngineType e)

instance AsInternalCombustionEngineType (InternalCombustionEngineType e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  _InternalCombustionEngineType =
    id
  _PistonEngineType =
    prism'
      (\(x, t) -> PistonEngineType x t)
      (
        \case
          PistonEngineType x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _RotaryEngineType =
    prism'
      (\(x, t) -> RotaryEngineType x t)
      (
        \case
          RotaryEngineType x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _XInternalCombustionEngineType =
    prism'
      InternalCombustionEngineType
      (
        \case
          InternalCombustionEngineType x ->
            Just x
          _ ->
            Nothing
      )

type InternalCombustionEngineType_ =
  InternalCombustionEngineType ()

type instance XPistonEngineType () =
  ()
type instance XRotaryEngineType () =
  ()
type instance XInternalCombustionEngineType () =
  Void

pattern PistonEngineType_ ::
  PistonEngine xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine
  -> InternalCombustionEngineType_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
pattern PistonEngineType_ t <- PistonEngineType _ t
  where PistonEngineType_ t = PistonEngineType () t

pattern RotaryEngineType_ ::
  RotaryEngine xrotaryengine xrotors xenginedisplacement_rotaryengine
  -> InternalCombustionEngineType_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
pattern RotaryEngineType_ t <- RotaryEngineType _ t
  where RotaryEngineType_ t = RotaryEngineType () t

pattern InternalCombustionEngineType_ ::
  Void
  -> InternalCombustionEngineType_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
pattern InternalCombustionEngineType_ v <- InternalCombustionEngineType v
  where InternalCombustionEngineType_ v = InternalCombustionEngineType v

----

instance AsPistonEngine (InternalCombustionEngineType_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine where
  _PistonEngine =
    prism'
      PistonEngineType_
      (
        \case
          PistonEngineType_ t ->
            Just t
          _ ->
            Nothing
      )

instance AsRotaryEngine (InternalCombustionEngineType_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) xrotaryengine xrotors xenginedisplacement_rotaryengine where
  _RotaryEngine =
    prism'
      RotaryEngineType_
      (
        \case
          RotaryEngineType_ t ->
            Just t
          _ ->
            Nothing
      )
