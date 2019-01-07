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
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine(PistonEngine_, AsPistonEngine(_PistonEngine))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine(RotaryEngine_, AsRotaryEngine(_RotaryEngine))
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude
import Util

type family XPistonEngineType x
type family XRotaryEngineType x
type family XInternalCombustionEngineType x

data InternalCombustionEngineType_ x xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine =
  PistonEngineType_ !(XPistonEngineType x) (PistonEngine_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine)
  | RotaryEngineType_ !(XRotaryEngineType x) (RotaryEngine_ xrotaryengine xrotors xenginedisplacement_rotaryengine)
  | InternalCombustionEngineType_ !(XInternalCombustionEngineType x)
  deriving Generic

deriving instance (Eq (XPistonEngineType x), Eq (PistonEngine_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine), Eq (XRotaryEngineType x), Eq (RotaryEngine_ xrotaryengine xrotors xenginedisplacement_rotaryengine), Eq (XInternalCombustionEngineType x)) =>
  Eq (InternalCombustionEngineType_ x xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

deriving instance (Ord (XPistonEngineType x), Ord (PistonEngine_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine), Ord (XRotaryEngineType x), Ord (RotaryEngine_ xrotaryengine xrotors xenginedisplacement_rotaryengine), Ord (XInternalCombustionEngineType x)) =>
  Ord (InternalCombustionEngineType_ x xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

deriving instance (Show (XPistonEngineType x), Show (PistonEngine_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine), Show (XRotaryEngineType x), Show (RotaryEngine_ xrotaryengine xrotors xenginedisplacement_rotaryengine), Show (XInternalCombustionEngineType x)) =>
  Show (InternalCombustionEngineType_ x xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

class HasInternalCombustionEngineType a e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine | a -> e, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine where
  internalCombustionEngineType ::
    Lens' a (InternalCombustionEngineType_ e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  xInternalCombustionEngineType ::
    Lens' a (XInternalCombustionEngineType e)
  xInternalCombustionEngineType =
    internalCombustionEngineType . xInternalCombustionEngineType

instance HasInternalCombustionEngineType (InternalCombustionEngineType_ e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  internalCombustionEngineType =
    id
 
xInternalCombustionEngineType' ::
  (
    XPistonEngineType e ~ x
  , XRotaryEngineType e ~ x
  , XInternalCombustionEngineType e ~ Void
  ) =>
  Lens' (InternalCombustionEngineType_ e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) x 
xInternalCombustionEngineType' f (PistonEngineType_ x t) =
  fmap (\x' -> PistonEngineType_ x' t) (f x)
xInternalCombustionEngineType' f (RotaryEngineType_ x t) =
  fmap (\x' -> RotaryEngineType_ x' t) (f x)
xInternalCombustionEngineType' _ (InternalCombustionEngineType_ x) =
  absurd x

class AsInternalCombustionEngineType a e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine | a -> e, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine where
  _InternalCombustionEngineType ::
    Prism' a (InternalCombustionEngineType_ e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  _PistonEngineType ::
    Prism' a (XPistonEngineType e, (PistonEngine_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine))
  _PistonEngineType' ::
    XPistonEngineType e ~ () =>
    Prism' a (PistonEngine_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine)
  _PistonEngineType' =
    _PistonEngineType . unproduct
  _RotaryEngineType ::
    Prism' a (XRotaryEngineType e, (RotaryEngine_ xrotaryengine xrotors xenginedisplacement_rotaryengine))
  _RotaryEngineType' ::
    XRotaryEngineType e ~ () =>
    Prism' a (RotaryEngine_ xrotaryengine xrotors xenginedisplacement_rotaryengine)
  _RotaryEngineType' =
    _RotaryEngineType . unproduct
  _XInternalCombustionEngineType ::
    Prism' a (XInternalCombustionEngineType e)

instance AsInternalCombustionEngineType (InternalCombustionEngineType_ e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) e xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  _InternalCombustionEngineType =
    id
  _PistonEngineType =
    prism'
      (\(x, t) -> PistonEngineType_ x t)
      (
        \case
          PistonEngineType_ x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _RotaryEngineType =
    prism'
      (\(x, t) -> RotaryEngineType_ x t)
      (
        \case
          RotaryEngineType_ x t ->
            Just (x, t)
          _ ->
            Nothing
      )
  _XInternalCombustionEngineType =
    prism'
      InternalCombustionEngineType_
      (
        \case
          InternalCombustionEngineType_ x ->
            Just x
          _ ->
            Nothing
      )

type InternalCombustionEngineType =
  InternalCombustionEngineType_ ()

type instance XPistonEngineType () =
  ()
type instance XRotaryEngineType () =
  ()
type instance XInternalCombustionEngineType () =
  Void

pattern PistonEngineType ::
  PistonEngine_ xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine
  -> InternalCombustionEngineType xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
pattern PistonEngineType t <- PistonEngineType_ _ t
  where PistonEngineType t = PistonEngineType_ () t

pattern RotaryEngineType ::
  RotaryEngine_ xrotaryengine xrotors xenginedisplacement_rotaryengine
  -> InternalCombustionEngineType xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
pattern RotaryEngineType t <- RotaryEngineType_ _ t
  where RotaryEngineType t = RotaryEngineType_ () t

pattern InternalCombustionEngineType ::
  Void
  -> InternalCombustionEngineType xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
pattern InternalCombustionEngineType v <- InternalCombustionEngineType_ v
  where InternalCombustionEngineType v = InternalCombustionEngineType_ v

----

instance AsPistonEngine (InternalCombustionEngineType xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine where
  _PistonEngine =
    prism'
      PistonEngineType
      (
        \case
          PistonEngineType t ->
            Just t
          _ ->
            Nothing
      )

instance AsRotaryEngine (InternalCombustionEngineType xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) xrotaryengine xrotors xenginedisplacement_rotaryengine where
  _RotaryEngine =
    prism'
      RotaryEngineType
      (
        \case
          RotaryEngineType t ->
            Just t
          _ ->
            Nothing
      )
