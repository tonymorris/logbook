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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType where

import Control.Lens(Lens', Prism', prism')
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude
import Util

type family XPistonEngineType x
type family XRotaryEngineType x
type family XInternalCombustionEngineType x

data InternalCombustionEngineType_ x=
  PistonEngineType_ !(XPistonEngineType x) PistonEngine
  | RotaryEngineType_ !(XRotaryEngineType x) RotaryEngine
  | InternalCombustionEngineType_ !(XInternalCombustionEngineType x)
  deriving Generic

deriving instance (Eq (XPistonEngineType x), Eq (XRotaryEngineType x), Eq (XInternalCombustionEngineType x)) =>
  Eq (InternalCombustionEngineType_ x)

deriving instance (Ord (XPistonEngineType x), Ord (XRotaryEngineType x), Ord (XInternalCombustionEngineType x)) =>
  Ord (InternalCombustionEngineType_ x)

deriving instance (Show (XPistonEngineType x), Show (XRotaryEngineType x), Show (XInternalCombustionEngineType x)) =>
  Show (InternalCombustionEngineType_ x)

class HasInternalCombustionEngineType a e | a -> e where
  internalCombustionEngineType ::
    Lens' a (InternalCombustionEngineType_ e)
  xInternalCombustionEngineType ::
    (
      XPistonEngineType e ~ x
    , XRotaryEngineType e ~ x
    , XInternalCombustionEngineType e ~ Void
    ) =>
    Lens' a x
  default xInternalCombustionEngineType ::
    (
      XPistonEngineType () ~ x
    , XRotaryEngineType () ~ x
    , XInternalCombustionEngineType e ~ Void
    ) =>
    Lens' a x
  xInternalCombustionEngineType f a =
    fmap (\() -> a) (f ())

instance HasInternalCombustionEngineType (InternalCombustionEngineType_ e) e where
  internalCombustionEngineType =
    id
  xInternalCombustionEngineType f (PistonEngineType_ x t) =
    fmap (\x' -> PistonEngineType_ x' t) (f x)
  xInternalCombustionEngineType f (RotaryEngineType_ x t) =
    fmap (\x' -> RotaryEngineType_ x' t) (f x)
  xInternalCombustionEngineType _ (InternalCombustionEngineType_ x) =
    absurd x

class AsInternalCombustionEngineType a e | a -> e where
  _InternalCombustionEngineType ::
    Prism' a (InternalCombustionEngineType_ e)
  _PistonEngineType ::
    Prism' a (XPistonEngineType e, PistonEngine)
  _PistonEngineType' ::
    XPistonEngineType e ~ () =>
    Prism' a PistonEngine
  _PistonEngineType' =
    _PistonEngineType . unproduct
  _RotaryEngineType ::
    Prism' a (XRotaryEngineType e, RotaryEngine)
  _RotaryEngineType' ::
    XRotaryEngineType e ~ () =>
    Prism' a RotaryEngine
  _RotaryEngineType' =
    _RotaryEngineType . unproduct
  _XInternalCombustionEngineType ::
    Prism' a (XInternalCombustionEngineType e)

instance AsInternalCombustionEngineType (InternalCombustionEngineType_ e) e where
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
  PistonEngine
  -> InternalCombustionEngineType
pattern PistonEngineType t <- PistonEngineType_ _ t
  where PistonEngineType t = PistonEngineType_ () t

pattern RotaryEngineType ::
  RotaryEngine
  -> InternalCombustionEngineType
pattern RotaryEngineType t <- RotaryEngineType_ _ t
  where RotaryEngineType t = RotaryEngineType_ () t

pattern InternalCombustionEngineType ::
  Void
  -> InternalCombustionEngineType
pattern InternalCombustionEngineType v <- InternalCombustionEngineType_ v
  where InternalCombustionEngineType v = InternalCombustionEngineType_ v
