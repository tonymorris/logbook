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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XSupercharged x
type family XTurbocharged x
type family XSuperTurbocharged x
type family XNaturalInduction x
type family XInternalCombustionEngineAirInduction x

data InternalCombustionEngineAirInduction x =
  Supercharged !(XSupercharged x)
  | Turbocharged !(XTurbocharged x)
  | SuperTurbocharged !(XSuperTurbocharged x)
  | NaturalInduction !(XNaturalInduction x)
  | InternalCombustionEngineAirInduction !(XInternalCombustionEngineAirInduction x)
  deriving Generic

deriving instance (Eq (XSupercharged x), Eq (XTurbocharged x), Eq (XSuperTurbocharged x), Eq (XNaturalInduction x), Eq (XInternalCombustionEngineAirInduction x)) =>
  Eq (InternalCombustionEngineAirInduction x)

deriving instance (Ord (XSupercharged x), Ord (XTurbocharged x), Ord (XSuperTurbocharged x), Ord (XNaturalInduction x), Ord (XInternalCombustionEngineAirInduction x)) =>
  Ord (InternalCombustionEngineAirInduction x)

deriving instance (Show (XSupercharged x), Show (XTurbocharged x), Show (XSuperTurbocharged x), Show (XNaturalInduction x), Show (XInternalCombustionEngineAirInduction x)) =>
  Show (InternalCombustionEngineAirInduction x)

class HasInternalCombustionEngineAirInduction a e | a -> e where
  internalCombustionEngineAirInduction ::
    Lens' a (InternalCombustionEngineAirInduction e)
  xInternalCombustionEngineAirInduction ::
    Lens' a (XInternalCombustionEngineAirInduction e)
  xInternalCombustionEngineAirInduction =
    internalCombustionEngineAirInduction . xInternalCombustionEngineAirInduction

instance HasInternalCombustionEngineAirInduction (InternalCombustionEngineAirInduction e) e where
  internalCombustionEngineAirInduction =
    id

xInternalCombustionEngineAirInduction' ::
  (
    XSupercharged e ~ x
  , XTurbocharged e ~ x
  , XSuperTurbocharged e ~ x
  , XNaturalInduction e ~ x
  , XInternalCombustionEngineAirInduction e ~ Void
  ) =>
  Lens' (InternalCombustionEngineAirInduction e) x
xInternalCombustionEngineAirInduction' f (Supercharged x) =
  fmap Supercharged (f x)
xInternalCombustionEngineAirInduction' f (Turbocharged x) =
  fmap Turbocharged (f x)
xInternalCombustionEngineAirInduction' f (SuperTurbocharged x) =
  fmap SuperTurbocharged (f x)
xInternalCombustionEngineAirInduction' f (NaturalInduction x) =
  fmap NaturalInduction (f x)
xInternalCombustionEngineAirInduction' _ (InternalCombustionEngineAirInduction x) =
  absurd x

class AsInternalCombustionEngineAirInduction a e | a -> e where
  _InternalCombustionEngineAirInduction ::
    Prism' a (InternalCombustionEngineAirInduction e)
  _XSupercharged ::
    Prism' a (XSupercharged e)
  _XTurbocharged ::
    Prism' a (XTurbocharged e)
  _XSuperTurbocharged ::
    Prism' a (XSuperTurbocharged e)
  _XNaturalInduction ::
    Prism' a (XNaturalInduction e)
  _XInternalCombustionEngineAirInduction ::
    Prism' a (XInternalCombustionEngineAirInduction e)

instance AsInternalCombustionEngineAirInduction (InternalCombustionEngineAirInduction e) e where
  _InternalCombustionEngineAirInduction =
    id
  _XSupercharged =
    prism'
      Supercharged
      (
        \case
          Supercharged x ->
            Just x
          _ ->
            Nothing
      )
  _XTurbocharged =
    prism'
      Turbocharged
      (
        \case
          Turbocharged x ->
            Just x
          _ ->
            Nothing
      )
  _XSuperTurbocharged =
    prism'
      SuperTurbocharged
      (
        \case
          SuperTurbocharged x ->
            Just x
          _ ->
            Nothing
      )
  _XNaturalInduction =
    prism'
      NaturalInduction
      (
        \case
          NaturalInduction x ->
            Just x
          _ ->
            Nothing
      )
  _XInternalCombustionEngineAirInduction =
    prism'
      InternalCombustionEngineAirInduction
      (
        \case
          InternalCombustionEngineAirInduction x ->
            Just x
          _ ->
            Nothing
      )

type InternalCombustionEngineAirInduction_ =
  InternalCombustionEngineAirInduction ()

type instance XSupercharged () =
  ()
type instance XTurbocharged () =
  ()
type instance XSuperTurbocharged () =
  ()
type instance XNaturalInduction () =
  ()
type instance XInternalCombustionEngineAirInduction () =
  Void

pattern Supercharged_ ::
  InternalCombustionEngineAirInduction_
pattern Supercharged_ <- Supercharged _
  where Supercharged_ = Supercharged ()

pattern Turbocharged_ ::
  InternalCombustionEngineAirInduction_
pattern Turbocharged_ <- Turbocharged _
  where Turbocharged_ = Turbocharged ()

pattern SuperTurbocharged_ ::
  InternalCombustionEngineAirInduction_
pattern SuperTurbocharged_ <- SuperTurbocharged _
  where SuperTurbocharged_ = SuperTurbocharged ()

pattern NaturalInduction_ ::
  InternalCombustionEngineAirInduction_
pattern NaturalInduction_ <- NaturalInduction _
  where NaturalInduction_ = NaturalInduction ()

pattern InternalCombustionEngineAirInduction_ ::
  Void
  -> InternalCombustionEngineAirInduction_
pattern InternalCombustionEngineAirInduction_ v <- InternalCombustionEngineAirInduction v
  where InternalCombustionEngineAirInduction_ v = InternalCombustionEngineAirInduction v
