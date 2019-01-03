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

data InternalCombustionEngineAirInduction_ x =
  Supercharged_ !(XSupercharged x)
  | Turbocharged_ !(XTurbocharged x)
  | SuperTurbocharged_ !(XSuperTurbocharged x)
  | NaturalInduction_ !(XNaturalInduction x)
  | InternalCombustionEngineAirInduction_ !(XInternalCombustionEngineAirInduction x)
  deriving Generic

deriving instance (Eq (XSupercharged x), Eq (XTurbocharged x), Eq (XSuperTurbocharged x), Eq (XNaturalInduction x), Eq (XInternalCombustionEngineAirInduction x)) =>
  Eq (InternalCombustionEngineAirInduction_ x)

deriving instance (Ord (XSupercharged x), Ord (XTurbocharged x), Ord (XSuperTurbocharged x), Ord (XNaturalInduction x), Ord (XInternalCombustionEngineAirInduction x)) =>
  Ord (InternalCombustionEngineAirInduction_ x)

deriving instance (Show (XSupercharged x), Show (XTurbocharged x), Show (XSuperTurbocharged x), Show (XNaturalInduction x), Show (XInternalCombustionEngineAirInduction x)) =>
  Show (InternalCombustionEngineAirInduction_ x)

class HasInternalCombustionEngineAirInduction a e | a -> e where
  internalCombustionEngineAirInduction ::
    Lens' a (InternalCombustionEngineAirInduction_ e)
  xInternalCombustionEngineAirInduction ::
    (
      XSupercharged e ~ x
    , XTurbocharged e ~ x
    , XSuperTurbocharged e ~ x
    , XNaturalInduction e ~ x
    , XInternalCombustionEngineAirInduction e ~ Void
    ) =>
    Lens' a x
  default xInternalCombustionEngineAirInduction ::
    (

      XSupercharged () ~ x
    , XTurbocharged () ~ x
    , XSuperTurbocharged () ~ x
    , XNaturalInduction () ~ x
    , XInternalCombustionEngineAirInduction e ~ Void
    ) =>
    Lens' a x
  xInternalCombustionEngineAirInduction f a =
    fmap (\() -> a) (f ())

instance HasInternalCombustionEngineAirInduction (InternalCombustionEngineAirInduction_ e) e where
  internalCombustionEngineAirInduction =
    id
  xInternalCombustionEngineAirInduction f (Supercharged_ x) =
    fmap Supercharged_ (f x)
  xInternalCombustionEngineAirInduction f (Turbocharged_ x) =
    fmap Turbocharged_ (f x)
  xInternalCombustionEngineAirInduction f (SuperTurbocharged_ x) =
    fmap SuperTurbocharged_ (f x)
  xInternalCombustionEngineAirInduction f (NaturalInduction_ x) =
    fmap NaturalInduction_ (f x)
  xInternalCombustionEngineAirInduction _ (InternalCombustionEngineAirInduction_ x) =
    absurd x

class AsInternalCombustionEngineAirInduction a e | a -> e where
  _InternalCombustionEngineAirInduction ::
    Prism' a (InternalCombustionEngineAirInduction_ e)
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

instance AsInternalCombustionEngineAirInduction (InternalCombustionEngineAirInduction_ e) e where
  _InternalCombustionEngineAirInduction =
    id
  _XSupercharged =
    prism'
      Supercharged_
      (
        \case
          Supercharged_ x ->
            Just x
          _ ->
            Nothing
      )
  _XTurbocharged =
    prism'
      Turbocharged_
      (
        \case
          Turbocharged_ x ->
            Just x
          _ ->
            Nothing
      )
  _XSuperTurbocharged =
    prism'
      SuperTurbocharged_
      (
        \case
          SuperTurbocharged_ x ->
            Just x
          _ ->
            Nothing
      )
  _XNaturalInduction =
    prism'
      NaturalInduction_
      (
        \case
          NaturalInduction_ x ->
            Just x
          _ ->
            Nothing
      )
  _XInternalCombustionEngineAirInduction =
    prism'
      InternalCombustionEngineAirInduction_
      (
        \case
          InternalCombustionEngineAirInduction_ x ->
            Just x
          _ ->
            Nothing
      )

type InternalCombustionEngineAirInduction =
  InternalCombustionEngineAirInduction_ ()

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

pattern Supercharged ::
  InternalCombustionEngineAirInduction
pattern Supercharged <- Supercharged_ _
  where Supercharged = Supercharged_ ()

pattern Turbocharged ::
  InternalCombustionEngineAirInduction
pattern Turbocharged <- Turbocharged_ _
  where Turbocharged = Turbocharged_ ()

pattern SuperTurbocharged ::
  InternalCombustionEngineAirInduction
pattern SuperTurbocharged <- SuperTurbocharged_ _
  where SuperTurbocharged = SuperTurbocharged_ ()

pattern NaturalInduction ::
  InternalCombustionEngineAirInduction
pattern NaturalInduction <- NaturalInduction_ _
  where NaturalInduction = NaturalInduction_ ()

pattern InternalCombustionEngineAirInduction ::
  Void
  -> InternalCombustionEngineAirInduction
pattern InternalCombustionEngineAirInduction v <- InternalCombustionEngineAirInduction_ v
  where InternalCombustionEngineAirInduction v = InternalCombustionEngineAirInduction_ v
