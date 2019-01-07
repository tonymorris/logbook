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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XCarburettor x
type family XFuelInjection x
type family XInternalCombustionEngineFuelInduction x

data InternalCombustionEngineFuelInduction x =
  Carburettor !(XCarburettor x)
  | FuelInjection !(XFuelInjection x)
  | InternalCombustionEngineFuelInduction !(XInternalCombustionEngineFuelInduction x)
  deriving Generic

deriving instance (Eq (XCarburettor x), Eq (XFuelInjection x), Eq (XInternalCombustionEngineFuelInduction x)) =>
  Eq (InternalCombustionEngineFuelInduction x)

deriving instance (Ord (XCarburettor x), Ord (XFuelInjection x), Ord (XInternalCombustionEngineFuelInduction x)) =>
  Ord (InternalCombustionEngineFuelInduction x)

deriving instance (Show (XCarburettor x), Show (XFuelInjection x), Show (XInternalCombustionEngineFuelInduction x)) =>
  Show (InternalCombustionEngineFuelInduction x)

class HasInternalCombustionEngineFuelInduction a e | a -> e where
  internalCombustionEngineFuelInduction ::
    Lens' a (InternalCombustionEngineFuelInduction e)
  xInternalCombustionEngineFuelInduction ::
    Lens' a (XInternalCombustionEngineFuelInduction e)
  xInternalCombustionEngineFuelInduction =
    internalCombustionEngineFuelInduction . xInternalCombustionEngineFuelInduction

instance HasInternalCombustionEngineFuelInduction (InternalCombustionEngineFuelInduction e) e where
  internalCombustionEngineFuelInduction =
    id

xInternalCombustionEngineFuelInduction' ::
  (
    XCarburettor e ~ x
  , XFuelInjection e ~ x
  , XInternalCombustionEngineFuelInduction e ~ Void
  ) =>
  Lens' (InternalCombustionEngineFuelInduction e) x 
xInternalCombustionEngineFuelInduction' f (Carburettor x) =
  fmap Carburettor (f x)
xInternalCombustionEngineFuelInduction' f (FuelInjection x) =
  fmap FuelInjection (f x)
xInternalCombustionEngineFuelInduction' _ (InternalCombustionEngineFuelInduction x) =
  absurd x

class AsInternalCombustionEngineFuelInduction a e | a -> e where
  _InternalCombustionEngineFuelInduction ::
    Prism' a (InternalCombustionEngineFuelInduction e)
  _XCarburettor ::
    Prism' a (XCarburettor e)
  _XFuelInjection ::
    Prism' a (XFuelInjection e)
  _XInternalCombustionEngineFuelInduction ::
    Prism' a (XInternalCombustionEngineFuelInduction e)

instance AsInternalCombustionEngineFuelInduction (InternalCombustionEngineFuelInduction e) e where
  _InternalCombustionEngineFuelInduction =
    id
  _XCarburettor =
    prism'
      Carburettor
      (
        \case
          Carburettor x ->
            Just x
          _ ->
            Nothing
      )
  _XFuelInjection =
    prism'
      FuelInjection
      (
        \case
          FuelInjection x ->
            Just x
          _ ->
            Nothing
      )
  _XInternalCombustionEngineFuelInduction =
    prism'
      InternalCombustionEngineFuelInduction
      (
        \case
          InternalCombustionEngineFuelInduction x ->
            Just x
          _ ->
            Nothing
      )

type InternalCombustionEngineFuelInduction_ =
  InternalCombustionEngineFuelInduction ()

type instance XCarburettor () =
  ()
type instance XFuelInjection () =
  ()
type instance XInternalCombustionEngineFuelInduction () =
  Void

pattern Carburettor_ ::
  InternalCombustionEngineFuelInduction_
pattern Carburettor_ <- Carburettor _
  where Carburettor_ = Carburettor ()

pattern FuelInjection_ ::
  InternalCombustionEngineFuelInduction_
pattern FuelInjection_ <- FuelInjection _
  where FuelInjection_ = FuelInjection ()

pattern InternalCombustionEngineFuelInduction_ ::
  Void
  -> InternalCombustionEngineFuelInduction_
pattern InternalCombustionEngineFuelInduction_ v <- InternalCombustionEngineFuelInduction v
  where InternalCombustionEngineFuelInduction_ v = InternalCombustionEngineFuelInduction v
