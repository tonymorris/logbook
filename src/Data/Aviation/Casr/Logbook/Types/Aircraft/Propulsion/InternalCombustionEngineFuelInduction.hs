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

data InternalCombustionEngineFuelInduction_ x =
  Carburettor_ !(XCarburettor x)
  | FuelInjection_ !(XFuelInjection x)
  | InternalCombustionEngineFuelInduction_ !(XInternalCombustionEngineFuelInduction x)
  deriving Generic

deriving instance (Eq (XCarburettor x), Eq (XFuelInjection x), Eq (XInternalCombustionEngineFuelInduction x)) =>
  Eq (InternalCombustionEngineFuelInduction_ x)

deriving instance (Ord (XCarburettor x), Ord (XFuelInjection x), Ord (XInternalCombustionEngineFuelInduction x)) =>
  Ord (InternalCombustionEngineFuelInduction_ x)

deriving instance (Show (XCarburettor x), Show (XFuelInjection x), Show (XInternalCombustionEngineFuelInduction x)) =>
  Show (InternalCombustionEngineFuelInduction_ x)

class HasInternalCombustionEngineFuelInduction a e | a -> e where
  internalCombustionEngineFuelInduction ::
    Lens' a (InternalCombustionEngineFuelInduction_ e)
  xInternalCombustionEngineFuelInduction ::
    Lens' a (XInternalCombustionEngineFuelInduction e)
  xInternalCombustionEngineFuelInduction =
    internalCombustionEngineFuelInduction . xInternalCombustionEngineFuelInduction

instance HasInternalCombustionEngineFuelInduction (InternalCombustionEngineFuelInduction_ e) e where
  internalCombustionEngineFuelInduction =
    id

xInternalCombustionEngineFuelInduction' ::
  (
    XCarburettor e ~ x
  , XFuelInjection e ~ x
  , XInternalCombustionEngineFuelInduction e ~ Void
  ) =>
  Lens' (InternalCombustionEngineFuelInduction_ e) x 
xInternalCombustionEngineFuelInduction' f (Carburettor_ x) =
  fmap Carburettor_ (f x)
xInternalCombustionEngineFuelInduction' f (FuelInjection_ x) =
  fmap FuelInjection_ (f x)
xInternalCombustionEngineFuelInduction' _ (InternalCombustionEngineFuelInduction_ x) =
  absurd x

class AsInternalCombustionEngineFuelInduction a e | a -> e where
  _InternalCombustionEngineFuelInduction ::
    Prism' a (InternalCombustionEngineFuelInduction_ e)
  _XCarburettor ::
    Prism' a (XCarburettor e)
  _XFuelInjection ::
    Prism' a (XFuelInjection e)
  _XInternalCombustionEngineFuelInduction ::
    Prism' a (XInternalCombustionEngineFuelInduction e)

instance AsInternalCombustionEngineFuelInduction (InternalCombustionEngineFuelInduction_ e) e where
  _InternalCombustionEngineFuelInduction =
    id
  _XCarburettor =
    prism'
      Carburettor_
      (
        \case
          Carburettor_ x ->
            Just x
          _ ->
            Nothing
      )
  _XFuelInjection =
    prism'
      FuelInjection_
      (
        \case
          FuelInjection_ x ->
            Just x
          _ ->
            Nothing
      )
  _XInternalCombustionEngineFuelInduction =
    prism'
      InternalCombustionEngineFuelInduction_
      (
        \case
          InternalCombustionEngineFuelInduction_ x ->
            Just x
          _ ->
            Nothing
      )

type InternalCombustionEngineFuelInduction =
  InternalCombustionEngineFuelInduction_ ()

type instance XCarburettor () =
  ()
type instance XFuelInjection () =
  ()
type instance XInternalCombustionEngineFuelInduction () =
  Void

pattern Carburettor ::
  InternalCombustionEngineFuelInduction
pattern Carburettor <- Carburettor_ _
  where Carburettor = Carburettor_ ()

pattern FuelInjection ::
  InternalCombustionEngineFuelInduction
pattern FuelInjection <- FuelInjection_ _
  where FuelInjection = FuelInjection_ ()

pattern InternalCombustionEngineFuelInduction ::
  Void
  -> InternalCombustionEngineFuelInduction
pattern InternalCombustionEngineFuelInduction v <- InternalCombustionEngineFuelInduction_ v
  where InternalCombustionEngineFuelInduction v = InternalCombustionEngineFuelInduction_ v
