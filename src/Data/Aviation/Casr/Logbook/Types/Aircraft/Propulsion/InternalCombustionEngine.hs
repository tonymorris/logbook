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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction(InternalCombustionEngineAirInduction, HasInternalCombustionEngineAirInduction(internalCombustionEngineAirInduction))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction(InternalCombustionEngineFuelInduction, HasInternalCombustionEngineFuelInduction(internalCombustionEngineFuelInduction))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition(InternalCombustionEngineIgnition, HasInternalCombustionEngineIgnition(internalCombustionEngineIgnition))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType(InternalCombustionEngineType, HasInternalCombustionEngineType(internalCombustionEngineType))
import GHC.Generics(Generic)
import Prelude

type family XInternalCombustionEngine x

data InternalCombustionEngine_ x =
  InternalCombustionEngine_
    !(XInternalCombustionEngine x)
    InternalCombustionEngineAirInduction
    InternalCombustionEngineFuelInduction
    InternalCombustionEngineIgnition
    InternalCombustionEngineType
  deriving Generic

deriving instance Eq (XInternalCombustionEngine x) =>
  Eq (InternalCombustionEngine_ x)

deriving instance Ord (XInternalCombustionEngine x) =>
  Ord (InternalCombustionEngine_ x)

deriving instance Show (XInternalCombustionEngine x) =>
  Show (InternalCombustionEngine_ x)

class HasInternalCombustionEngine a e | a -> e where
  internalCombustionEngine ::
    Lens' a (InternalCombustionEngine_ e)
  xInternalCombustionEngine ::
    Lens' a (XInternalCombustionEngine e)
  default xInternalCombustionEngine ::
    Lens' a (XInternalCombustionEngine e)
  xInternalCombustionEngine =
    internalCombustionEngine . xInternalCombustionEngine

instance HasInternalCombustionEngine (InternalCombustionEngine_ e) e where
  internalCombustionEngine =
    id
  xInternalCombustionEngine f (InternalCombustionEngine_ x a l i t) =
    fmap (\x' -> InternalCombustionEngine_ x' a l i t) (f x)

class AsInternalCombustionEngine a e | a -> e where
  _InternalCombustionEngine ::
    Prism' a (InternalCombustionEngine_ e)

instance AsInternalCombustionEngine (InternalCombustionEngine_ e) e where
  _InternalCombustionEngine =
    id

type InternalCombustionEngine =
  InternalCombustionEngine_ ()

type instance XInternalCombustionEngine () =
  ()

pattern InternalCombustionEngine ::
  InternalCombustionEngineAirInduction
  -> InternalCombustionEngineFuelInduction
  -> InternalCombustionEngineIgnition
  -> InternalCombustionEngineType
  -> InternalCombustionEngine
pattern InternalCombustionEngine a l i t <- InternalCombustionEngine_ _ a l i t
  where InternalCombustionEngine a l i t = InternalCombustionEngine_ () a l i t

----

instance HasInternalCombustionEngineAirInduction (InternalCombustionEngine_ x) () where
  internalCombustionEngineAirInduction f (InternalCombustionEngine_ x a l i t) =
    fmap (\a' -> InternalCombustionEngine_ x a' l i t) (f a)

instance HasInternalCombustionEngineFuelInduction (InternalCombustionEngine_ x) () where
  internalCombustionEngineFuelInduction f (InternalCombustionEngine_ x a l i t) =
    fmap (\l' -> InternalCombustionEngine_ x a l' i t) (f l)

instance HasInternalCombustionEngineIgnition (InternalCombustionEngine_ x) () where
  internalCombustionEngineIgnition f (InternalCombustionEngine_ x a l i t) =
    fmap (\i' -> InternalCombustionEngine_ x a l i' t) (f i)

instance HasInternalCombustionEngineType (InternalCombustionEngine_ x) () where
  internalCombustionEngineType f (InternalCombustionEngine_ x a l i t) =
    fmap (\t' -> InternalCombustionEngine_ x a l i t') (f t)
