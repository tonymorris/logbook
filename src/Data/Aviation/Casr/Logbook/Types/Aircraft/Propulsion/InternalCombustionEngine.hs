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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction( InternalCombustionEngineAirInduction, HasInternalCombustionEngineAirInduction(internalCombustionEngineAirInduction))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction(InternalCombustionEngineFuelInduction, HasInternalCombustionEngineFuelInduction(internalCombustionEngineFuelInduction))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition(InternalCombustionEngineIgnition, HasInternalCombustionEngineIgnition(internalCombustionEngineIgnition))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType(InternalCombustionEngineType, HasInternalCombustionEngineType(internalCombustionEngineType))
import GHC.Generics(Generic)
import Prelude

type family XInternalCombustionEngine x

data InternalCombustionEngine x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine =
  InternalCombustionEngine
    !(XInternalCombustionEngine x)
    (InternalCombustionEngineAirInduction internalcombustionengineairinduction)
    (InternalCombustionEngineFuelInduction internalcombustionenginefuelinduction)
    (InternalCombustionEngineIgnition internalcombustionengineignition)
    (InternalCombustionEngineType internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  deriving Generic

deriving instance (Eq (XInternalCombustionEngine x), Eq (InternalCombustionEngineAirInduction internalcombustionengineairinduction), Eq (InternalCombustionEngineFuelInduction internalcombustionenginefuelinduction), Eq (InternalCombustionEngineIgnition internalcombustionengineignition), Eq (InternalCombustionEngineType internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)) =>
  Eq (InternalCombustionEngine x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

deriving instance (Ord (XInternalCombustionEngine x), Ord (InternalCombustionEngineAirInduction internalcombustionengineairinduction), Ord (InternalCombustionEngineFuelInduction internalcombustionenginefuelinduction), Ord (InternalCombustionEngineIgnition internalcombustionengineignition), Ord (InternalCombustionEngineType internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)) =>
  Ord (InternalCombustionEngine x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

deriving instance (Show (XInternalCombustionEngine x), Show (InternalCombustionEngineAirInduction internalcombustionengineairinduction), Show (InternalCombustionEngineFuelInduction internalcombustionenginefuelinduction), Show (InternalCombustionEngineIgnition internalcombustionengineignition), Show (InternalCombustionEngineType internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)) =>
  Show (InternalCombustionEngine x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

class HasInternalCombustionEngine a e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine | a -> e, a -> internalcombustionengineairinduction, a -> internalcombustionenginefuelinduction, a -> internalcombustionengineignition, a -> internalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine where
  internalCombustionEngine ::
    Lens' a (InternalCombustionEngine e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  xInternalCombustionEngine ::
    Lens' a (XInternalCombustionEngine e)
  xInternalCombustionEngine =
    internalCombustionEngine . xInternalCombustionEngine

instance HasInternalCombustionEngine (InternalCombustionEngine e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  internalCombustionEngine =
    id
  xInternalCombustionEngine f (InternalCombustionEngine x a l i t) =
    fmap (\x' -> InternalCombustionEngine x' a l i t) (f x)

class AsInternalCombustionEngine a e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine | a -> e, a -> internalcombustionengineairinduction, a -> internalcombustionenginefuelinduction, a ->  internalcombustionengineignition, a -> internalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine where
  _InternalCombustionEngine ::
    Prism' a (InternalCombustionEngine e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

instance AsInternalCombustionEngine (InternalCombustionEngine e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  _InternalCombustionEngine =
    id

type InternalCombustionEngine_ =
  InternalCombustionEngine ()

type instance XInternalCombustionEngine () =
  ()

pattern InternalCombustionEngine_ ::
  InternalCombustionEngineAirInduction internalcombustionengineairinduction
  -> InternalCombustionEngineFuelInduction internalcombustionenginefuelinduction
  -> InternalCombustionEngineIgnition internalcombustionengineignition
  -> InternalCombustionEngineType internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
  -> InternalCombustionEngine_ internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
pattern InternalCombustionEngine_ a l i t <- InternalCombustionEngine _ a l i t
  where InternalCombustionEngine_ a l i t = InternalCombustionEngine () a l i t

----

instance HasInternalCombustionEngineAirInduction (InternalCombustionEngine x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) internalcombustionengineairinduction where
  internalCombustionEngineAirInduction f (InternalCombustionEngine x a l i t) =
    fmap (\a' -> InternalCombustionEngine x a' l i t) (f a)

instance HasInternalCombustionEngineFuelInduction (InternalCombustionEngine x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) internalcombustionenginefuelinduction where
  internalCombustionEngineFuelInduction f (InternalCombustionEngine x a l i t) =
    fmap (\l' -> InternalCombustionEngine x a l' i t) (f l)

instance HasInternalCombustionEngineIgnition (InternalCombustionEngine x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) internalcombustionengineignition where
  internalCombustionEngineIgnition f (InternalCombustionEngine x a l i t) =
    fmap (\i' -> InternalCombustionEngine x a l i' t) (f i)

instance HasInternalCombustionEngineType (InternalCombustionEngine x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  internalCombustionEngineType f (InternalCombustionEngine x a l i t) =
    fmap (\t' -> InternalCombustionEngine x a l i t') (f t)
