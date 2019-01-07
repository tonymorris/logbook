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
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction( InternalCombustionEngineAirInduction_, HasInternalCombustionEngineAirInduction(internalCombustionEngineAirInduction))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction(InternalCombustionEngineFuelInduction_, HasInternalCombustionEngineFuelInduction(internalCombustionEngineFuelInduction))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition(InternalCombustionEngineIgnition_, HasInternalCombustionEngineIgnition(internalCombustionEngineIgnition))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType(InternalCombustionEngineType_, HasInternalCombustionEngineType(internalCombustionEngineType))
import GHC.Generics(Generic)
import Prelude

type family XInternalCombustionEngine x

data InternalCombustionEngine_ x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine =
  InternalCombustionEngine_
    !(XInternalCombustionEngine x)
    (InternalCombustionEngineAirInduction_ internalcombustionengineairinduction)
    (InternalCombustionEngineFuelInduction_ internalcombustionenginefuelinduction)
    (InternalCombustionEngineIgnition_ internalcombustionengineignition)
    (InternalCombustionEngineType_ internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  deriving Generic

deriving instance (Eq (XInternalCombustionEngine x), Eq (InternalCombustionEngineAirInduction_ internalcombustionengineairinduction), Eq (InternalCombustionEngineFuelInduction_ internalcombustionenginefuelinduction), Eq (InternalCombustionEngineIgnition_ internalcombustionengineignition), Eq (InternalCombustionEngineType_ internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)) =>
  Eq (InternalCombustionEngine_ x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

deriving instance (Ord (XInternalCombustionEngine x), Ord (InternalCombustionEngineAirInduction_ internalcombustionengineairinduction), Ord (InternalCombustionEngineFuelInduction_ internalcombustionenginefuelinduction), Ord (InternalCombustionEngineIgnition_ internalcombustionengineignition), Ord (InternalCombustionEngineType_ internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)) =>
  Ord (InternalCombustionEngine_ x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

deriving instance (Show (XInternalCombustionEngine x), Show (InternalCombustionEngineAirInduction_ internalcombustionengineairinduction), Show (InternalCombustionEngineFuelInduction_ internalcombustionenginefuelinduction), Show (InternalCombustionEngineIgnition_ internalcombustionengineignition), Show (InternalCombustionEngineType_ internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)) =>
  Show (InternalCombustionEngine_ x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

class HasInternalCombustionEngine a e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine | a -> e, a -> internalcombustionengineairinduction, a -> internalcombustionenginefuelinduction, a -> internalcombustionengineignition, a -> internalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine where
  internalCombustionEngine ::
    Lens' a (InternalCombustionEngine_ e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)
  xInternalCombustionEngine ::
    Lens' a (XInternalCombustionEngine e)
  xInternalCombustionEngine =
    internalCombustionEngine . xInternalCombustionEngine

instance HasInternalCombustionEngine (InternalCombustionEngine_ e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  internalCombustionEngine =
    id
  xInternalCombustionEngine f (InternalCombustionEngine_ x a l i t) =
    fmap (\x' -> InternalCombustionEngine_ x' a l i t) (f x)

class AsInternalCombustionEngine a e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine | a -> e, a -> internalcombustionengineairinduction, a -> internalcombustionenginefuelinduction, a ->  internalcombustionengineignition, a -> internalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine where
  _InternalCombustionEngine ::
    Prism' a (InternalCombustionEngine_ e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine)

instance AsInternalCombustionEngine (InternalCombustionEngine_ e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) e internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  _InternalCombustionEngine =
    id

type InternalCombustionEngine =
  InternalCombustionEngine_ ()

type instance XInternalCombustionEngine () =
  ()

pattern InternalCombustionEngine ::
  InternalCombustionEngineAirInduction_ internalcombustionengineairinduction
  -> InternalCombustionEngineFuelInduction_ internalcombustionenginefuelinduction
  -> InternalCombustionEngineIgnition_ internalcombustionengineignition
  -> InternalCombustionEngineType_ internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
  -> InternalCombustionEngine internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine
pattern InternalCombustionEngine a l i t <- InternalCombustionEngine_ _ a l i t
  where InternalCombustionEngine a l i t = InternalCombustionEngine_ () a l i t

----

instance HasInternalCombustionEngineAirInduction (InternalCombustionEngine_ x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) internalcombustionengineairinduction where
  internalCombustionEngineAirInduction f (InternalCombustionEngine_ x a l i t) =
    fmap (\a' -> InternalCombustionEngine_ x a' l i t) (f a)

instance HasInternalCombustionEngineFuelInduction (InternalCombustionEngine_ x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) internalcombustionenginefuelinduction where
  internalCombustionEngineFuelInduction f (InternalCombustionEngine_ x a l i t) =
    fmap (\l' -> InternalCombustionEngine_ x a l' i t) (f l)

instance HasInternalCombustionEngineIgnition (InternalCombustionEngine_ x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) internalcombustionengineignition where
  internalCombustionEngineIgnition f (InternalCombustionEngine_ x a l i t) =
    fmap (\i' -> InternalCombustionEngine_ x a l i' t) (f i)

instance HasInternalCombustionEngineType (InternalCombustionEngine_ x internalcombustionengineairinduction internalcombustionenginefuelinduction internalcombustionengineignition internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine) internalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine where
  internalCombustionEngineType f (InternalCombustionEngine_ x a l i t) =
    fmap (\t' -> InternalCombustionEngine_ x a l i t') (f t)
