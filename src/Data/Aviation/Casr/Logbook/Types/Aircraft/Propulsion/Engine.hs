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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Engine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineType(EngineType, HasEngineType(engineType))
import GHC.Generics(Generic)
import Prelude

type Manufacturer =
  String

type Designation =
  String

type family XEngine x

data Engine x xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype =
  Engine
    !(XEngine x)
    Manufacturer
    Designation
    (EngineType xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)
  deriving Generic

deriving instance (Eq (XEngine x), Eq Manufacturer, Eq Designation, Eq (EngineType xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)) =>
  Eq (Engine x xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

deriving instance (Ord (XEngine x), Ord Manufacturer, Ord Designation, Ord (EngineType xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)) =>
  Ord (Engine x xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

deriving instance (Show (XEngine x), Show Manufacturer, Show Designation, Show (EngineType xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)) =>
  Show (Engine x xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

class HasEngine a e xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype | a -> e, a ->xengine, a -> xinternalcombustionengine, a -> xinternalcombustionengineairinduction, a -> xinternalcombustionenginefuelinduction, a -> xinternalcombustionengineignition, a -> xinternalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine, a -> xelectrictype, a -> xjettype where
  engine ::
    Lens' a (Engine e xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)
  xEngine ::
    Lens' a (XEngine e)
  xEngine =
    engine . xEngine

instance HasEngine (Engine e xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) e xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype where
  engine =
    id
  xEngine f (Engine x m d t) =
    fmap (\x' -> Engine x' m d t) (f x)

class AsEngine a e xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype | a -> e, a -> xengine, a -> xinternalcombustionengine, a -> xinternalcombustionengineairinduction, a -> xinternalcombustionenginefuelinduction, a -> xinternalcombustionengineignition, a -> xinternalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine, a -> xelectrictype, a -> xjettype where
  _Engine ::
    Prism' a (Engine e xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype)

instance AsEngine (Engine e xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) e xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype where
  _Engine =
    id

type Engine_ =
  Engine ()

type instance XEngine () =
  ()

pattern Engine_ ::
  Manufacturer
  -> Designation
  -> EngineType xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
  -> Engine_ xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype
pattern Engine_ m d t <- Engine _ m d t
  where Engine_ m d t = Engine () m d t

----

instance HasEngineType (Engine x xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype) xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype where
  engineType f (Engine x m d t) =
    fmap (\t' -> Engine x m d t') (f t) 
