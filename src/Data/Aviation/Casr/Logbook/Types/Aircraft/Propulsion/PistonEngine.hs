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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineConfiguration
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle
import GHC.Generics
import Natural
import Prelude

type family XPistonEngine x

type Cylinders =
  Positive

data PistonEngine x xpistonengineconfiguration xpistonenginecycle xenginedisplacement =
  PistonEngine
    !(XPistonEngine x)
    (PistonEngineConfiguration xpistonengineconfiguration)
    (PistonEngineCycle xpistonenginecycle)
    Cylinders
    (EngineDisplacement xenginedisplacement)
    deriving Generic
    
deriving instance (Eq (XPistonEngine x), Eq (PistonEngineConfiguration xpistonengineconfiguration), Eq (PistonEngineCycle xpistonenginecycle), Eq (XEngineDisplacement xenginedisplacement)) =>
  Eq (PistonEngine x xpistonengineconfiguration xpistonenginecycle xenginedisplacement)

deriving instance (Ord (XPistonEngine x), Ord (PistonEngineConfiguration xpistonengineconfiguration), Ord (PistonEngineCycle xpistonenginecycle), Ord (XEngineDisplacement xenginedisplacement)) =>
  Ord (PistonEngine x xpistonengineconfiguration xpistonenginecycle xenginedisplacement)

deriving instance (Show (XPistonEngine x), Show (PistonEngineConfiguration xpistonengineconfiguration), Show (PistonEngineCycle xpistonenginecycle), Show (XEngineDisplacement xenginedisplacement)) =>
  Show (PistonEngine x xpistonengineconfiguration xpistonenginecycle xenginedisplacement)

class HasPistonEngine a e xpistonengineconfiguration xpistonenginecycle xenginedisplacement | a -> e, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement where
  pistonEngine ::
    Lens' a (PistonEngine e xpistonengineconfiguration xpistonenginecycle xenginedisplacement)
  xPistonEngine ::
    Lens' a (XPistonEngine e)
  xPistonEngine =
    pistonEngine . xPistonEngine

instance HasPistonEngine (PistonEngine e xpistonengineconfiguration xpistonenginecycle xenginedisplacement) e xpistonengineconfiguration xpistonenginecycle xenginedisplacement where
  pistonEngine =
    id
  xPistonEngine f (PistonEngine x c y l d) =
    fmap (\x' -> PistonEngine x' c y l d) (f x)

class AsPistonEngine a e xpistonengineconfiguration xpistonenginecycle xenginedisplacement | a -> e, a ->xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement where
  _PistonEngine ::
    Prism' a (PistonEngine e xpistonengineconfiguration xpistonenginecycle xenginedisplacement)

instance AsPistonEngine (PistonEngine e xpistonengineconfiguration xpistonenginecycle xenginedisplacement) e xpistonengineconfiguration xpistonenginecycle xenginedisplacement where
  _PistonEngine =
    id

type PistonEngine_ =
  PistonEngine ()

type instance XPistonEngine () =
  ()

pattern PistonEngine_ ::
  PistonEngineConfiguration xpistonengineconfiguration
  -> PistonEngineCycle xpistonenginecycle
  -> Cylinders
  -> EngineDisplacement xenginedisplacement
  -> PistonEngine_ xpistonengineconfiguration xpistonenginecycle xenginedisplacement
pattern PistonEngine_ c y l d <- PistonEngine _ c y l d
  where PistonEngine_ c y l d = PistonEngine () c y l d

----

instance HasPistonEngineConfiguration (PistonEngine x xpistonengineconfiguration xpistonenginecycle xenginedisplacement) xpistonengineconfiguration where
  pistonEngineConfiguration f (PistonEngine x c y l d) =
    fmap (\c' -> PistonEngine x c' y l d) (f c)

instance HasPistonEngineCycle (PistonEngine x xpistonengineconfiguration xpistonenginecycle xenginedisplacement) xpistonenginecycle where
  pistonEngineCycle f (PistonEngine x c y l d) =
    fmap (\y' -> PistonEngine x c y' l d) (f y)

instance HasPositive (PistonEngine x xpistonengineconfiguration xpistonenginecycle xenginedisplacement) where
  positive f (PistonEngine x c y l d) =
    fmap (\l' -> PistonEngine x c y l' d) (f l)

instance HasEngineDisplacement (PistonEngine x xpistonengineconfiguration xpistonenginecycle xenginedisplacement) xenginedisplacement where
  engineDisplacement f (PistonEngine x c y l d) =
    fmap (\d' -> PistonEngine x c y l d') (f d)
