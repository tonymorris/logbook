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

data PistonEngine_ x xpistonengineconfiguration xpistonenginecycle xenginedisplacement =
  PistonEngine_
    !(XPistonEngine x)
    (PistonEngineConfiguration_ xpistonengineconfiguration)
    (PistonEngineCycle_ xpistonenginecycle)
    Cylinders
    (EngineDisplacement_ xenginedisplacement)
    deriving Generic
    
deriving instance (Eq (XPistonEngine x), Eq (PistonEngineConfiguration_ xpistonengineconfiguration), Eq (PistonEngineCycle_ xpistonenginecycle), Eq (XEngineDisplacement xenginedisplacement)) =>
  Eq (PistonEngine_ x xpistonengineconfiguration xpistonenginecycle xenginedisplacement)

deriving instance (Ord (XPistonEngine x), Ord (PistonEngineConfiguration_ xpistonengineconfiguration), Ord (PistonEngineCycle_ xpistonenginecycle), Ord (XEngineDisplacement xenginedisplacement)) =>
  Ord (PistonEngine_ x xpistonengineconfiguration xpistonenginecycle xenginedisplacement)

deriving instance (Show (XPistonEngine x), Show (PistonEngineConfiguration_ xpistonengineconfiguration), Show (PistonEngineCycle_ xpistonenginecycle), Show (XEngineDisplacement xenginedisplacement)) =>
  Show (PistonEngine_ x xpistonengineconfiguration xpistonenginecycle xenginedisplacement)

class HasPistonEngine a e xpistonengineconfiguration xpistonenginecycle xenginedisplacement | a -> e, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement where
  pistonEngine ::
    Lens' a (PistonEngine_ e xpistonengineconfiguration xpistonenginecycle xenginedisplacement)
  xPistonEngine ::
    Lens' a (XPistonEngine e)
  xPistonEngine =
    pistonEngine . xPistonEngine

instance HasPistonEngine (PistonEngine_ e xpistonengineconfiguration xpistonenginecycle xenginedisplacement) e xpistonengineconfiguration xpistonenginecycle xenginedisplacement where
  pistonEngine =
    id
  xPistonEngine f (PistonEngine_ x c y l d) =
    fmap (\x' -> PistonEngine_ x' c y l d) (f x)

class AsPistonEngine a e xpistonengineconfiguration xpistonenginecycle xenginedisplacement | a -> e, a ->xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement where
  _PistonEngine ::
    Prism' a (PistonEngine_ e xpistonengineconfiguration xpistonenginecycle xenginedisplacement)

instance AsPistonEngine (PistonEngine_ e xpistonengineconfiguration xpistonenginecycle xenginedisplacement) e xpistonengineconfiguration xpistonenginecycle xenginedisplacement where
  _PistonEngine =
    id

type PistonEngine =
  PistonEngine_ ()

type instance XPistonEngine () =
  ()

pattern PistonEngine ::
  PistonEngineConfiguration_ xpistonengineconfiguration
  -> PistonEngineCycle_ xpistonenginecycle
  -> Cylinders
  -> EngineDisplacement_ xenginedisplacement
  -> PistonEngine xpistonengineconfiguration xpistonenginecycle xenginedisplacement
pattern PistonEngine c y l d <- PistonEngine_ _ c y l d
  where PistonEngine c y l d = PistonEngine_ () c y l d

----

instance HasPistonEngineConfiguration (PistonEngine_ x xpistonengineconfiguration xpistonenginecycle xenginedisplacement) xpistonengineconfiguration where
  pistonEngineConfiguration f (PistonEngine_ x c y l d) =
    fmap (\c' -> PistonEngine_ x c' y l d) (f c)

instance HasPistonEngineCycle (PistonEngine_ x xpistonengineconfiguration xpistonenginecycle xenginedisplacement) xpistonenginecycle where
  pistonEngineCycle f (PistonEngine_ x c y l d) =
    fmap (\y' -> PistonEngine_ x c y' l d) (f y)

instance HasPositive (PistonEngine_ x xpistonengineconfiguration xpistonenginecycle xenginedisplacement) where
  positive f (PistonEngine_ x c y l d) =
    fmap (\l' -> PistonEngine_ x c y l' d) (f l)

instance HasEngineDisplacement (PistonEngine_ x xpistonengineconfiguration xpistonenginecycle xenginedisplacement) xenginedisplacement where
  engineDisplacement f (PistonEngine_ x c y l d) =
    fmap (\d' -> PistonEngine_ x c y l d') (f d)
