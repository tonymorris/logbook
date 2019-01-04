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

data PistonEngine_ x =
  PistonEngine_
    !(XPistonEngine x)
    PistonEngineConfiguration
    PistonEngineCycle
    Cylinders
    EngineDisplacement
    deriving Generic
    
deriving instance Eq (XPistonEngine x) =>
  Eq (PistonEngine_ x)

deriving instance Ord (XPistonEngine x) =>
  Ord (PistonEngine_ x)

deriving instance Show (XPistonEngine x) =>
  Show (PistonEngine_ x)

class HasPistonEngine a e | a -> e where
  pistonEngine ::
    Lens' a (PistonEngine_ e)
  xPistonEngine ::
    XPistonEngine e ~ x =>
    Lens' a x
  default xPistonEngine ::
    XPistonEngine () ~ x =>
    Lens' a x
  xPistonEngine f a =
    fmap (\() -> a) (f ())

instance HasPistonEngine (PistonEngine_ e) e where
  pistonEngine =
    id
  xPistonEngine f (PistonEngine_ x c y l d) =
    fmap (\x' -> PistonEngine_ x' c y l d) (f x)

class AsPistonEngine a e | a -> e where
  _PistonEngine ::
    Prism' a (PistonEngine_ e)

instance AsPistonEngine (PistonEngine_ e) e where
  _PistonEngine =
    id

type PistonEngine =
  PistonEngine_ ()

type instance XPistonEngine () =
  ()

pattern PistonEngine ::
  PistonEngineConfiguration
  -> PistonEngineCycle
  -> Cylinders
  -> EngineDisplacement
  -> PistonEngine
pattern PistonEngine c y l d <- PistonEngine_ _ c y l d
  where PistonEngine c y l d = PistonEngine_ () c y l d

----

instance HasPistonEngineConfiguration (PistonEngine_ x) () where
  pistonEngineConfiguration f (PistonEngine_ x c y l d) =
    fmap (\c' -> PistonEngine_ x c' y l d) (f c)

instance HasPistonEngineCycle (PistonEngine_ x) () where
  pistonEngineCycle f (PistonEngine_ x c y l d) =
    fmap (\y' -> PistonEngine_ x c y' l d) (f y)

instance HasPositive (PistonEngine_ x) where
  positive f (PistonEngine_ x c y l d) =
    fmap (\l' -> PistonEngine_ x c y l' d) (f l)

instance HasEngineDisplacement (PistonEngine_ x) () where
  engineDisplacement f (PistonEngine_ x c y l d) =
    fmap (\d' -> PistonEngine_ x c y l d') (f d)
