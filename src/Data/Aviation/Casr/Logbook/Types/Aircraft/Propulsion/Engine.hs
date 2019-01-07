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

data Engine_ x =
  Engine_
    !(XEngine x)
    Manufacturer
    Designation
    (EngineType () () () () () () () () () () ())
  deriving Generic

deriving instance Eq (XEngine x) =>
  Eq (Engine_ x)

deriving instance Ord (XEngine x) =>
  Ord (Engine_ x)

deriving instance Show (XEngine x) =>
  Show (Engine_ x)

class HasEngine a e | a -> e where
  engine ::
    Lens' a (Engine_ e)
  xEngine ::
    Lens' a (XEngine e)
  xEngine =
    engine . xEngine

instance HasEngine (Engine_ e) e where
  engine =
    id
  xEngine f (Engine_ x m d t) =
    fmap (\x' -> Engine_ x' m d t) (f x)

class AsEngine a e | a -> e where
  _Engine ::
    Prism' a (Engine_ e)

instance AsEngine (Engine_ e) e where
  _Engine =
    id

type Engine =
  Engine_ ()

type instance XEngine () =
  ()

pattern Engine ::
  Manufacturer
  -> Designation
  -> EngineType () () () () () () () () () () ()
  -> Engine
pattern Engine m d t <- Engine_ _ m d t
  where Engine m d t = Engine_ () m d t

----

instance HasEngineType (Engine_ x) () () () () () () () () () () () () where
  engineType f (Engine_ x m d t) =
    fmap (\t' -> Engine_ x m d t') (f t) 
