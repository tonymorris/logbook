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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsion where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Engine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PropulsionPosition
import GHC.Generics
import Prelude

type family XPropulsion x

data Propulsion_ x =
  Propulsion_
    !(XPropulsion x)
    Engine
    PropulsionPosition
  deriving Generic

deriving instance Eq (XPropulsion x) =>
  Eq (Propulsion_ x)

deriving instance Ord (XPropulsion x) =>
  Ord (Propulsion_ x)

deriving instance Show (XPropulsion x) =>
  Show (Propulsion_ x)

class HasPropulsion a e | a -> e where
  propulsion ::
    Lens' a (Propulsion_ e)
  xPropulsion ::
    Lens' a (XPropulsion e)
  xPropulsion =
    propulsion . xPropulsion

instance HasPropulsion (Propulsion_ e) e where
  propulsion =
    id
  xPropulsion f (Propulsion_ x e p) =
    fmap (\x' -> Propulsion_ x' e p) (f x)

class AsPropulsion a e | a -> e where
  _Propulsion ::
    Prism' a (Propulsion_ e)

instance AsPropulsion (Propulsion_ e) e where
  _Propulsion =
    id

type Propulsion =
  Propulsion_ ()

type instance XPropulsion () =
  ()

pattern Propulsion ::
  Engine
  -> PropulsionPosition
  -> Propulsion
pattern Propulsion e p <- Propulsion_ _ e p
  where Propulsion e p = Propulsion_ () e p

----

instance HasEngine (Propulsion_ x) () where
  engine f (Propulsion_ x e p) =
    fmap (\e' -> Propulsion_ x e' p) (f e)

instance HasPropulsionPosition (Propulsion_ x) () where
  propulsionPosition f (Propulsion_ x e p) =
    fmap (\p' -> Propulsion_ x e p') (f p)
