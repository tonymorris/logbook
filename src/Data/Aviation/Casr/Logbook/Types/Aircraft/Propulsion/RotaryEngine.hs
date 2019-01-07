{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement(EngineDisplacement, XEngineDisplacement, HasEngineDisplacement(engineDisplacement))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors(Rotors, XRotors, HasRotors(rotors))
import GHC.Generics(Generic)
import Prelude

type family XRotaryEngine x

data RotaryEngine x xrotors xenginedisplacement =
  RotaryEngine
    !(XRotaryEngine x)
    (Rotors xrotors)
    (EngineDisplacement xenginedisplacement)
  deriving Generic

deriving instance (Eq (XRotaryEngine x), Eq (XRotors xrotors), Eq (XEngineDisplacement xenginedisplacement)) =>
    Eq (RotaryEngine x xrotors xenginedisplacement)

deriving instance (Ord (XRotaryEngine x), Ord (XRotors xrotors), Ord (XEngineDisplacement xenginedisplacement)) =>
    Ord (RotaryEngine x xrotors xenginedisplacement)

deriving instance (Show (XRotaryEngine x), Show (XRotors xrotors), Show (XEngineDisplacement xenginedisplacement)) =>
    Show (RotaryEngine x xrotors xenginedisplacement)

class HasRotaryEngine a e xrotors xenginedisplacement
    | a -> e, a -> xrotors, a -> xenginedisplacement where
  rotaryEngine ::
    Lens' a (RotaryEngine e xrotors xenginedisplacement)
  xRotaryEngine ::
    Lens' a (XRotaryEngine e)
  xRotaryEngine =
    rotaryEngine . xRotaryEngine

instance HasRotaryEngine (RotaryEngine e xrotors xenginedisplacement) e xrotors xenginedisplacement where
  rotaryEngine =
    id
  xRotaryEngine f (RotaryEngine x r d) =
    fmap (\x' -> RotaryEngine x' r d) (f x)

class AsRotaryEngine a e xrotors xenginedisplacement | a -> e, a -> xrotors, a -> xenginedisplacement where
  _RotaryEngine ::
    Prism' a (RotaryEngine e xrotors xenginedisplacement)
 
instance AsRotaryEngine (RotaryEngine e xrotors xenginedisplacement) e xrotors xenginedisplacement where
  _RotaryEngine =
    id

type RotaryEngine_ =
  RotaryEngine ()

type instance XRotaryEngine () =
  ()

pattern RotaryEngine_ ::
  Rotors xrotors
  -> EngineDisplacement xenginedisplacement
  -> RotaryEngine_ xrotors xenginedisplacement
pattern RotaryEngine_ r d <- RotaryEngine _ r d
  where RotaryEngine_ r d = RotaryEngine () r d

----

instance HasRotors (RotaryEngine e xrotors xenginedisplacement) xrotors where
  rotors f (RotaryEngine x r d) =
    fmap (\r' -> RotaryEngine x r' d) (f r)

instance HasEngineDisplacement (RotaryEngine e xrotors xenginedisplacement) xenginedisplacement where
  engineDisplacement f (RotaryEngine x r d) =
    fmap (\d' -> RotaryEngine x r d') (f d)
