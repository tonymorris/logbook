{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.RotaryEngine where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement(EngineDisplacement_, XEngineDisplacement, HasEngineDisplacement(engineDisplacement))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors(Rotors_, XRotors, HasRotors(rotors))
import GHC.Generics(Generic)
import Prelude

type family XRotaryEngine x

data RotaryEngine_ x xrotors xenginedisplacement =
  RotaryEngine_
    !(XRotaryEngine x)
    (Rotors_ xrotors)
    (EngineDisplacement_ xenginedisplacement)
  deriving Generic

deriving instance (Eq (XRotaryEngine x), Eq (XRotors xrotors), Eq (XEngineDisplacement xenginedisplacement)) =>
    Eq (RotaryEngine_ x xrotors xenginedisplacement)

deriving instance (Ord (XRotaryEngine x), Ord (XRotors xrotors), Ord (XEngineDisplacement xenginedisplacement)) =>
    Ord (RotaryEngine_ x xrotors xenginedisplacement)

deriving instance (Show (XRotaryEngine x), Show (XRotors xrotors), Show (XEngineDisplacement xenginedisplacement)) =>
    Show (RotaryEngine_ x xrotors xenginedisplacement)

class HasRotaryEngine a e xrotors xenginedisplacement
    | a -> e, a -> xrotors, a -> xenginedisplacement where
  rotaryEngine ::
    Lens' a (RotaryEngine_ e xrotors xenginedisplacement)
  xRotaryEngine ::
    Lens' a (XRotaryEngine e)
  default xRotaryEngine ::
    Lens' a (XRotaryEngine e)
  xRotaryEngine =
    rotaryEngine . xRotaryEngine

instance HasRotaryEngine (RotaryEngine_ e xrotors xenginedisplacement) e xrotors xenginedisplacement where
  rotaryEngine =
    id
  xRotaryEngine f (RotaryEngine_ x r d) =
    fmap (\x' -> RotaryEngine_ x' r d) (f x)

class AsRotaryEngine a e xrotors xenginedisplacement | a -> e, a -> xrotors, a -> xenginedisplacement where
  _RotaryEngine ::
    Prism' a (RotaryEngine_ e xrotors xenginedisplacement)
 
instance AsRotaryEngine (RotaryEngine_ e xrotors xenginedisplacement) e xrotors xenginedisplacement where
  _RotaryEngine =
    id

type RotaryEngine =
  RotaryEngine_ ()

type instance XRotaryEngine () =
  ()

pattern RotaryEngine ::
  Rotors_ xrotors
  -> EngineDisplacement_ xenginedisplacement
  -> RotaryEngine xrotors xenginedisplacement
pattern RotaryEngine r d <- RotaryEngine_ _ r d
  where RotaryEngine r d = RotaryEngine_ () r d

----

instance HasRotors (RotaryEngine_ e xrotors xenginedisplacement) xrotors where
  rotors f (RotaryEngine_ x r d) =
    fmap (\r' -> RotaryEngine_ x r' d) (f r)

instance HasEngineDisplacement (RotaryEngine_ e xrotors xenginedisplacement) xenginedisplacement where
  engineDisplacement f (RotaryEngine_ x r d) =
    fmap (\d' -> RotaryEngine_ x r d') (f d)
