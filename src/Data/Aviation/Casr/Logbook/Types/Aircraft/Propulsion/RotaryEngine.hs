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
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement(EngineDisplacement, HasEngineDisplacement(engineDisplacement))
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors(Rotors, HasRotors(rotors))
import GHC.Generics(Generic)
import Prelude

type family XRotaryEngine x

data RotaryEngine_ x =
  RotaryEngine_
    !(XRotaryEngine x)
    Rotors
    EngineDisplacement
  deriving Generic

deriving instance Eq (XRotaryEngine x) =>
    Eq (RotaryEngine_ x)

deriving instance Ord (XRotaryEngine x) =>
    Ord (RotaryEngine_ x)

deriving instance Show (XRotaryEngine x) =>
    Show (RotaryEngine_ x)

class HasRotaryEngine a e | a -> e where
  rotaryEngine ::
    Lens' a (RotaryEngine_ e)
  xRotaryEngine ::
    XRotaryEngine e ~ x =>
    Lens' a x
  default xRotaryEngine ::
    XRotaryEngine () ~ x =>
    Lens' a x
  xRotaryEngine f a =
    fmap (\() -> a) (f ())

instance HasRotaryEngine (RotaryEngine_ e) e where
  rotaryEngine =
    id
  xRotaryEngine f (RotaryEngine_ x r d) =
    fmap (\x' -> RotaryEngine_ x' r d) (f x)

class AsRotaryEngine a e | a -> e where
  _RotaryEngine ::
    Prism' a (RotaryEngine_ e)
 
instance AsRotaryEngine (RotaryEngine_ e) e where
  _RotaryEngine =
    id

type RotaryEngine =
  RotaryEngine_ ()

type instance XRotaryEngine () =
  ()

pattern RotaryEngine ::
  Rotors
  -> EngineDisplacement
  -> RotaryEngine
pattern RotaryEngine r d <- RotaryEngine_ _ r d
  where RotaryEngine r d = RotaryEngine_ () r d

----

instance HasRotors (RotaryEngine_ e) () where
  rotors f (RotaryEngine_ x r d) =
    fmap (\r' -> RotaryEngine_ x r' d) (f r)

instance HasEngineDisplacement (RotaryEngine_ e) () where
  engineDisplacement f (RotaryEngine_ x r d) =
    fmap (\d' -> RotaryEngine_ x r d') (f d)
