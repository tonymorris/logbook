{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors where

import Control.Lens
import GHC.Generics(Generic)
import Natural(Positive, HasPositive(positive))
import Prelude

type family XRotors x

data Rotors x =
  Rotors
    !(XRotors x)
    Positive
  deriving Generic

deriving instance Eq (XRotors x) =>
  Eq (Rotors x)

deriving instance Ord (XRotors x) =>
  Ord (Rotors x)

deriving instance Show (XRotors x) =>
  Show (Rotors x)

class HasRotors a e | a -> e where
  rotors ::
    Lens' a (Rotors e)
  xRotors ::
    Lens' a (XRotors e)
  xRotors =
    rotors . xRotors

instance HasRotors (Rotors e) e where
  rotors =
    id
  xRotors f (Rotors x n) =
    fmap (\x' -> Rotors x' n) (f x)

class AsRotors a e | a -> e where
  _Rotors ::
    Prism' a (Rotors e)
 
instance AsRotors (Rotors e) e where
  _Rotors =
    id

instance Rotors_ ~ x => Rewrapped Rotors_ x
instance Wrapped Rotors_ where
  type Unwrapped Rotors_ =
    Positive
  _Wrapped' =
    iso
      (\(Rotors () x) -> x)
      (Rotors ())

type Rotors_ =
  Rotors ()

type instance XRotors () =
  ()

pattern Rotors_ ::
  Positive
  -> Rotors_
pattern Rotors_ p <- Rotors _ p
  where Rotors_ p = Rotors () p

----

instance HasPositive (Rotors x) where
  positive f (Rotors x p) =
    fmap (\p' -> Rotors x p') (f p)
