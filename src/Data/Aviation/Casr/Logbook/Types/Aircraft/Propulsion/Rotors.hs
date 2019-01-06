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
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors where

import Control.Lens
import GHC.Generics(Generic)
import Natural(Positive, HasPositive(positive))
import Prelude

type family XRotors x

data Rotors_ x =
  Rotors_
    !(XRotors x)
    Positive
  deriving Generic

deriving instance Eq (XRotors x) =>
  Eq (Rotors_ x)

deriving instance Ord (XRotors x) =>
  Ord (Rotors_ x)

deriving instance Show (XRotors x) =>
  Show (Rotors_ x)

class HasRotors a e | a -> e where
  rotors ::
    Lens' a (Rotors_ e)
  xRotors ::
    Lens' a (XRotors e)
  default xRotors ::
    Lens' a (XRotors e)
  xRotors =
    rotors . xRotors

instance HasRotors (Rotors_ e) e where
  rotors =
    id
  xRotors f (Rotors_ x n) =
    fmap (\x' -> Rotors_ x' n) (f x)

class AsRotors a e | a -> e where
  _Rotors ::
    Prism' a (Rotors_ e)
 
instance AsRotors (Rotors_ e) e where
  _Rotors =
    id

instance Rotors ~ x => Rewrapped Rotors x
instance Wrapped Rotors where
  type Unwrapped Rotors =
    Positive
  _Wrapped' =
    iso
      (\(Rotors_ () x) -> x)
      (Rotors_ ())

type Rotors =
  Rotors_ ()

type instance XRotors () =
  ()

pattern Rotors ::
  Positive
  -> Rotors
pattern Rotors p <- Rotors_ _ p
  where Rotors p = Rotors_ () p

----

instance HasPositive (Rotors_ x) where
  positive f (Rotors_ x p) =
    fmap (\p' -> Rotors_ x p') (f p)
