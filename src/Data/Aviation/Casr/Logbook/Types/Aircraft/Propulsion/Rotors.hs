{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors where

import Control.Lens
import GHC.Generics(Generic)
import Natural(Positive, HasPositive(positive), AsPositive(_Positive))
import Prelude

newtype Rotors =
  Rotors
    Positive
  deriving (Eq, Ord, Show, Generic)

class HasRotors a where
  rotors ::
    Lens' a Rotors

instance HasRotors Rotors where
  rotors =
    id

class AsRotors a where
  _Rotors ::
    Prism' a Rotors
 
instance AsRotors Rotors where
  _Rotors =
    id

instance Rotors ~ x => Rewrapped Rotors x
instance Wrapped Rotors where
  type Unwrapped Rotors =
    Positive
  _Wrapped' =
    iso
      (\(Rotors x) -> x)
      Rotors

----

instance HasPositive Rotors where
  positive =
    _Wrapped

instance AsPositive Rotors where
  _Positive =
    _Wrapped
