{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsions where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsion
import GHC.Generics
import Prelude

newtype Propulsions =
  Propulsions
    [Propulsion]
  deriving (Eq, Ord, Show, Generic)

class HasPropulsions a where
  propulsions ::
    Lens' a Propulsions

instance HasPropulsions Propulsions where
  propulsions =
    id

class AsPropulsions a where
  _Propulsions ::
    Prism' a Propulsions

instance AsPropulsions Propulsions where
  _Propulsions =
    id

instance Propulsions ~ x => Rewrapped Propulsions x
instance Wrapped Propulsions where
  type Unwrapped Propulsions =
    [Propulsion]
  _Wrapped' =
    iso
      (\(Propulsions x) -> x)
      Propulsions

----

type instance Index Propulsions = Int
type instance IxValue Propulsions = Propulsion

instance Ixed Propulsions where
  ix n =
    _Wrapped . ix n

instance Cons Propulsions Propulsions Propulsion Propulsion where
  _Cons =
    _Wrapped . _Cons . seconding _Unwrapped'

instance Snoc Propulsions Propulsions Propulsion Propulsion where
  _Snoc =
    _Wrapped . _Snoc . firsting _Unwrapped'

instance Each Propulsions Propulsions Propulsion Propulsion where
  each =
    _Wrapped . each

instance Reversing Propulsions where
  reversing =
    _Wrapped %~ reversing

instance Plated Propulsions where
  plate =
    _Wrapped . plate . _Unwrapped'

instance AsEmpty Propulsions where
  _Empty =
    _Wrapped . _Empty

singlePropulsions ::
  Propulsion
  -> Propulsions
singlePropulsions propulsion_ =
  Propulsions [propulsion_]
