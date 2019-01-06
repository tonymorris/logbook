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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsions where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsion
import GHC.Generics
import Prelude

type family XPropulsions x

data Propulsions_ x =
  Propulsions_
    !(XPropulsions x)
    [Propulsion]
  deriving Generic

deriving instance Eq (XPropulsions x) =>
  Eq (Propulsions_ x)

deriving instance Ord (XPropulsions x) =>
  Ord (Propulsions_ x)

deriving instance Show (XPropulsions x) =>
  Show (Propulsions_ x)

class HasPropulsions a e | a -> e where
  propulsions ::
    Lens' a (Propulsions_ e)
  xPropulsions ::
    Lens' a (XPropulsions e)
  xPropulsions =
    propulsions . xPropulsions

instance HasPropulsions (Propulsions_ e) e where
  propulsions =
    id
  xPropulsions f (Propulsions_ x n) =
    fmap (\x' -> Propulsions_ x' n) (f x)

class AsPropulsions a e | a -> e where
  _Propulsions ::
    Prism' a (Propulsions_ e)
 
instance AsPropulsions (Propulsions_ e) e where
  _Propulsions =
    id

instance Propulsions ~ x => Rewrapped Propulsions x
instance Wrapped Propulsions where
  type Unwrapped Propulsions =
    [Propulsion]
  _Wrapped' =
    iso
      (\(Propulsions_ () x) -> x)
      (Propulsions_ ())

type Propulsions =
  Propulsions_ ()

type instance XPropulsions () =
  ()

pattern Propulsions ::
  [Propulsion]
  -> Propulsions
pattern Propulsions p <- Propulsions_ _ p
  where Propulsions p = Propulsions_ () p

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
