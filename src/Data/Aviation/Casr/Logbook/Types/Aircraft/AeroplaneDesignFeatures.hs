{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures(
  module Semigroup
, module Monoid
, AeroplaneDesignFeatures(..)
, ManyAeroplaneDesignFeatures(..)
, HasAeroplaneDesignFeatures(..)
, AsAeroplaneDesignFeatures(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeature
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.Monoid as Monoid(Monoid(mappend, mempty))
import GHC.Generics
import Prelude

newtype AeroplaneDesignFeatures =
  AeroplaneDesignFeatures
    [AeroplaneDesignFeature]
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

instance AeroplaneDesignFeatures ~ a =>
  Rewrapped AeroplaneDesignFeatures a

instance Wrapped AeroplaneDesignFeatures where
  type Unwrapped AeroplaneDesignFeatures =
    [AeroplaneDesignFeature]
  _Wrapped' =
    iso (\ (AeroplaneDesignFeatures x) -> x) AeroplaneDesignFeatures

class ManyAeroplaneDesignFeatures a where
  _AeroplaneDesignFeatures_ ::
    Traversal' a AeroplaneDesignFeatures

instance ManyAeroplaneDesignFeatures AeroplaneDesignFeatures where
  _AeroplaneDesignFeatures_ =
    id

class ManyAeroplaneDesignFeatures a => HasAeroplaneDesignFeatures a where
  aeroplaneDesignFeatures ::
    Lens' a AeroplaneDesignFeatures

instance HasAeroplaneDesignFeatures AeroplaneDesignFeatures where
  aeroplaneDesignFeatures =
    id

class ManyAeroplaneDesignFeatures a => AsAeroplaneDesignFeatures a where
  _AeroplaneDesignFeatures ::
    Prism' a AeroplaneDesignFeatures

instance AsAeroplaneDesignFeatures AeroplaneDesignFeatures where
  _AeroplaneDesignFeatures =
    id

type instance Index AeroplaneDesignFeatures = Int
type instance IxValue AeroplaneDesignFeatures = AeroplaneDesignFeature

instance Ixed AeroplaneDesignFeatures where
  ix n =
    _Wrapped . ix n

instance Cons AeroplaneDesignFeatures AeroplaneDesignFeatures AeroplaneDesignFeature AeroplaneDesignFeature where
  _Cons =
    _Wrapped . _Cons . seconding _Unwrapped'

instance Snoc AeroplaneDesignFeatures AeroplaneDesignFeatures AeroplaneDesignFeature AeroplaneDesignFeature where
  _Snoc =
    _Wrapped . _Snoc . firsting _Unwrapped'

instance Each AeroplaneDesignFeatures AeroplaneDesignFeatures AeroplaneDesignFeature AeroplaneDesignFeature where
  each =
    _Wrapped . each

instance Reversing AeroplaneDesignFeatures where
  reversing =
    _Wrapped %~ reversing

instance Plated AeroplaneDesignFeatures where
  plate =
    _Wrapped . plate . _Unwrapped'

instance AsEmpty AeroplaneDesignFeatures where
  _Empty =
    _Wrapped . _Empty
