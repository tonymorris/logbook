{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures(
  module Semigroup
, module Monoid
, AeroplaneDesignFeatures(..)
, AsAeroplaneDesignFeatures(..)
, HasAeroplaneDesignFeatures(..)
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

makeWrapped ''AeroplaneDesignFeatures
makeClassy ''AeroplaneDesignFeatures

class AsAeroplaneDesignFeatures a where
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
