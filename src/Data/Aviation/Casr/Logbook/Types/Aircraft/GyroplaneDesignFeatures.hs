{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures(
  module Semigroup
, module Monoid
, GyroplaneDesignFeatures(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeature
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.Monoid as Monoid(Monoid(mappend, mempty))
import GHC.Generics
import Prelude

newtype GyroplaneDesignFeatures =
  GyroplaneDesignFeatures
    [GyroplaneDesignFeature]
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

makeWrapped ''GyroplaneDesignFeatures
makeClassy ''GyroplaneDesignFeatures

class AsGyroplaneDesignFeatures a where
  _GyroplaneDesignFeatures ::
    Prism' a GyroplaneDesignFeatures

instance AsGyroplaneDesignFeatures GyroplaneDesignFeatures where
  _GyroplaneDesignFeatures =
    id

type instance Index GyroplaneDesignFeatures = Int
type instance IxValue GyroplaneDesignFeatures = GyroplaneDesignFeature

instance Ixed GyroplaneDesignFeatures where
  ix n =
    _Wrapped . ix n

instance Cons GyroplaneDesignFeatures GyroplaneDesignFeatures GyroplaneDesignFeature GyroplaneDesignFeature where
  _Cons =
    _Wrapped . _Cons . seconding _Unwrapped'

instance Snoc GyroplaneDesignFeatures GyroplaneDesignFeatures GyroplaneDesignFeature GyroplaneDesignFeature where
  _Snoc =
    _Wrapped . _Snoc . firsting _Unwrapped'

instance Each GyroplaneDesignFeatures GyroplaneDesignFeatures GyroplaneDesignFeature GyroplaneDesignFeature where
  each =
    _Wrapped . each

instance Reversing GyroplaneDesignFeatures where
  reversing =
    _Wrapped %~ reversing

instance Plated GyroplaneDesignFeatures where
  plate =
    _Wrapped . plate . _Unwrapped'

instance AsEmpty GyroplaneDesignFeatures where
  _Empty =
    _Wrapped . _Empty
