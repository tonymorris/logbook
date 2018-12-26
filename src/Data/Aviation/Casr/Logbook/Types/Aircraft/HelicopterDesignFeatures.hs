{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures(
  module Semigroup
, module Monoid
, HelicopterDesignFeatures(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeature
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.Monoid as Monoid(Monoid(mappend, mempty))
import GHC.Generics
import Prelude

newtype HelicopterDesignFeatures =
  HelicopterDesignFeatures
    [HelicopterDesignFeature]
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

makeWrapped ''HelicopterDesignFeatures
makeClassy ''HelicopterDesignFeatures

class AsHelicopterDesignFeatures a where
  _HelicopterDesignFeatures ::
    Prism' a HelicopterDesignFeatures

instance AsHelicopterDesignFeatures HelicopterDesignFeatures where
  _HelicopterDesignFeatures =
    id

type instance Index HelicopterDesignFeatures = Int
type instance IxValue HelicopterDesignFeatures = HelicopterDesignFeature

instance Ixed HelicopterDesignFeatures where
  ix n =
    _Wrapped . ix n

instance Cons HelicopterDesignFeatures HelicopterDesignFeatures HelicopterDesignFeature HelicopterDesignFeature where
  _Cons =
    _Wrapped . _Cons . seconding _Unwrapped'

instance Snoc HelicopterDesignFeatures HelicopterDesignFeatures HelicopterDesignFeature HelicopterDesignFeature where
  _Snoc =
    _Wrapped . _Snoc . firsting _Unwrapped'

instance Each HelicopterDesignFeatures HelicopterDesignFeatures HelicopterDesignFeature HelicopterDesignFeature where
  each =
    _Wrapped . each

instance Reversing HelicopterDesignFeatures where
  reversing =
    _Wrapped %~ reversing

instance Plated HelicopterDesignFeatures where
  plate =
    _Wrapped . plate . _Unwrapped'

instance AsEmpty HelicopterDesignFeatures where
  _Empty =
    _Wrapped . _Empty
