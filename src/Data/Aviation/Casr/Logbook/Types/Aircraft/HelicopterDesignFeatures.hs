{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures(
  module Semigroup
, module Monoid
, HelicopterDesignFeatures(..)
) where

import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeature
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.Monoid as Monoid(Monoid(mappend, mempty))
import GHC.Generics
import Prelude

newtype HelicopterDesignFeatures =
  HelicopterDesignFeatures
    [HelicopterDesignFeature]
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)
