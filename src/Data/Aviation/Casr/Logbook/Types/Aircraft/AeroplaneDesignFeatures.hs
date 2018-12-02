{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures(
  module Semigroup
, module Monoid
, AeroplaneDesignFeatures(..)
) where

import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeature
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.Monoid as Monoid(Monoid(mappend, mempty))
import Data.Set
import GHC.Generics
import Prelude

newtype AeroplaneDesignFeatures =
  AeroplaneDesignFeatures
    (Set AeroplaneDesignFeature)
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)
