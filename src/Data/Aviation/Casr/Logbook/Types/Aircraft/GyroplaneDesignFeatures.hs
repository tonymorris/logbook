{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures(
  module Semigroup
, module Monoid
, GyroplaneDesignFeatures(..)
) where

import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeature
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.Monoid as Monoid(Monoid(mappend, mempty))
import GHC.Generics
import Prelude

newtype GyroplaneDesignFeatures =
  GyroplaneDesignFeatures
    [GyroplaneDesignFeature]
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)
