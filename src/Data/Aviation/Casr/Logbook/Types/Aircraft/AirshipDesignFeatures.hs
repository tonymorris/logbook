{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures(
  module Semigroup
, module Monoid
, AirshipDesignFeatures(..)
) where

import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeature
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.Monoid as Monoid(Monoid(mappend, mempty))
import GHC.Generics
import Prelude

newtype AirshipDesignFeatures =
  AirshipDesignFeatures
    [AirshipDesignFeature]
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)
