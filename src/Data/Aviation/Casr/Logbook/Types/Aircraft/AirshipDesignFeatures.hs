{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures(
  module Semigroup
, module Monoid
, AirshipDesignFeatures(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeature
import Data.Semigroup as Semigroup(Semigroup((<>)))
import Data.Monoid as Monoid(Monoid(mappend, mempty))
import GHC.Generics
import Prelude

newtype AirshipDesignFeatures =
  AirshipDesignFeatures
    [AirshipDesignFeature]
  deriving (Eq, Ord, Show, Generic, Semigroup, Monoid)

makeWrapped ''AirshipDesignFeatures
makeClassy ''AirshipDesignFeatures

class AsAirshipDesignFeatures a where
  _AirshipDesignFeatures ::
    Prism' a AirshipDesignFeatures

instance AsAirshipDesignFeatures AirshipDesignFeatures where
  _AirshipDesignFeatures =
    id

type instance Index AirshipDesignFeatures = Int
type instance IxValue AirshipDesignFeatures = AirshipDesignFeature

instance Ixed AirshipDesignFeatures where
  ix n =
    _Wrapped . ix n

instance Cons AirshipDesignFeatures AirshipDesignFeatures AirshipDesignFeature AirshipDesignFeature where
  _Cons =
    _Wrapped . _Cons . seconding _Unwrapped'

instance Snoc AirshipDesignFeatures AirshipDesignFeatures AirshipDesignFeature AirshipDesignFeature where
  _Snoc =
    _Wrapped . _Snoc . firsting _Unwrapped'

instance Each AirshipDesignFeatures AirshipDesignFeatures AirshipDesignFeature AirshipDesignFeature where
  each =
    _Wrapped . each

instance Reversing AirshipDesignFeatures where
  reversing =
    _Wrapped %~ reversing

instance Plated AirshipDesignFeatures where
  plate =
    _Wrapped . plate . _Unwrapped'

instance AsEmpty AirshipDesignFeatures where
  _Empty =
    _Wrapped . _Empty
