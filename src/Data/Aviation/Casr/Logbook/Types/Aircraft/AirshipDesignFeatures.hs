{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures where

import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeature
import Data.Set
import GHC.Generics
import Prelude

newtype AirshipDesignFeatures =
  AirshipDesignFeatures
    (Set AirshipDesignFeature)
  deriving (Eq, Ord, Show, Generic)
