{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures where

import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeature
import Data.Set
import GHC.Generics
import Prelude

newtype HelicopterDesignFeatures =
  HelicopterDesignFeatures
    (Set HelicopterDesignFeature)
  deriving (Eq, Ord, Show, Generic)
