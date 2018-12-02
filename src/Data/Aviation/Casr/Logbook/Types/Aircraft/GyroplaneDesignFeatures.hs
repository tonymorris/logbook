{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures where

import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeature
import Data.Set
import GHC.Generics
import Prelude

newtype GyroplaneDesignFeatures =
  GyroplaneDesignFeatures
    (Set GyroplaneDesignFeature)
  deriving (Eq, Ord, Show, Generic)
