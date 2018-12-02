{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures where

import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeature
import Data.Set
import GHC.Generics
import Prelude

newtype AeroplaneDesignFeatures =
  AeroplaneDesignFeatures
    (Set AeroplaneDesignFeature)
  deriving (Eq, Ord, Show, Generic)
