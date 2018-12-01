{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.PropulsionPosition where

import GHC.Generics
import Prelude

data PropulsionPosition =
  Centreline
  | LeftPropulsion
  | RightPropulsion
  deriving (Eq, Ord, Show, Generic)
