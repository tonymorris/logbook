{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PropulsionPosition where

import Control.Lens
import GHC.Generics
import Prelude

data PropulsionPosition =
  Centreline
  | LeftPropulsion
  | RightPropulsion
  deriving (Eq, Ord, Show, Generic)

makeClassy ''PropulsionPosition
makeClassyPrisms ''PropulsionPosition
