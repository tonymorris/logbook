{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle(
  PistonEngineCycle(..)
, HasPistonEngineCycle(..)
, AsPistonEngineCycle(..)
) where

import Control.Lens
import GHC.Generics
import Prelude

data PistonEngineCycle =
  FourStroke
  | TwoStroke
  deriving (Eq, Ord, Show, Generic)

makeClassy ''PistonEngineCycle
makeClassyPrisms ''PistonEngineCycle
