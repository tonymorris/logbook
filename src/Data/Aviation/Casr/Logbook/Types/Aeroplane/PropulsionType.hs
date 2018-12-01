{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.PropulsionType where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.JetType
import GHC.Generics
import Natural
import Prelude

data PropulsionType cylinders displacement jettype =
  Piston (cylinders Natural) (displacement Natural) -- cc
  | Jet (jettype JetType)
  | Electric
  | Rocket
  deriving Generic
