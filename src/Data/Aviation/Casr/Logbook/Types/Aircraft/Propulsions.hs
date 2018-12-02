{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions where

import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import GHC.Generics

newtype Propulsions cylinders displacement jettype position vtol =
  Propulsions
    [Propulsion cylinders displacement jettype position vtol]
  deriving Generic