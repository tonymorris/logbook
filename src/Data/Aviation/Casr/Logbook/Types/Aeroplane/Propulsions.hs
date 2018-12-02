{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.Propulsions where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.Propulsion
import GHC.Generics

newtype Propulsions cylinders displacement jettype position vtol =
  Propulsions
    [Propulsion cylinders displacement jettype position vtol]
  deriving Generic
