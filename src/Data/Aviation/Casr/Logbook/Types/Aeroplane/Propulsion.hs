{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.Propulsion where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aeroplane.PropulsionType
import GHC.Generics
import Prelude

data Propulsion cylinders displacement jettype position vtol =
  Propulsion
    (PropulsionType cylinders displacement jettype)
    (position PropulsionPosition)
    (vtol Bool)
  deriving Generic
