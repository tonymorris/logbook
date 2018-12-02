{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions where

import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Functor.Identity
import GHC.Generics

newtype Propulsions cylinders displacement jettype position vtol =
  Propulsions
    [Propulsion cylinders displacement jettype position vtol]
  deriving Generic

type Propulsions' a =
  Propulsions a a a a a

type PropulsionsI =
  Propulsions' Identity
  
singlePropulsions ::
  Propulsion cylinders displacement jettype position vtol
  -> Propulsions cylinders displacement jettype position vtol
singlePropulsions propulsion =
  Propulsions [propulsion]
  