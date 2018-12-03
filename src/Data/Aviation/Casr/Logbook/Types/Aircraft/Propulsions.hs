{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions where

import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Functor.Identity
import GHC.Generics
import Prelude
import Natural

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

deriving instance (Eq (cylinders Positive), Eq (displacement (Positive)), Eq (jettype JetType), Eq (position PropulsionPosition), Eq (vtol Bool)) => Eq (Propulsions cylinders displacement jettype position vtol)

deriving instance (Ord (cylinders Positive), Ord (displacement (Positive)), Ord (jettype JetType), Ord (position PropulsionPosition), Ord (vtol Bool)) => Ord (Propulsions cylinders displacement jettype position vtol)

deriving instance (Show (cylinders Positive), Show (displacement (Positive)), Show (jettype JetType), Show (position PropulsionPosition), Show (vtol Bool)) => Show (Propulsions cylinders displacement jettype position vtol)
