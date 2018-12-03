{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1 where

import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Functor.Identity
import Data.List.NonEmpty
import GHC.Generics
import Prelude
import Natural

newtype Propulsions1 cylinders displacement jettype position vtol =
  Propulsions1
    (NonEmpty (Propulsion cylinders displacement jettype position vtol))
  deriving Generic

type Propulsions1' a =
  Propulsions1 a a a a a
  
type Propulsions1I =
  Propulsions1' Identity

singlePropulsions1 ::
  Propulsion cylinders displacement jettype position vtol
  -> Propulsions1 cylinders displacement jettype position vtol
singlePropulsions1 propulsion =
  Propulsions1 (propulsion :| [])

deriving instance (Eq (cylinders Positive), Eq (displacement (Positive)), Eq (jettype JetType), Eq (position PropulsionPosition), Eq (vtol Bool)) => Eq (Propulsions1 cylinders displacement jettype position vtol)

deriving instance (Ord (cylinders Positive), Ord (displacement (Positive)), Ord (jettype JetType), Ord (position PropulsionPosition), Ord (vtol Bool)) => Ord (Propulsions1 cylinders displacement jettype position vtol)

deriving instance (Show (cylinders Positive), Show (displacement (Positive)), Show (jettype JetType), Show (position PropulsionPosition), Show (vtol Bool)) => Show (Propulsions1 cylinders displacement jettype position vtol)
  