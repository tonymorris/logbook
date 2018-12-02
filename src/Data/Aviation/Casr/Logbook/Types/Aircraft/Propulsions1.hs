{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1 where

import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Functor.Identity
import Data.List.NonEmpty
import GHC.Generics

newtype Propulsions1 cylinders displacement jettype position vtol =
  Propulsions1
    (NonEmpty (Propulsion cylinders displacement jettype position vtol))
  deriving Generic

type Propulsions1' a =
  Propulsions1 a a a a a
  
type Propulsions1I =
  Propulsions1 Identity
  