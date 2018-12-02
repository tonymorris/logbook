{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.Propulsions1 where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.Propulsion
import Data.List.NonEmpty
import GHC.Generics

newtype Propulsions1 cylinders displacement jettype position vtol =
  Propulsions1
    (NonEmpty (Propulsion cylinders displacement jettype position vtol))
  deriving Generic
