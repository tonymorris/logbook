{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.RPACategory where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.Propulsions1
import GHC.Generics
import Natural
 
data RPACategory cylinders displacement jettype position vtol rotors =
  RPAAeroplane (Propulsions1 cylinders displacement jettype position vtol)
  | RPACopter (Propulsions1 cylinders displacement jettype position vtol) (rotors Positive)
  | RPAAirship (Propulsions1 cylinders displacement jettype position vtol)
  | RPAPoweredLift (Propulsions1 cylinders displacement jettype position vtol)
  deriving Generic
