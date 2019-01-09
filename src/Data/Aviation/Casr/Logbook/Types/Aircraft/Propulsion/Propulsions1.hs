{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsions1 where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Engine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineConfiguration
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PropulsionPosition
import Data.List.NonEmpty
import GHC.Generics
import Natural
import Prelude

newtype Propulsions1 =
  Propulsions1
    (NonEmpty Propulsion)
  deriving (Eq, Ord, Show, Generic)

class HasPropulsions1 a where
  propulsions1 ::
    Lens' a Propulsions1

instance HasPropulsions1 Propulsions1 where
  propulsions1 =
    id

class AsPropulsions1 a where
  _Propulsions1 ::
    Prism' a Propulsions1

instance AsPropulsions1 Propulsions1 where
  _Propulsions1 =
    id

instance Propulsions1 ~ x => Rewrapped Propulsions1 x

instance Wrapped Propulsions1 where
  type Unwrapped Propulsions1 =
    NonEmpty Propulsion
  _Wrapped' =
    iso
      (\(Propulsions1 x) -> x)
      Propulsions1

----

type instance Index Propulsions1 = Int
type instance IxValue Propulsions1 = Propulsion

instance Ixed Propulsions1 where
  ix n =
    _Wrapped . ix n

instance Each Propulsions1 Propulsions1 Propulsion Propulsion where
  each =
    _Wrapped . each

instance Reversing Propulsions1 where
  reversing =
    _Wrapped %~ reversing

single_naturallyinduced_ice_fourstroke :: 
  Manufacturer
  -> Designation
  -> InternalCombustionEngineFuelInduction
  -> Cylinders
  -> Positive
  -> Propulsions1
single_naturallyinduced_ice_fourstroke enginemanufacturer enginedesgination fuelinduction cylinders displacement =
  Propulsions1
    ( 
      pure
        (
          Propulsion
            (
              Engine
                enginemanufacturer
                enginedesgination
                (
                  InternalCombustionEngineEngineType
                    (
                      InternalCombustionEngine
                        NaturalInduction
                        fuelinduction
                        Spark
                        (
                          PistonEngineType
                            (
                              PistonEngine
                                Opposed
                                FourStroke
                                cylinders
                                (EngineDisplacement displacement)
                            )
                        )
                    )
                )
            )
            Centreline
        )
    )
      