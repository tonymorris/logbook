{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

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

makeWrapped ''Propulsions1
makeClassy ''Propulsions1

class AsPropulsions1 a where
  _Propulsions1 ::
    Prism' a Propulsions1

instance AsPropulsions1 Propulsions1 where
  _Propulsions1 =
    id

single_naturallyinduced_ice_fourstroke ::
  String
  -> String
  -> InternalCombustionEngineFuelInduction
  -> Positive
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
    