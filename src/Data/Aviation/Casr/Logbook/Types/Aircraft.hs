{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft(
  module A
) where

import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeature as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.Aircraft as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftRegistration as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeature as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.CASARegistration as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigits4 as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeature as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeature as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1 as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory as A

import Control.Lens
import Data.Int
import Data.List.NonEmpty
import Data.Maybe

testAircraft5350 ::
  AircraftI
testAircraft5350 =
  aircraftI
    (
      raAusAircraftRegistrationI (
        raAusRegistrationI
          RAAusRegistrationTypeFull
          RAAusRegistrationPrefix24
          (
            DecDigits4
              DecDigit5
              DecDigit3
              DecDigit5
              DecDigit0
          )
      )
    )
    (
      aeroplaneAircraftCategoryI
        (
          singlePropulsions1
            (
              propulsionI
                (
                  pistonPropulsionTypeI
                    (list1 # (() :| [(), (), ()]))
                    (fromMaybe one' ((1352 :: Int) ^? _Positive))
                )
                Centreline
                False
            )
        )
        LandingGearFixedTricycle
        mempty
    )

testAircraftAFR ::
  AircraftI
testAircraftAFR =
  aircraftI
    (
      casaAircraftRegistrationI (
        (
          CASARegistration
            Upper_A
            Upper_F
            Upper_R
        )
      )
    )
    (
      aeroplaneAircraftCategoryI
        (
          singlePropulsions1
            (
              propulsionI
                (
                  pistonPropulsionTypeI
                    (list1 # (() :| [(), (), ()]))
                    (fromMaybe one' ((5920 :: Int) ^? _Positive))
                )
                Centreline
                False
            )
        )
        LandingGearFixedTricycle
        mempty
    )
