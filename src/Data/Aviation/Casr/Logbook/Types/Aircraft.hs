{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft(
  module A
, testAircraft5350
, testAircraftAFR
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
import Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.MTOW as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType as A
import Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory as A

import Control.Lens
import Data.Int
import Data.List.NonEmpty
import Natural

testAircraft5350 ::
  Aircraft
testAircraft5350 =
  Aircraft
    (
      RAAusAircraftRegistration (
        RAAusRegistration
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
      Aeroplane
        (
          single_naturallyinduce_dice_fourstroke
            "Rotax"
            "912ULS"
            Carburetor
            (list1 # (() :| [(), (), ()]))
            (oneOr (1352 :: Int))
        )
        LandingGearFixedTricycle
        mempty
    )
    (
      MTOW (oneOr (560 :: Int))
    )

testAircraftAFR ::
  Aircraft
testAircraftAFR =
  Aircraft
    (
      CASAAircraftRegistration (
        (
          CASARegistration
            Upper_A
            Upper_F
            Upper_R
        )
      )
    )
    (
      Aeroplane
        (
          single_naturallyinduce_dice_fourstroke
            "Textron Lycoming"
            "IO-360-L2A"
            FuelInjection
            (list1 # (() :| [(), (), ()]))
            (oneOr (5920 :: Int))
        )
        LandingGearFixedTricycle
        mempty
    )
    (
      MTOW (oneOr (1157 :: Int))
    )
