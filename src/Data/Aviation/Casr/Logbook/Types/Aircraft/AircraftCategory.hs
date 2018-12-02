{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory where

import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeature
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeature
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeature
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeature
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1
import Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear
import Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory
import Data.Functor.Identity
import GHC.Generics

data AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeature airshipdesignfeature gyroplanedesignfeature helicopterdesignfeature =
  Aeroplane
    (Propulsions1 cylinders displacement jettype position vtol)
    (landinggear LandingGear)
    (aeroplanedesignfeature AeroplaneDesignFeature)
  | Helicopter (Propulsions1 cylinders displacement jettype position vtol) (helicopterdesignfeature HelicopterDesignFeature)
  | PoweredLift (Propulsions1 cylinders displacement jettype position vtol)
  | Gyroplane (Propulsions1 cylinders displacement jettype position vtol) (gyroplanedesignfeature GyroplaneDesignFeature)
  | Airship (Propulsions1 cylinders displacement jettype position vtol) (airshipdesignfeature AirshipDesignFeature)
  | Balloon
  | RPA (RPACategory cylinders displacement jettype position vtol rotors)
  | Glider (Propulsions cylinders displacement jettype position vtol)
  | Paraglider
  | Paramotor (Propulsions1 cylinders displacement jettype position vtol)
  | Trike (Propulsions cylinders displacement jettype position vtol)
  | PoweredParachute (Propulsions1 cylinders displacement jettype position vtol)
  | Hangglider
  | Simulator
  deriving Generic

type AircraftCategory' a =
  AircraftCategory a a a a a a a a a a a
  
type AircraftCategoryI =
  AircraftCategory' Identity
  