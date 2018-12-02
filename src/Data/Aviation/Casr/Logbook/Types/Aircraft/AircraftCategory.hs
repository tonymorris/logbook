{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory where

import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1
import Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear
import Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory
import Data.Functor.Identity
import GHC.Generics

data AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures =
  Aeroplane
    (Propulsions1 cylinders displacement jettype position vtol)
    (landinggear LandingGear)
    (aeroplanedesignfeatures AeroplaneDesignFeatures)
  | Helicopter (Propulsions1 cylinders displacement jettype position vtol) (helicopterdesignfeatures HelicopterDesignFeatures)
  | PoweredLift (Propulsions1 cylinders displacement jettype position vtol)
  | Gyroplane (Propulsions1 cylinders displacement jettype position vtol) (gyroplanedesignfeatures GyroplaneDesignFeatures)
  | Airship (Propulsions1 cylinders displacement jettype position vtol) (airshipdesignfeatures AirshipDesignFeatures)
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
  
aeroplaneAircraftCategoryI ::
  Propulsions1I
  -> LandingGear
  -> AeroplaneDesignFeatures
  -> AircraftCategoryI
aeroplaneAircraftCategoryI propulsions1 landinggear aeroplanedesignfeatures =
  Aeroplane propulsions1 (Identity landinggear) (Identity aeroplanedesignfeatures)

helicopterAircraftCategoryI ::
  Propulsions1I
  -> HelicopterDesignFeatures
  -> AircraftCategoryI
helicopterAircraftCategoryI propulsions1 helicopterdesignfeatures =
  Helicopter propulsions1 (Identity helicopterdesignfeatures)

gyroplaneAircraftCategoryI ::
  Propulsions1I
  -> GyroplaneDesignFeatures
  -> AircraftCategoryI
gyroplaneAircraftCategoryI propulsions1 gyroplanedesignfeatures =
  Gyroplane propulsions1 (Identity gyroplanedesignfeatures)

airshipAircraftCategoryI ::
  Propulsions1I
  -> AirshipDesignFeatures
  -> AircraftCategoryI
airshipAircraftCategoryI propulsions1 airshipdesignfeatures =
  Airship propulsions1 (Identity airshipdesignfeatures)
