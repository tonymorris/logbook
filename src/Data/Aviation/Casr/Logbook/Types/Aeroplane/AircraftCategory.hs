{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.AircraftCategory where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.AeroplaneDesignFeature
import Data.Aviation.Casr.Logbook.Types.Aeroplane.AirshipDesignFeature
import Data.Aviation.Casr.Logbook.Types.Aeroplane.GyroplaneDesignFeature
import Data.Aviation.Casr.Logbook.Types.Aeroplane.HelicopterDesignFeature
import Data.Aviation.Casr.Logbook.Types.Aeroplane.Propulsions
import Data.Aviation.Casr.Logbook.Types.Aeroplane.Propulsions1
import Data.Aviation.Casr.Logbook.Types.Aeroplane.LandingGear
import Data.Aviation.Casr.Logbook.Types.Aeroplane.RPACategory
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
