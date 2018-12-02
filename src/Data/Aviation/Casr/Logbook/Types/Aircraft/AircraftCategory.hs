{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory where

import Control.Applicative(Applicative(pure))
import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
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
  (Applicative landinggear, Applicative aeroplanedesignfeatures) =>
  Propulsions1 cylinders displacement jettype position vtol
  -> LandingGear
  -> AeroplaneDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
aeroplaneAircraftCategoryI propulsions1 landinggear aeroplanedesignfeatures =
  Aeroplane propulsions1 (pure landinggear) (pure aeroplanedesignfeatures)

helicopterAircraftCategoryI ::
  Applicative helicopterdesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> HelicopterDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
helicopterAircraftCategoryI propulsions1 helicopterdesignfeatures =
  Helicopter propulsions1 (pure helicopterdesignfeatures)

gyroplaneAircraftCategoryI ::
  Applicative gyroplanedesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> GyroplaneDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
gyroplaneAircraftCategoryI propulsions1 gyroplanedesignfeatures =
  Gyroplane propulsions1 (pure gyroplanedesignfeatures)

airshipAircraftCategoryI ::
  Applicative airshipdesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> AirshipDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
airshipAircraftCategoryI propulsions1 airshipdesignfeatures =
  Airship propulsions1 (pure airshipdesignfeatures)

singleEnginePistonCentrelineNovtolAeroplaneCategory ::
  (Applicative cylinders, Applicative displacement, Applicative position, Applicative vtol, Applicative landinggear, Applicative aeroplanedesignfeatures) =>
  Positive
  -> Positive
  -> LandingGear
  -> AeroplaneDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotos landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
singleEnginePistonCentrelineNovtolAeroplaneCategory cylinders displacement landinggear aeroplanedesignfeatures =
  Aeroplane (singlePropulsions1 (Propulsion (Piston (pure cylinders) (pure displacement)) (pure Centreline) (pure False))) (pure landinggear) (pure aeroplanedesignfeatures)
