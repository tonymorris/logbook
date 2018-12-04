{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory where

import Control.Applicative(Applicative(pure))
import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import Data.Aviation.Casr.Logbook.Types.Aircraft.MTOW
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
import Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear
import Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory
import Data.Functor.Identity
import GHC.Generics
import Prelude

-- todo insert MTOW

data AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow =
  Aeroplane
    (Propulsions1 cylinders displacement jettype position vtol)
    (landinggear LandingGear)
    (aeroplanedesignfeatures AeroplaneDesignFeatures)
    (aeroplanemtow MTOW)
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

deriving instance (Eq (cylinders Positive), Eq (displacement Positive), Eq (jettype JetType), Eq (position PropulsionPosition), Eq (vtol Bool), Eq (landinggear LandingGear), Eq (aeroplanedesignfeatures AeroplaneDesignFeatures), Eq (helicopterdesignfeatures HelicopterDesignFeatures), Eq (gyroplanedesignfeatures GyroplaneDesignFeatures), Eq (airshipdesignfeatures AirshipDesignFeatures), Eq (rotors Positive), Eq (aeroplanemtow MTOW)) => Eq (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow)

deriving instance (Ord (cylinders Positive), Ord (displacement Positive), Ord (jettype JetType), Ord (position PropulsionPosition), Ord (vtol Bool), Ord (landinggear LandingGear), Ord (aeroplanedesignfeatures AeroplaneDesignFeatures), Ord (helicopterdesignfeatures HelicopterDesignFeatures), Ord (gyroplanedesignfeatures GyroplaneDesignFeatures), Ord (airshipdesignfeatures AirshipDesignFeatures), Ord (rotors Positive), Ord (aeroplanemtow MTOW)) => Ord (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow)

deriving instance (Show (cylinders Positive), Show (displacement Positive), Show (jettype JetType), Show (position PropulsionPosition), Show (vtol Bool), Show (landinggear LandingGear), Show (aeroplanedesignfeatures AeroplaneDesignFeatures), Show (helicopterdesignfeatures HelicopterDesignFeatures), Show (gyroplanedesignfeatures GyroplaneDesignFeatures), Show (airshipdesignfeatures AirshipDesignFeatures), Show (rotors Positive), Show (aeroplanemtow MTOW)) => Show (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow)

aeroplaneAircraftCategoryI ::
  (Applicative landinggear, Applicative aeroplanedesignfeatures, Applicative aeroplanemtow) =>
  Propulsions1 cylinders displacement jettype position vtol
  -> LandingGear
  -> AeroplaneDesignFeatures
  -> MTOW
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow
aeroplaneAircraftCategoryI propulsions1 landinggear aeroplanedesignfeatures mtow =
  Aeroplane propulsions1 (pure landinggear) (pure aeroplanedesignfeatures) (pure mtow)

helicopterAircraftCategoryI ::
  Applicative helicopterdesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> HelicopterDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow
helicopterAircraftCategoryI propulsions1 helicopterdesignfeatures =
  Helicopter propulsions1 (pure helicopterdesignfeatures)

gyroplaneAircraftCategoryI ::
  Applicative gyroplanedesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> GyroplaneDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow
gyroplaneAircraftCategoryI propulsions1 gyroplanedesignfeatures =
  Gyroplane propulsions1 (pure gyroplanedesignfeatures)

airshipAircraftCategoryI ::
  Applicative airshipdesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> AirshipDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow
airshipAircraftCategoryI propulsions1 airshipdesignfeatures =
  Airship propulsions1 (pure airshipdesignfeatures)

singleEnginePistonCentrelineNovtolAeroplaneCategory ::
  (Applicative cylinders, Applicative displacement, Applicative position, Applicative vtol, Applicative landinggear, Applicative aeroplanedesignfeatures, Applicative aeroplanemtow) =>
  Positive
  -> Positive
  -> LandingGear
  -> AeroplaneDesignFeatures
  -> MTOW
  -> AircraftCategory cylinders displacement jettype position vtol rotos landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow
singleEnginePistonCentrelineNovtolAeroplaneCategory cylinders displacement landinggear aeroplanedesignfeatures mtow =
  Aeroplane (singlePropulsions1 (Propulsion (Piston (pure cylinders) (pure displacement)) (pure Centreline) (pure False))) (pure landinggear) (pure aeroplanedesignfeatures) (pure mtow)
