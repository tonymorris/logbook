{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory where

import Control.Applicative(Applicative(pure))
import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
import Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear
import Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory
import GHC.Generics
import Prelude

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
  -- | Simulator
  deriving Generic

makeClassy ''AircraftCategory
makeClassyPrisms ''AircraftCategory

__Aeroplane ::
  Prism (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures) (AircraftCategory cylinders displacement jettype position vtol rotors landinggear' aeroplanedesignfeatures' airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures) (Propulsions1 cylinders displacement jettype position vtol, landinggear LandingGear, aeroplanedesignfeatures AeroplaneDesignFeatures) (Propulsions1 cylinders displacement jettype position vtol, landinggear' LandingGear, aeroplanedesignfeatures' AeroplaneDesignFeatures)
__Aeroplane =
  prism
    (\(p, g, f) -> Aeroplane p g f)
    (
      \case
        Aeroplane p g f -> Right (p, g, f)
        Helicopter p f -> Left (Helicopter p f)
        PoweredLift p -> Left (PoweredLift p)
        Gyroplane p f -> Left (Gyroplane p f)
        Airship p f -> Left (Airship p f)
        Balloon -> Left Balloon
        RPA p -> Left (RPA p)
        Glider p -> Left (Glider p)
        Paraglider -> Left Paraglider
        Paramotor p -> Left (Paramotor p)
        Trike p -> Left (Trike p)
        PoweredParachute p -> Left (PoweredParachute p)
        Hangglider -> Left Hangglider
    )

__Helicopter ::
  Prism (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures) (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures') (Propulsions1 cylinders displacement jettype position vtol, helicopterdesignfeatures HelicopterDesignFeatures) (Propulsions1 cylinders displacement jettype position vtol, helicopterdesignfeatures' HelicopterDesignFeatures)
__Helicopter =
  prism
    (\(p, f) -> Helicopter p f)
    (
      \case
        Aeroplane p g f -> Left (Aeroplane p g f)
        Helicopter p f -> Right (p, f)
        PoweredLift p -> Left (PoweredLift p)
        Gyroplane p f -> Left (Gyroplane p f)
        Airship p f -> Left (Airship p f)
        Balloon -> Left Balloon
        RPA p -> Left (RPA p)
        Glider p -> Left (Glider p)
        Paraglider -> Left Paraglider
        Paramotor p -> Left (Paramotor p)
        Trike p -> Left (Trike p)
        PoweredParachute p -> Left (PoweredParachute p)
        Hangglider -> Left Hangglider
    )

__Gyroplane ::
  Prism (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures) (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures' helicopterdesignfeatures) (Propulsions1 cylinders displacement jettype position vtol, gyroplanedesignfeatures GyroplaneDesignFeatures) (Propulsions1 cylinders displacement jettype position vtol, gyroplanedesignfeatures' GyroplaneDesignFeatures)
__Gyroplane =
  prism
    (\(p, f) -> Gyroplane p f)
    (
      \case
        Aeroplane p g f -> Left (Aeroplane p g f)
        Helicopter p f -> Left (Helicopter p f)
        PoweredLift p -> Left (PoweredLift p)
        Gyroplane p f -> Right (p, f)
        Airship p f -> Left (Airship p f)
        Balloon -> Left Balloon
        RPA p -> Left (RPA p)
        Glider p -> Left (Glider p)
        Paraglider -> Left Paraglider
        Paramotor p -> Left (Paramotor p)
        Trike p -> Left (Trike p)
        PoweredParachute p -> Left (PoweredParachute p)
        Hangglider -> Left Hangglider
    )

__Airship ::
  Prism (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures) (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures' gyroplanedesignfeatures helicopterdesignfeatures) (Propulsions1 cylinders displacement jettype position vtol, airshipdesignfeatures AirshipDesignFeatures) (Propulsions1 cylinders displacement jettype position vtol, airshipdesignfeatures' AirshipDesignFeatures)
__Airship =
  prism
    (\(p, f) -> Airship p f)
    (
      \case
        Aeroplane p g f -> Left (Aeroplane p g f)
        Helicopter p f -> Left (Helicopter p f)
        PoweredLift p -> Left (PoweredLift p)
        Gyroplane p f -> Left (Gyroplane p f)
        Airship p f -> Right (p, f)
        Balloon -> Left Balloon
        RPA p -> Left (RPA p)
        Glider p -> Left (Glider p)
        Paraglider -> Left Paraglider
        Paramotor p -> Left (Paramotor p)
        Trike p -> Left (Trike p)
        PoweredParachute p -> Left (PoweredParachute p)
        Hangglider -> Left Hangglider
    )

__RPA ::
  Prism (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures) (AircraftCategory cylinders displacement jettype position vtol rotors' landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures) (RPACategory cylinders displacement jettype position vtol rotors) (RPACategory cylinders displacement jettype position vtol rotors')
__RPA =
  prism
    RPA
    (
      \case
        Aeroplane p g f -> Left (Aeroplane p g f)
        Helicopter p f -> Left (Helicopter p f)
        PoweredLift p -> Left (PoweredLift p)
        Gyroplane p f -> Left (Gyroplane p f)
        Airship p f -> Left (Airship p f)
        Balloon -> Left Balloon
        RPA p -> Right p
        Glider p -> Left (Glider p)
        Paraglider -> Left Paraglider
        Paramotor p -> Left (Paramotor p)
        Trike p -> Left (Trike p)
        PoweredParachute p -> Left (PoweredParachute p)
        Hangglider -> Left Hangglider
    )

type AircraftCategory' a =
  AircraftCategory a a a a a a a a a a a
  
type AircraftCategoryI =
  AircraftCategory' Identity

deriving instance (Eq (cylinders Positive), Eq (displacement Positive), Eq (jettype JetType), Eq (position PropulsionPosition), Eq (vtol Bool), Eq (landinggear LandingGear), Eq (aeroplanedesignfeatures AeroplaneDesignFeatures), Eq (helicopterdesignfeatures HelicopterDesignFeatures), Eq (gyroplanedesignfeatures GyroplaneDesignFeatures), Eq (airshipdesignfeatures AirshipDesignFeatures), Eq (rotors Positive)) => Eq (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)

deriving instance (Ord (cylinders Positive), Ord (displacement Positive), Ord (jettype JetType), Ord (position PropulsionPosition), Ord (vtol Bool), Ord (landinggear LandingGear), Ord (aeroplanedesignfeatures AeroplaneDesignFeatures), Ord (helicopterdesignfeatures HelicopterDesignFeatures), Ord (gyroplanedesignfeatures GyroplaneDesignFeatures), Ord (airshipdesignfeatures AirshipDesignFeatures), Ord (rotors Positive)) => Ord (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)

deriving instance (Show (cylinders Positive), Show (displacement Positive), Show (jettype JetType), Show (position PropulsionPosition), Show (vtol Bool), Show (landinggear LandingGear), Show (aeroplanedesignfeatures AeroplaneDesignFeatures), Show (helicopterdesignfeatures HelicopterDesignFeatures), Show (gyroplanedesignfeatures GyroplaneDesignFeatures), Show (airshipdesignfeatures AirshipDesignFeatures), Show (rotors Positive)) => Show (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)

aeroplaneAircraftCategoryI ::
  (Applicative landinggear, Applicative aeroplanedesignfeatures) =>
  Propulsions1 cylinders displacement jettype position vtol
  -> LandingGear
  -> AeroplaneDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
aeroplaneAircraftCategoryI propulsions1_ landinggear_ aeroplanedesignfeatures_ =
  Aeroplane propulsions1_ (pure landinggear_) (pure aeroplanedesignfeatures_)

helicopterAircraftCategoryI ::
  Applicative helicopterdesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> HelicopterDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
helicopterAircraftCategoryI propulsions1_ helicopterdesignfeatures_ =
  Helicopter propulsions1_ (pure helicopterdesignfeatures_)

gyroplaneAircraftCategoryI ::
  Applicative gyroplanedesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> GyroplaneDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
gyroplaneAircraftCategoryI propulsions1_ gyroplanedesignfeatures_ =
  Gyroplane propulsions1_ (pure gyroplanedesignfeatures_)

airshipAircraftCategoryI ::
  Applicative airshipdesignfeatures =>
  Propulsions1 cylinders displacement jettype position vtol
  -> AirshipDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
airshipAircraftCategoryI propulsions1_ airshipdesignfeatures_ =
  Airship propulsions1_ (pure airshipdesignfeatures_)

singleEnginePistonCentrelineNovtolAeroplaneCategory ::
  (Applicative cylinders, Applicative displacement, Applicative position, Applicative vtol, Applicative landinggear, Applicative aeroplanedesignfeatures) =>
  Positive
  -> Positive
  -> LandingGear
  -> AeroplaneDesignFeatures
  -> AircraftCategory cylinders displacement jettype position vtol rotos landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures
singleEnginePistonCentrelineNovtolAeroplaneCategory cylinders displacement landinggear aeroplanedesignfeatures =
  Aeroplane (singlePropulsions1 (Propulsion (Piston (pure cylinders) (pure displacement)) (pure Centreline) (pure False))) (pure landinggear) (pure aeroplanedesignfeatures)
