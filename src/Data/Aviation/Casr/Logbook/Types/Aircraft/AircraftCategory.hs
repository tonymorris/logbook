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
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
import Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear
import Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory
import GHC.Generics
import Prelude

data AircraftCategory =
  Aeroplane
    Propulsions1
    LandingGear
    AeroplaneDesignFeatures
  | Helicopter Propulsions1 HelicopterDesignFeatures
  | PoweredLift Propulsions1
  | Gyroplane Propulsions1 GyroplaneDesignFeatures
  | Airship Propulsions1 AirshipDesignFeatures
  | Balloon
  | RPA RPACategory
  | Glider Propulsions
  | Paraglider
  | Paramotor Propulsions1
  | Trike Propulsions
  | PoweredParachute Propulsions1
  | Hangglider
  deriving (Eq, Ord, Show, Generic)

makeClassy ''AircraftCategory
makeClassyPrisms ''AircraftCategory

singleEnginePistonCentrelineNovtolAeroplaneCategory ::
  Positive
  -> Positive
  -> LandingGear
  -> AeroplaneDesignFeatures
  -> AircraftCategory
singleEnginePistonCentrelineNovtolAeroplaneCategory cylinders displacement landinggear aeroplanedesignfeatures =
  Aeroplane (Propulsions1 (pure (Propulsion (Piston cylinders displacement) Centreline False))) landinggear aeroplanedesignfeatures
