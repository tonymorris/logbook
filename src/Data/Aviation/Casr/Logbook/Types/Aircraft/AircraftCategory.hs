{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.AeroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.GyroplaneDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.HelicopterDesignFeatures
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.LandingGear
import Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory
import GHC.Generics
import Prelude

data AircraftCategory =
  Aeroplane
    (Propulsions1_ () () () () () () () () () () () () () () () () () ())
    LandingGear
    AeroplaneDesignFeatures
  | Helicopter (Propulsions1_ () () () () () () () () () () () () () () () () () ()) HelicopterDesignFeatures
  | PoweredLift (Propulsions1_ () () () () () () () () () () () () () () () () () ())
  | Gyroplane (Propulsions1_ () () () () () () () () () () () () () () () () () ()) GyroplaneDesignFeatures
  | Airship (Propulsions1_ () () () () () () () () () () () () () () () () () ()) AirshipDesignFeatures
  | Balloon
  | RPA RPACategory
  | Glider (Propulsions_ () () () () () () () () () () () () () () () () () ())
  | Paraglider
  | Paramotor (Propulsions1_ () () () () () () () () () () () () () () () () () ())
  | Trike (Propulsions_ () () () () () () () () () () () () () () () () () ())
  | PoweredParachute (Propulsions1_ () () () () () () () () () () () () () () () () () ())
  | Hangglider
  deriving (Eq, Ord, Show, Generic)

makeClassy ''AircraftCategory
makeClassyPrisms ''AircraftCategory
