{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.Aircraft where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.AircraftRegistration
import Data.Aviation.Casr.Logbook.Types.Aeroplane.AircraftCategory
import GHC.Generics

data Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeature airshipdesignfeature gyroplanedesignfeature helicopterdesignfeature =
  Aircraft
    (aircraftregistration (AircraftRegistration otherregistration raausregistration casaregistration raausregistrationtype prefix digits4))
    (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeature airshipdesignfeature gyroplanedesignfeature helicopterdesignfeature))
  deriving Generic