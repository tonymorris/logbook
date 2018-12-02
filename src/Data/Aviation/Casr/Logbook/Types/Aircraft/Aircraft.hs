{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Aircraft where

import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftRegistration
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory
import Data.Functor.Identity
import GHC.Generics

data Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures =
  Aircraft
    (aircraftregistration (AircraftRegistration otherregistration raausregistration casaregistration raausregistrationtype prefix digits4))
    (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))
  deriving Generic

type Aircraft' a =
  Aircraft a a a a a a a a a a a a a a a a a a a

type AircraftI =
  Aircraft' Identity
  
aircraftI ::
  AircraftRegistrationI
  -> AircraftCategoryI
  -> AircraftI
aircraftI aircraftregistration aircraftcategory =
  Aircraft (Identity aircraftregistration) (Identity aircraftcategory)
