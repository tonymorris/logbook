{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Aircraft where

import Control.Applicative(Applicative(pure))
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftRegistration
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory
import Data.Functor.Identity
import GHC.Generics
import Prelude

data Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures =
  Aircraft
    (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4))
    (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))
  deriving Generic

deriving instance (Eq (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Eq (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)))) => Eq (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)

deriving instance (Ord (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Ord (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)))) => Ord (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)

deriving instance (Show (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Show (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)))) => Show (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)

type Aircraft' a =
  Aircraft a a a a a a a a a a a a a a a a a a a

type AircraftI =
  Aircraft' Identity

aircraftI ::
  (Applicative aircraftcategory, Applicative aircraftregistration) =>
      AircraftRegistration
        raausregistration
        casaregistration
        otherregistration
        raausregistrationtype
        prefix
        digits4
  ->  AircraftCategory
        cylinders
        displacement
        jettype
        position
        vtol
        rotors
        landinggear
        aeroplanedesignfeatures
        airshipdesignfeatures
        gyroplanedesignfeatures
        helicopterdesignfeatures
  ->  Aircraft
        aircraftregistration
        otherregistration
        aircraftcategory
        raausregistration
        casaregistration
        raausregistrationtype
        prefix
        digits4
        cylinders
        displacement
        jettype
        position
        vtol
        rotors
        landinggear
        aeroplanedesignfeatures
        airshipdesignfeatures
        gyroplanedesignfeatures
        helicopterdesignfeatures
aircraftI aircraftregistration aircraftcategory =
  Aircraft (pure aircraftregistration) (pure aircraftcategory)
