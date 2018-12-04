{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Aircraft where

import Control.Applicative(Applicative(pure))
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftRegistration
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory
import Data.Aviation.Casr.Logbook.Types.Aircraft.MTOW
import Data.Functor.Identity
import GHC.Generics
import Prelude

data Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow =
  Aircraft
    (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4))
    (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow))
  deriving Generic

deriving instance (Eq (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Eq (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow))), Eq (aeroplanemtow MTOW)) => Eq (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow)

deriving instance (Ord (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Ord (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow))), Ord (aeroplanemtow MTOW)) => Ord (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow)

deriving instance (Show (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Show (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow)))) => Show (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures aeroplanemtow)

type Aircraft' a =
  Aircraft a a a a a a a a a a a a a a a a a a a a

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
        aeroplanemtow
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
        aeroplanemtow
aircraftI aircraftregistration aircraftcategory =
  Aircraft (pure aircraftregistration) (pure aircraftcategory)
