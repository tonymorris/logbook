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

data Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow =
  Aircraft
    (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4))
    (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))
    (mtow MTOW)
  deriving Generic

deriving instance (Eq (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Eq (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))), Eq (mtow MTOW)) => Eq (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow)

deriving instance (Ord (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Ord (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))), Ord (mtow MTOW)) => Ord (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow)

deriving instance (Show (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Show (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))), Show (mtow MTOW)) => Show (Aircraft aircraftregistration otherregistration aircraftcategory raausregistration casaregistration raausregistrationtype prefix digits4 cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow)

type Aircraft' a =
  Aircraft a a a a a a a a a a a a a a a a a a a a

type AircraftI =
  Aircraft' Identity

aircraftI ::
  (Applicative aircraftcategory, Applicative aircraftregistration, Applicative mtow) =>
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
  ->  MTOW
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
        mtow
aircraftI aircraftregistration aircraftcategory mtow =
  Aircraft (pure aircraftregistration) (pure aircraftcategory) (pure mtow)
