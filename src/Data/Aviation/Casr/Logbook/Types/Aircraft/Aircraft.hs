{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Aircraft where

import Control.Applicative(Applicative(pure))
import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftRegistration
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory
import Data.Aviation.Casr.Logbook.Types.Aircraft.MTOW
import GHC.Generics
import Prelude

data Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow =
  Aircraft {
    _aircraftregistration :: aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)
  , _aircraftcategory :: aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)
  , _mtow :: mtow MTOW
  } deriving Generic

makeClassy ''Aircraft

__aircraftregistration ::
  Lens (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow) (Aircraft aircraftregistration'  raausregistration' casaregistration' otherregistration' raausregistrationtype' prefix' digits4' aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow) (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)) (aircraftregistration' (AircraftRegistration raausregistration' casaregistration' otherregistration' raausregistrationtype' prefix' digits4'))
__aircraftregistration f (Aircraft aircraftregistration_ aircraftcategory_ mtow_) =
  fmap (\aircraftregistration_' -> Aircraft aircraftregistration_' aircraftcategory_ mtow_) (f aircraftregistration_)

__aircraftcategory ::
  Lens (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow) (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory' cylinders' displacement' jettype' position' vtol' rotors' landinggear' aeroplanedesignfeatures' airshipdesignfeatures' gyroplanedesignfeatures' helicopterdesignfeatures' mtow) (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures)) (aircraftcategory' (AircraftCategory cylinders' displacement' jettype' position' vtol' rotors' landinggear' aeroplanedesignfeatures' airshipdesignfeatures' gyroplanedesignfeatures' helicopterdesignfeatures'))
__aircraftcategory f (Aircraft aircraftregistration_ aircraftcategory_ mtow_) =
  fmap (\aircraftcategory_' -> Aircraft aircraftregistration_ aircraftcategory_' mtow_) (f aircraftcategory_)

__mtow ::
  Lens (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow) (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow') (mtow MTOW) (mtow' MTOW)
__mtow f (Aircraft aircraftregistration_ aircraftcategory_ mtow_) =
  fmap (\mtow_' -> Aircraft aircraftregistration_ aircraftcategory_ mtow_') (f mtow_)

class AsAircraft a aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow | a -> aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow where
  _Aircraft ::
    Prism' a (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow)

instance AsAircraft (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow) aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow where
  _Aircraft =
    id

deriving instance (Eq (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Eq (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))), Eq (mtow MTOW)) => Eq (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow)

deriving instance (Ord (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Ord (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))), Ord (mtow MTOW)) => Ord (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow)

deriving instance (Show (aircraftregistration (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)), (Show (aircraftcategory (AircraftCategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures))), Show (mtow MTOW)) => Show (Aircraft aircraftregistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 aircraftcategory cylinders displacement jettype position vtol rotors landinggear aeroplanedesignfeatures airshipdesignfeatures gyroplanedesignfeatures helicopterdesignfeatures mtow)

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
        raausregistration
        casaregistration
        otherregistration        
        raausregistrationtype
        prefix
        digits4
        aircraftcategory
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
aircraftI aircraftregistration_ aircraftcategory_ mtow_ =
  Aircraft (pure aircraftregistration_) (pure aircraftcategory_) (pure mtow_)
