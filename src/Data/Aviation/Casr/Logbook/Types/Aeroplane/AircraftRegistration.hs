{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.AircraftRegistration where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.RAAusRegistration
import Data.Aviation.Casr.Logbook.Types.Aeroplane.CASARegistration
import GHC.Generics
import Prelude

data AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 =
  RAAusAircraftRegistration (raausregistration (RAAusRegistration raausregistrationtype prefix digits4))
  | CASAAircraftRegistration (casaregistration CASARegistration)
  | OtherAircraftRegistration (otherregistration String)
  deriving Generic
