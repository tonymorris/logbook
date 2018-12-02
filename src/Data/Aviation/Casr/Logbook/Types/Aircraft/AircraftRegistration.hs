{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftRegistration where

import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration
import Data.Aviation.Casr.Logbook.Types.Aircraft.CASARegistration
import Data.Functor.Identity
import GHC.Generics
import Prelude

data AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 =
  RAAusAircraftRegistration (raausregistration (RAAusRegistration raausregistrationtype prefix digits4))
  | CASAAircraftRegistration (casaregistration CASARegistration)
  | OtherAircraftRegistration (otherregistration String)
  deriving Generic

type AircraftRegistration' a =
  AircraftRegistration a a a a a a
  
type AircraftRegistrationI =
  AircraftRegistration' Identity

raAusAircraftRegistrationI ::
  Applicative raausregistration =>
  RAAusRegistration raausregistrationtype prefix digits4
  -> AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4
raAusAircraftRegistrationI raausregistration =
  RAAusAircraftRegistration (pure raausregistration)

casaAircraftRegistrationI ::
  Applicative casaregistration =>
  CASARegistration
  -> AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4
casaAircraftRegistrationI casaregistration =
  CASAAircraftRegistration (pure casaregistration)

otherAircraftRegistrationI ::
  Applicative otherregistration =>
  String
  -> AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4
otherAircraftRegistrationI otherregistration =
  OtherAircraftRegistration (pure otherregistration)
