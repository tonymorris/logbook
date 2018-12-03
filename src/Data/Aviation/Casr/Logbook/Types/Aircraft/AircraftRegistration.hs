{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

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

deriving instance (Eq (raausregistration (RAAusRegistration raausregistrationtype prefix digits4)), Eq (casaregistration CASARegistration), Eq (otherregistration String)) => Eq (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)

deriving instance (Ord (raausregistration (RAAusRegistration raausregistrationtype prefix digits4)), Ord (casaregistration CASARegistration), Ord (otherregistration String)) => Ord (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)

deriving instance (Show (raausregistration (RAAusRegistration raausregistrationtype prefix digits4)), Show (casaregistration CASARegistration), Show (otherregistration String)) => Show (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4)

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
