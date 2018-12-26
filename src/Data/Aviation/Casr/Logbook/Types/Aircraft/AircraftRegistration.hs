{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftRegistration where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration
import Data.Aviation.Casr.Logbook.Types.Aircraft.CASARegistration
import GHC.Generics
import Prelude

data AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4 =
  RAAusAircraftRegistration (raausregistration (RAAusRegistration raausregistrationtype prefix digits4))
  | CASAAircraftRegistration (casaregistration CASARegistration)
  | OtherAircraftRegistration (otherregistration String)
  deriving Generic

makeClassy ''AircraftRegistration
makeClassyPrisms ''AircraftRegistration

__RAAusAircraftRegistration ::
  Prism (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4) (AircraftRegistration raausregistration' casaregistration otherregistration raausregistrationtype' prefix' digits4') (raausregistration (RAAusRegistration raausregistrationtype prefix digits4)) (raausregistration' (RAAusRegistration raausregistrationtype' prefix' digits4'))
__RAAusAircraftRegistration =
  prism
    RAAusAircraftRegistration
    (
      \case
        RAAusAircraftRegistration r ->
          Right r
        CASAAircraftRegistration r ->
          Left (CASAAircraftRegistration r)
        OtherAircraftRegistration r ->
          Left (OtherAircraftRegistration r)
    )

__CASAAircraftRegistration ::
  Prism (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4) (AircraftRegistration raausregistration casaregistration' otherregistration raausregistrationtype prefix digits4) (casaregistration CASARegistration) (casaregistration' CASARegistration)
__CASAAircraftRegistration =
  prism
    CASAAircraftRegistration
    (
      \case
        RAAusAircraftRegistration r ->
          Left (RAAusAircraftRegistration r)
        CASAAircraftRegistration r ->
          Right r
        OtherAircraftRegistration r ->
          Left (OtherAircraftRegistration r)
    )

__OtherAircraftRegistration ::
  Prism (AircraftRegistration raausregistration casaregistration otherregistration raausregistrationtype prefix digits4) (AircraftRegistration raausregistration casaregistration otherregistration' raausregistrationtype prefix digits4) (otherregistration String) (otherregistration' String)
__OtherAircraftRegistration =
  prism
    OtherAircraftRegistration
    (
      \case
        RAAusAircraftRegistration r ->
          Left (RAAusAircraftRegistration r)
        CASAAircraftRegistration r ->
          Left (CASAAircraftRegistration r)
        OtherAircraftRegistration r ->
          Right r
    )

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
