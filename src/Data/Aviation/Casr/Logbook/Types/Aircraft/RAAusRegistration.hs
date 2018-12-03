{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration where

import Control.Applicative(Applicative(pure))
import Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigits4
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType
import Data.Functor.Identity
import GHC.Generics
import Prelude

data RAAusRegistration raausregistrationtype prefix digits4 =
  RAAusRegistration
    (raausregistrationtype RAAusRegistrationType)
    (prefix RAAusRegistrationPrefix)
    (digits4 DecDigits4)
  deriving Generic

deriving instance (Eq (raausregistrationtype RAAusRegistrationType), Eq (prefix RAAusRegistrationPrefix), Eq (digits4 DecDigits4)) => Eq (RAAusRegistration raausregistrationtype prefix digits4)

deriving instance (Ord (raausregistrationtype RAAusRegistrationType), Ord (prefix RAAusRegistrationPrefix), Ord (digits4 DecDigits4)) => Ord (RAAusRegistration raausregistrationtype prefix digits4)

deriving instance (Show (raausregistrationtype RAAusRegistrationType), Show (prefix RAAusRegistrationPrefix), Show (digits4 DecDigits4)) => Show (RAAusRegistration raausregistrationtype prefix digits4)

type RAAusRegistration' a =
  RAAusRegistration a a a

type RAAusRegistrationI =
  RAAusRegistration' Identity

raAusRegistrationI ::
  (Applicative raausregistrationtype, Applicative prefix, Applicative digits4) =>
  RAAusRegistrationType
  -> RAAusRegistrationPrefix
  -> DecDigits4
  -> RAAusRegistration raausregistrationtype prefix digits4
raAusRegistrationI raausregistrationtype prefix digits4 =
  RAAusRegistration (pure raausregistrationtype) (pure prefix) (pure digits4)
