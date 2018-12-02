{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration where

import Control.Applicative(Applicative(pure))
import Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigits4
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType
import Data.Functor.Identity
import GHC.Generics

data RAAusRegistration raausregistrationtype prefix digits4 =
  RAAusRegistration
    (raausregistrationtype RAAusRegistrationType)
    (prefix RAAusRegistrationPrefix)
    (digits4 DecDigits4)
  deriving Generic

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
