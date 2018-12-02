{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration where

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
  RAAusRegistrationType
  -> RAAusRegistrationPrefix
  -> DecDigits4
  -> RAAusRegistrationI
raAusRegistrationI raausregistrationtype prefix digits4 =
  RAAusRegistration (Identity raausregistrationtype) (Identity prefix) (Identity digits4)
