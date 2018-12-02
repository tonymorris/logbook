{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration where

import Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigit4
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType
import GHC.Generics

data RAAusRegistration raausregistrationtype prefix digits4 =
  RAAusRegistration
    (raausregistrationtype RAAusRegistrationType)
    (prefix RAAusRegistrationPrefix)
    (digits4 DecDigit4)
  deriving Generic
