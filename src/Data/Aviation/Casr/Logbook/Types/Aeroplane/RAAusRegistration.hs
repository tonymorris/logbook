{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aeroplane.RAAusRegistration where

import Data.Aviation.Casr.Logbook.Types.Aeroplane.DecDigit4
import Data.Aviation.Casr.Logbook.Types.Aeroplane.RAAusRegistrationPrefix
import Data.Aviation.Casr.Logbook.Types.Aeroplane.RAAusRegistrationType
import GHC.Generics

data RAAusRegistration raausregistrationtype prefix digits4 =
  RAAusRegistration
    (raausregistrationtype RAAusRegistrationType)
    (prefix RAAusRegistrationPrefix)
    (digits4 DecDigit4)
  deriving Generic
