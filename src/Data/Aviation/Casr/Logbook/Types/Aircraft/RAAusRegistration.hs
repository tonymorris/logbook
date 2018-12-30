{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigits4
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType
import GHC.Generics
import Prelude

data RAAusRegistration =
  RAAusRegistration {
    _raaus_registration_type ::
      RAAusRegistrationType
  , raaus_registration_prefix ::
      RAAusRegistrationPrefix
  , _raaus_registration_digits4 ::
      DecDigits4
  } deriving (Eq, Ord, Show, Generic)

makeClassy ''RAAusRegistration

class AsRAAusRegistration a where
  _RAAusRegistration ::
    Prism' a RAAusRegistration

instance AsRAAusRegistration RAAusRegistration where
  _RAAusRegistration =
    id
