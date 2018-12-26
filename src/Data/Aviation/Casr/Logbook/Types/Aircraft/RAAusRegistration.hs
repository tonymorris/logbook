{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistration where

import Control.Applicative(Applicative(pure))
import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigits4
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix
import Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType
import GHC.Generics
import Prelude

data RAAusRegistration raausregistrationtype prefix digits4 =
  RAAusRegistration {
    _raaus_registration_type ::
      raausregistrationtype RAAusRegistrationType
  , raaus_registration_prefix ::
      prefix RAAusRegistrationPrefix
  , _raaus_registration_digits4 ::
      digits4 DecDigits4
  } deriving Generic

makeClassy ''RAAusRegistration
  
__raaus_registration_type ::
  Lens (RAAusRegistration raausregistrationtype prefix digits4) (RAAusRegistration raausregistrationtype' prefix digits4) (raausregistrationtype RAAusRegistrationType) (raausregistrationtype' RAAusRegistrationType)
__raaus_registration_type f (RAAusRegistration t p d) =
  fmap (\t' -> RAAusRegistration t' p d) (f t)

__raaus_registration_prefix ::
  Lens (RAAusRegistration raausregistrationtype prefix digits4) (RAAusRegistration raausregistrationtype prefix' digits4) (prefix RAAusRegistrationPrefix) (prefix' RAAusRegistrationPrefix)
__raaus_registration_prefix f (RAAusRegistration t p d) =
  fmap (\p' -> RAAusRegistration t p' d) (f p)

__raaus_registration_digits4 ::
  Lens (RAAusRegistration raausregistrationtype prefix digits4) (RAAusRegistration raausregistrationtype prefix digits4') (digits4 DecDigits4) (digits4' DecDigits4)
__raaus_registration_digits4 f (RAAusRegistration t p d) =
  fmap (\d' -> RAAusRegistration t p d') (f d)

class AsRAAusRegistration a raausregistrationtype prefix digits4 | a -> raausregistrationtype prefix digits4 where
  _RAAusRegistration ::
    Prism' a (RAAusRegistration raausregistrationtype prefix digits4)

instance AsRAAusRegistration (RAAusRegistration raausregistrationtype prefix digits4) raausregistrationtype prefix digits4 where
  _RAAusRegistration =
    id

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
