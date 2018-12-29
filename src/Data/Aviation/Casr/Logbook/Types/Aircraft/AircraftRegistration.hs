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

data AircraftRegistration =
  RAAusAircraftRegistration RAAusRegistration
  | CASAAircraftRegistration CASARegistration
  | OtherAircraftRegistration String
  deriving (Eq, Ord, Show, Generic)

makeClassy ''AircraftRegistration
makeClassyPrisms ''AircraftRegistration
