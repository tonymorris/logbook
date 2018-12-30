{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Aircraft where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftRegistration
import Data.Aviation.Casr.Logbook.Types.Aircraft.AircraftCategory
import Data.Aviation.Casr.Logbook.Types.Aircraft.MTOW
import GHC.Generics
import Prelude

data Aircraft =
  Aircraft {
    _aircraftregistration ::
      AircraftRegistration
  , _aircraftcategory ::
      AircraftCategory
  , _mtow ::
      MTOW
  } deriving (Eq, Ord, Show, Generic)

makeClassy ''Aircraft

class AsAircraft a where
  _Aircraft ::
    Prism' a Aircraft

instance AsAircraft Aircraft where
  _Aircraft =
    id
