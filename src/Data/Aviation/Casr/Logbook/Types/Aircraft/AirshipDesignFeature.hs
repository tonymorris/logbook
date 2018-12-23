{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.AirshipDesignFeature where

import Control.Lens
import GHC.Generics
import Prelude

data AirshipDesignFeature =
  -- gas turbine is covered by propulsion
  PressurisedAirshipDesignFeature
  deriving (Eq, Ord, Show, Generic)

class ManyAirshipDesignFeature a where
  _AirshipDesignFeature_ ::
    Traversal' a AirshipDesignFeature

instance ManyAirshipDesignFeature AirshipDesignFeature where
  _AirshipDesignFeature_ =
    id
    
class ManyAirshipDesignFeature a => HasAirshipDesignFeature a where
  airshipDesignFeature ::
    Lens' a AirshipDesignFeature

instance HasAirshipDesignFeature AirshipDesignFeature where
  airshipDesignFeature =
    id

class ManyAirshipDesignFeature a => AsAirshipDesignFeature a where
  _AirshipDesignFeature ::
    Prism' a AirshipDesignFeature
  _PressurisedAirshipDesignFeature ::
    Prism' a ()
  
instance AsAirshipDesignFeature AirshipDesignFeature where
  _AirshipDesignFeature =
    id
  _PressurisedAirshipDesignFeature =
    prism'
      (\() -> PressurisedAirshipDesignFeature)
      (\case
        PressurisedAirshipDesignFeature ->
          Just ())
  