{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType(
  module Natural
, PropulsionType(..)
, HasPropulsionType(..)
, AsPropulsionType(..)
) where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import GHC.Generics
import Natural as Natural
import Prelude

data PropulsionType =
  Piston Positive Positive -- cc
  | Jet JetType
  | Electric
  | Rocket
  deriving (Eq, Ord, Show, Generic)

makeClassy ''PropulsionType
makeClassyPrisms ''PropulsionType
