{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.JetType where

import Control.Lens
import GHC.Generics
import Prelude

data JetType =
  Turbojet
  | Turbofan
  | Turboprop
  | Ramjet
  | Scramjet
  deriving (Eq, Ord, Show, Generic)

makeClassy ''JetType
makeClassyPrisms ''JetType
