{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix where

import Control.Lens
import GHC.Generics
import Prelude

data RAAusRegistrationPrefix =
  RAAusRegistrationPrefix10
  | RAAusRegistrationPrefix19
  | RAAusRegistrationPrefix24
  | RAAusRegistrationPrefix28
  | RAAusRegistrationPrefix32
  | RAAusRegistrationPrefix55
  deriving (Eq, Ord, Show, Generic)

makeClassy ''RAAusRegistrationPrefix
makeClassyPrisms ''RAAusRegistrationPrefix
