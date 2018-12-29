{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1
import GHC.Generics
import Natural
import Prelude
 
data RPACategory =
  RPAAeroplane Propulsions1
  | RPACopter Propulsions1 Positive
  | RPAAirship Propulsions1
  | RPAPoweredLift Propulsions1
  deriving (Eq, Ord, Show, Generic)

makeClassy ''RPACategory
makeClassyPrisms ''RPACategory
