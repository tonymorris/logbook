{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement where

import Control.Lens
import GHC.Generics
import Natural
import Prelude

newtype EngineDisplacement =
  EngineDisplacement
    Positive
  deriving (Eq, Ord, Show, Generic)

makeWrapped ''EngineDisplacement
makeClassy ''EngineDisplacement

class AsEngineDisplacement a where
  _EngineDisplacement ::
    Prism' a EngineDisplacement

instance AsEngineDisplacement EngineDisplacement where
  _EngineDisplacement =
    id
