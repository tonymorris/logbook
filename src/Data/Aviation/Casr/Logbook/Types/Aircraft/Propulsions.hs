{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import GHC.Generics
import Prelude

newtype Propulsions =
  Propulsions
    [Propulsion]
  deriving (Eq, Ord, Show, Generic)

makeWrapped ''Propulsions
makeClassy ''Propulsions

class AsPropulsions a where
  _Propulsions ::
    Prism' a Propulsions

instance AsPropulsions Propulsions where
  _Propulsions =
    id

singlePropulsions ::
  Propulsion
  -> Propulsions
singlePropulsions propulsion_ =
  Propulsions [propulsion_]
