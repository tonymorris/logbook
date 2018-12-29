{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsions1 where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion
import Data.List.NonEmpty
import GHC.Generics
import Prelude

newtype Propulsions1 =
  Propulsions1
    (NonEmpty Propulsion)
  deriving (Eq, Ord, Show, Generic)

makeWrapped ''Propulsions1
makeClassy ''Propulsions1

class AsPropulsions1 a where
  _Propulsions1 ::
    Prism' a Propulsions1

instance AsPropulsions1 Propulsions1 where
  _Propulsions1 =
    id
