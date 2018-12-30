{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Rotors where

import Control.Lens
import GHC.Generics
import Natural
import Prelude

newtype Rotors =
  Rotors
    Positive
  deriving (Eq, Ord, Show, Generic)

makeWrapped ''Rotors
makeClassy ''Rotors

class AsRotors a where
  _Rotors ::
    Prism' a Rotors

instance AsRotors Rotors where
  _Rotors =
    id
