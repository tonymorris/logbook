{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.MTOW where

import Control.Lens
import Data.Semigroup
import Natural
import Prelude

newtype MTOW =
  MTOW
    Positive
  deriving (Eq, Ord, Show, Semigroup, Monoid)

makeWrapped ''MTOW
makeClassy ''MTOW

class AsMTOW a where
  _MTOW ::
    Prism' a MTOW

instance AsMTOW MTOW where
  _MTOW =
    id
