{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.MTOW where

import Control.Lens
import Data.Semigroup
import Natural
import Prelude

newtype MTOW =
  MTOW
    Positive
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class ManyMTOW a where
  _MTOW_ ::
    Traversal' a MTOW

instance ManyMTOW MTOW where
  _MTOW_ =
    id

class ManyMTOW a => HasMTOW a where
  mtow ::
    Lens' a MTOW

instance HasMTOW MTOW where
  mtow =
    id

class ManyMTOW a => AsMTOW a where
  _MTOW ::
    Prism' a MTOW

instance AsMTOW MTOW where
  _MTOW =
    id

instance ManyMTOW Positive where
  _MTOW_ =
    _Unwrapped' . (_MTOW_ :: Traversal' MTOW MTOW)

instance HasMTOW Positive where
  mtow =
    _Unwrapped' . (mtow :: Lens' MTOW MTOW)

instance AsMTOW Positive where
  _MTOW =
    _Unwrapped' . (_MTOW :: Prism' MTOW MTOW)

instance MTOW ~ a =>
  Rewrapped MTOW a

instance Wrapped MTOW where
  type Unwrapped MTOW =
    Positive
  _Wrapped' =
    iso (\ (MTOW x) -> x) MTOW

instance AsPositive MTOW where
  _Positive =
    _Wrapped . _Positive

instance HasPositive MTOW where
  positive =
    _Wrapped . positive
