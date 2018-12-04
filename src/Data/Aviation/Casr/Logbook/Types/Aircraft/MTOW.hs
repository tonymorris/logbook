{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.MTOW(
  MTOW(..)
) where

import Natural
import Prelude

newtype MTOW =
  MTOW
    Positive
  deriving (Eq, Ord, Show)
