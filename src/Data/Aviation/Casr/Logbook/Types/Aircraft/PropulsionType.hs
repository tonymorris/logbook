{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType where

import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import GHC.Generics
import Natural
import Prelude
import Data.Functor.Classes(Eq1, eq1, Show1, showsPrec1, Ord1, compare1)

data PropulsionType cylinders displacement jettype =
  Piston (cylinders Natural) (displacement Natural) -- cc
  | Jet (jettype JetType)
  | Electric
  | Rocket
  deriving Generic

instance (Eq1 cylinders, Eq1 displacement, Eq1 jettype) => Eq (PropulsionType cylinders displacement jettype) where
  Piston x1 y1 == Piston x2 y2 =
    and [x1 `eq1` x2, y1 `eq1` y2]
  Jet x1 == Jet x2 =
    x1 `eq1` x2
  Electric == Electric =
    True
  Rocket == Rocket =
    True
  _ == _ =
    False

instance (Ord1 cylinders, Ord1 displacement, Ord1 jettype) => Ord (PropulsionType cylinders displacement jettype) where
  Piston x1 y1 `compare` Piston x2 y2 =
    mconcat [x1 `compare1` x2, y1 `compare1` y2]
  Jet x1 `compare` Jet x2 =
    x1 `compare1` x2
  Electric `compare` Electric =
    EQ
  Rocket `compare` Rocket =
    EQ
  Piston _ _ `compare` _ =
    LT
  Jet _ `compare` _ =
    LT
  Electric `compare` _ =
    LT
  Rocket `compare` _ =
    LT

instance (Show1 cylinders, Show1 displacement, Show1 jettype) => Show (PropulsionType cylinders displacement jettype) where
  showsPrec n (Piston x y) =
    ("Piston " ++) . showsPrec1 n x . (' ':) . showsPrec1 n y
  showsPrec n (Jet x) =
    ("Jet " ++) . showsPrec1 n x
  showsPrec _ Electric =
    ("Electric" ++)
  showsPrec _ Rocket =
    ("Rocket" ++)
