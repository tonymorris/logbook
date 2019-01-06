{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsions1 where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Engine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineConfiguration
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsion
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineAirInduction
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineFuelInduction
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineType
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngine
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PropulsionPosition
import Data.List.NonEmpty
import GHC.Generics
import Natural
import Prelude

type family XPropulsions1 x

data Propulsions1_ x =
  Propulsions1_
    !(XPropulsions1 x)
    (NonEmpty Propulsion)
  deriving Generic

deriving instance Eq (XPropulsions1 x) =>
  Eq (Propulsions1_ x)

deriving instance Ord (XPropulsions1 x) =>
  Ord (Propulsions1_ x)

deriving instance Show (XPropulsions1 x) =>
  Show (Propulsions1_ x)

class HasPropulsions1 a e | a -> e where
  propulsions1 ::
    Lens' a (Propulsions1_ e)
  xPropulsions1 ::
    Lens' a (XPropulsions1 e)
  default xPropulsions1 ::
    Lens' a (XPropulsions1 e)
  xPropulsions1 =
    propulsions1 . xPropulsions1

instance HasPropulsions1 (Propulsions1_ e) e where
  propulsions1 =
    id
  xPropulsions1 f (Propulsions1_ x n) =
    fmap (\x' -> Propulsions1_ x' n) (f x)

class AsPropulsions1 a e | a -> e where
  _Propulsions1 ::
    Prism' a (Propulsions1_ e)
 
instance AsPropulsions1 (Propulsions1_ e) e where
  _Propulsions1 =
    id

instance Propulsions1 ~ x => Rewrapped Propulsions1 x
instance Wrapped Propulsions1 where
  type Unwrapped Propulsions1 =
    NonEmpty Propulsion
  _Wrapped' =
    iso
      (\(Propulsions1_ () x) -> x)
      (Propulsions1_ ())

type Propulsions1 =
  Propulsions1_ ()

type instance XPropulsions1 () =
  ()

pattern Propulsions1 ::
  NonEmpty Propulsion
  -> Propulsions1
pattern Propulsions1 p <- Propulsions1_ _ p
  where Propulsions1 p = Propulsions1_ () p

----

type instance Index Propulsions1 = Int
type instance IxValue Propulsions1 = Propulsion

instance Ixed Propulsions1 where
  ix n =
    _Wrapped . ix n

instance Each Propulsions1 Propulsions1 Propulsion Propulsion where
  each =
    _Wrapped . each

instance Reversing Propulsions1 where
  reversing =
    _Wrapped %~ reversing

single_naturallyinduced_ice_fourstroke ::
  String
  -> String
  -> InternalCombustionEngineFuelInduction
  -> Positive
  -> Positive
  -> Propulsions1
single_naturallyinduced_ice_fourstroke enginemanufacturer enginedesgination fuelinduction cylinders displacement =
  Propulsions1
    ( 
      pure
        (
          Propulsion
            (
              Engine
                enginemanufacturer
                enginedesgination
                (
                  InternalCombustionEngineEngineType
                    (
                      InternalCombustionEngine
                        NaturalInduction
                        fuelinduction
                        Spark
                        (
                          PistonEngineType
                            (
                              PistonEngine
                                Opposed
                                FourStroke
                                cylinders
                                (EngineDisplacement displacement)
                            )
                        )
                    )
                )
            )
            Centreline
        )
    )
    