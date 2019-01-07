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

data Propulsions1 x xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition =
  Propulsions1
    !(XPropulsions1 x)
    (NonEmpty (Propulsion xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition))
  deriving Generic

deriving instance (Eq (XPropulsions1 x), Eq (Propulsion xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)) =>
  Eq (Propulsions1 x xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)

deriving instance (Ord (XPropulsions1 x), Ord (Propulsion xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)) =>
  Ord (Propulsions1 x xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)

deriving instance (Show (XPropulsions1 x), Show (Propulsion xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)) =>
  Show (Propulsions1 x xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)

class HasPropulsions1 a e xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition | a -> e, a -> xpropulsions1, a -> xpropulsionengine, a -> xengine, a -> xinternalcombustionengine, a -> xinternalcombustionengineairinduction, a -> xinternalcombustionenginefuelinduction, a -> xinternalcombustionengineignition, a -> xinternalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine, a -> xelectrictype, a -> xjettype, a -> xpropulsionposition where
  propulsions1 ::
    Lens' a (Propulsions1 e xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)
  xPropulsions1 ::
    Lens' a (XPropulsions1 e)
  xPropulsions1 =
    propulsions1 . xPropulsions1

instance HasPropulsions1 (Propulsions1 e xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) e xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition where
  propulsions1 =
    id
  xPropulsions1 f (Propulsions1 x n) =
    fmap (\x' -> Propulsions1 x' n) (f x)

class AsPropulsions1 a e xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition | a -> e, a -> xpropulsions1, a -> xpropulsionengine, a -> xengine, a -> xinternalcombustionengine, a -> xinternalcombustionengineairinduction, a -> xinternalcombustionenginefuelinduction, a -> xinternalcombustionengineignition, a -> xinternalcombustionenginetype, a -> xpistonengine, a -> xpistonengineconfiguration, a -> xpistonenginecycle, a -> xenginedisplacement_pistonengine, a -> xrotaryengine, a -> xrotors, a -> xenginedisplacement_rotaryengine, a -> xelectrictype, a -> xjettype, a -> xpropulsionposition where
  _Propulsions1 ::
    Prism' a (Propulsions1 e xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)
 
instance AsPropulsions1 (Propulsions1 e xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) e xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition where
  _Propulsions1 =
    id

instance (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) ~ x => Rewrapped (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) x
instance Wrapped (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) where
  type Unwrapped (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) =
    NonEmpty (Propulsion_ xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)
  _Wrapped' =
    iso
      (\(Propulsions1_ x) -> x)
      Propulsions1_

type Propulsions1_ =
  Propulsions1 ()

type instance XPropulsions1 () =
  ()

pattern Propulsions1_ ::
  NonEmpty (Propulsion xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)
  -> Propulsions1_ xpropulsions1 xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition
pattern Propulsions1_ p <- Propulsions1 _ p
  where Propulsions1_ p = Propulsions1 () p

----

type instance Index (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) = Int
type instance IxValue (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) = (Propulsion_ xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition)

instance Ixed (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) where
  ix n =
    _Wrapped . ix n

instance Each (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) (Propulsion_ xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) (Propulsion_ xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) where
  each =
    _Wrapped . each

instance Reversing (Propulsions1_ () xpropulsionengine xengine xinternalcombustionengine xinternalcombustionengineairinduction xinternalcombustionenginefuelinduction xinternalcombustionengineignition xinternalcombustionenginetype xpistonengine xpistonengineconfiguration xpistonenginecycle xenginedisplacement_pistonengine xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype xpropulsionposition) where
  reversing =
    _Wrapped %~ reversing

single_naturallyinduced_ice_fourstroke :: 
  Manufacturer
  -> Designation
  -> InternalCombustionEngineFuelInduction xinternalcombustionenginefuelinduction
  -> Cylinders
  -> Positive
  -> Propulsions1_ () () () () () xinternalcombustionenginefuelinduction () () () () () () xrotaryengine xrotors xenginedisplacement_rotaryengine xelectrictype xjettype ()
single_naturallyinduced_ice_fourstroke enginemanufacturer enginedesgination fuelinduction cylinders displacement =
  Propulsions1_
    ( 
      pure
        (
          Propulsion_
            (
              Engine_
                enginemanufacturer
                enginedesgination
                (
                  InternalCombustionEngineEngineType_
                    (
                      InternalCombustionEngine_
                        NaturalInduction_
                        fuelinduction
                        Spark_
                        (
                          PistonEngineType_
                            (
                              PistonEngine_
                                Opposed_
                                FourStroke_
                                cylinders
                                (EngineDisplacement_ displacement)
                            )
                        )
                    )
                )
            )
            Centreline_
        )
    )
    