{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.ElectricType where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XBrushlessAC x
type family XBrushedAC x
type family XBrushlessDC x
type family XBrushedDC x
type family XUniversalACDC x
type family XSwitchedReluctance x
type family XDirectDrive x
type family XLinear x
type family XElectricType x

data ElectricType x =
  BrushlessAC !(XBrushlessAC x)
  | BrushedAC !(XBrushedAC x)
  | BrushlessDC !(XBrushlessDC x)
  | BrushedDC !(XBrushedDC x)
  | UniversalACDC !(XUniversalACDC x)
  | SwitchedReluctance !(XSwitchedReluctance x)
  | DirectDrive !(XDirectDrive x)
  | Linear !(XLinear x)
  | ElectricType !(XElectricType x)
  deriving Generic

deriving instance (Eq (XBrushlessAC x), Eq (XBrushedAC x), Eq (XBrushlessDC x), Eq (XBrushedDC x), Eq (XUniversalACDC x), Eq (XSwitchedReluctance x), Eq (XDirectDrive x), Eq (XLinear x), Eq (XElectricType x)) =>
  Eq (ElectricType x)

deriving instance (Ord (XBrushlessAC x), Ord (XBrushedAC x), Ord (XBrushlessDC x), Ord (XBrushedDC x), Ord (XUniversalACDC x), Ord (XSwitchedReluctance x), Ord (XDirectDrive x), Ord (XLinear x), Ord (XElectricType x)) =>
  Ord (ElectricType x)

deriving instance (Show (XBrushlessAC x), Show (XBrushedAC x), Show (XBrushlessDC x), Show (XBrushedDC x), Show (XUniversalACDC x), Show (XSwitchedReluctance x), Show (XDirectDrive x), Show (XLinear x), Show (XElectricType x)) =>
  Show (ElectricType x)

class HasElectricType a e | a -> e where
  electricType ::
    Lens' a (ElectricType e)
  xElectricType ::
    Lens' a (XElectricType e)
  xElectricType =
    electricType . xElectricType

instance HasElectricType (ElectricType e) e where
  electricType =
    id
  
xElectricType' ::
  (
    XBrushlessAC e ~ x
  , XBrushedAC e ~ x
  , XBrushlessDC e ~ x
  , XBrushedDC e ~ x
  , XUniversalACDC e ~ x
  , XSwitchedReluctance e ~ x
  , XDirectDrive e ~ x
  , XLinear e ~ x
  , XElectricType e ~ Void
  ) =>
  Lens' (ElectricType e) x
xElectricType' f (BrushlessAC x) =
  fmap BrushlessAC (f x)
xElectricType' f (BrushedAC x) =
  fmap BrushedAC (f x)
xElectricType' f (BrushlessDC x) =
  fmap BrushlessDC (f x)
xElectricType' f (BrushedDC x) =
  fmap BrushedDC (f x)
xElectricType' f (UniversalACDC x) =
  fmap UniversalACDC (f x)
xElectricType' f (SwitchedReluctance x) =
  fmap SwitchedReluctance (f x)
xElectricType' f (DirectDrive x) =
  fmap DirectDrive (f x)
xElectricType' f (Linear x) =
  fmap Linear (f x)
xElectricType' _ (ElectricType x) =
  absurd x

class AsElectricType a e | a -> e where
  _ElectricType ::
    Prism' a (ElectricType e)
  _BrushlessAC ::
    Prism' a (XBrushlessAC e)
  _BrushedAC ::
    Prism' a (XBrushedAC e)
  _BrushlessDC ::
    Prism' a (XBrushlessDC e)
  _BrushedDC ::
    Prism' a (XBrushedDC e)
  _UniversalACDC ::
    Prism' a (XUniversalACDC e)
  _SwitchedReluctance ::
    Prism' a (XSwitchedReluctance e)
  _DirectDrive ::
    Prism' a (XDirectDrive e)
  _Linear ::
    Prism' a (XLinear e)
  _XElectricType ::
    Prism' a (XElectricType e)

instance AsElectricType (ElectricType e) e where
  _ElectricType =
    id
  _BrushlessAC =
    prism'
      BrushlessAC
      (
        \case
          BrushlessAC x ->
            Just x
          _ ->
            Nothing
      )
  _BrushedAC =
    prism'
      BrushedAC
      (
        \case
          BrushedAC x ->
            Just x
          _ ->
            Nothing
      )
  _BrushlessDC =
    prism'
      BrushlessDC
      (
        \case
          BrushlessDC x ->
            Just x
          _ ->
            Nothing
      )
  _BrushedDC =
    prism'
      BrushedDC
      (
        \case
          BrushedDC x ->
            Just x
          _ ->
            Nothing
      )
  _UniversalACDC =
    prism'
      UniversalACDC
      (
        \case
          UniversalACDC x ->
            Just x
          _ ->
            Nothing
      )
  _SwitchedReluctance =
    prism'
      SwitchedReluctance
      (
        \case
          SwitchedReluctance x ->
            Just x
          _ ->
            Nothing
      )
  _DirectDrive =
    prism'
      DirectDrive
      (
        \case
          DirectDrive x ->
            Just x
          _ ->
            Nothing
      )
  _Linear =
    prism'
      Linear
      (
        \case
          Linear x ->
            Just x
          _ ->
            Nothing
      )
  _XElectricType =
    prism'
      ElectricType
      (
        \case
          ElectricType x ->
            Just x
          _ ->
            Nothing
      )

type ElectricType_ =
  ElectricType ()

type instance XBrushlessAC () =
  ()
type instance XBrushedAC () =
  ()
type instance XBrushlessDC () =
  ()
type instance XBrushedDC () =
  ()
type instance XUniversalACDC () =
  ()
type instance XSwitchedReluctance () =
  ()
type instance XDirectDrive () =
  ()
type instance XLinear () =
  ()
type instance XElectricType () =
  Void

pattern BrushlessAC_ ::
  ElectricType_
pattern BrushlessAC_ <- BrushlessAC _
  where BrushlessAC_ = BrushlessAC ()

pattern BrushedAC_ ::
  ElectricType_
pattern BrushedAC_ <- BrushedAC _
  where BrushedAC_ = BrushedAC ()

pattern BrushlessDC_ ::
  ElectricType_
pattern BrushlessDC_ <- BrushlessDC _
  where BrushlessDC_ = BrushlessDC ()

pattern BrushedDC_ ::
  ElectricType_
pattern BrushedDC_ <- BrushedDC _
  where BrushedDC_ = BrushedDC ()

pattern UniversalACDC_ ::
  ElectricType_
pattern UniversalACDC_ <- UniversalACDC _
  where UniversalACDC_ = UniversalACDC ()

pattern SwitchedReluctance_ ::
  ElectricType_
pattern SwitchedReluctance_ <- SwitchedReluctance _
  where SwitchedReluctance_ = SwitchedReluctance ()

pattern DirectDrive_ ::
  ElectricType_
pattern DirectDrive_ <- DirectDrive _
  where DirectDrive_ = DirectDrive ()

pattern Linear_ ::
  ElectricType_
pattern Linear_ <- Linear _
  where Linear_ = Linear ()

pattern ElectricType_ ::
  Void
  -> ElectricType_
pattern ElectricType_ v <- ElectricType v
  where ElectricType_ v = ElectricType v
