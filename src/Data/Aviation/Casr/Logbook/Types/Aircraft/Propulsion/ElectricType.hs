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
{-# LANGUAGE DefaultSignatures #-}

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

data ElectricType_ x =
  BrushlessAC_ !(XBrushlessAC x)
  | BrushedAC_ !(XBrushedAC x)
  | BrushlessDC_ !(XBrushlessDC x)
  | BrushedDC_ !(XBrushedDC x)
  | UniversalACDC_ !(XUniversalACDC x)
  | SwitchedReluctance_ !(XSwitchedReluctance x)
  | DirectDrive_ !(XDirectDrive x)
  | Linear_ !(XLinear x)
  | ElectricType_ !(XElectricType x)
  deriving Generic

deriving instance (Eq (XBrushlessAC x), Eq (XBrushedAC x), Eq (XBrushlessDC x), Eq (XBrushedDC x), Eq (XUniversalACDC x), Eq (XSwitchedReluctance x), Eq (XDirectDrive x), Eq (XLinear x), Eq (XElectricType x)) =>
  Eq (ElectricType_ x)
deriving instance (Ord (XBrushlessAC x), Ord (XBrushedAC x), Ord (XBrushlessDC x), Ord (XBrushedDC x), Ord (XUniversalACDC x), Ord (XSwitchedReluctance x), Ord (XDirectDrive x), Ord (XLinear x), Ord (XElectricType x)) =>
  Ord (ElectricType_ x)
deriving instance (Show (XBrushlessAC x), Show (XBrushedAC x), Show (XBrushlessDC x), Show (XBrushedDC x), Show (XUniversalACDC x), Show (XSwitchedReluctance x), Show (XDirectDrive x), Show (XLinear x), Show (XElectricType x)) =>
  Show (ElectricType_ x)

class HasElectricType a e | a -> e where
  electricType ::
    Lens' a (ElectricType_ e)
  xElectricType ::
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
    Lens' a x
  default xElectricType ::
    (
      XBrushlessAC () ~ x
    , XBrushedAC () ~ x
    , XBrushlessDC () ~ x
    , XBrushedDC () ~ x
    , XUniversalACDC () ~ x
    , XSwitchedReluctance () ~ x
    , XDirectDrive () ~ x
    , XLinear () ~ x
    , XElectricType e ~ Void
    ) =>
    Lens' a x
  xElectricType f a =
    fmap (\() -> a) (f ())

instance HasElectricType (ElectricType_ e) e where
  electricType =
    id
  xElectricType f (BrushlessAC_ x) =
    fmap BrushlessAC_ (f x)
  xElectricType f (BrushedAC_ x) =
    fmap BrushedAC_ (f x)
  xElectricType f (BrushlessDC_ x) =
    fmap BrushlessDC_ (f x)
  xElectricType f (BrushedDC_ x) =
    fmap BrushedDC_ (f x)
  xElectricType f (UniversalACDC_ x) =
    fmap UniversalACDC_ (f x)
  xElectricType f (SwitchedReluctance_ x) =
    fmap SwitchedReluctance_ (f x)
  xElectricType f (DirectDrive_ x) =
    fmap DirectDrive_ (f x)
  xElectricType f (Linear_ x) =
    fmap Linear_ (f x)
  xElectricType _ (ElectricType_ x) =
    absurd x

class AsElectricType a e | a -> e where
  _ElectricType ::
    Prism' a (ElectricType_ e)
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

instance AsElectricType (ElectricType_ e) e where
  _ElectricType =
    id
  _BrushlessAC =
    prism'
      BrushlessAC_
      (
        \case
          BrushlessAC_ x ->
            Just x
          _ ->
            Nothing
      )
  _BrushedAC =
    prism'
      BrushedAC_
      (
        \case
          BrushedAC_ x ->
            Just x
          _ ->
            Nothing
      )
  _BrushlessDC =
    prism'
      BrushlessDC_
      (
        \case
          BrushlessDC_ x ->
            Just x
          _ ->
            Nothing
      )
  _BrushedDC =
    prism'
      BrushedDC_
      (
        \case
          BrushedDC_ x ->
            Just x
          _ ->
            Nothing
      )
  _UniversalACDC =
    prism'
      UniversalACDC_
      (
        \case
          UniversalACDC_ x ->
            Just x
          _ ->
            Nothing
      )
  _SwitchedReluctance =
    prism'
      SwitchedReluctance_
      (
        \case
          SwitchedReluctance_ x ->
            Just x
          _ ->
            Nothing
      )
  _DirectDrive =
    prism'
      DirectDrive_
      (
        \case
          DirectDrive_ x ->
            Just x
          _ ->
            Nothing
      )
  _Linear =
    prism'
      Linear_
      (
        \case
          Linear_ x ->
            Just x
          _ ->
            Nothing
      )
  _XElectricType =
    prism'
      ElectricType_
      (
        \case
          ElectricType_ x ->
            Just x
          _ ->
            Nothing
      )

type ElectricType =
  ElectricType_ ()

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

pattern BrushlessAC ::
  ElectricType
pattern BrushlessAC <- BrushlessAC_ _
  where BrushlessAC = BrushlessAC_ ()

pattern BrushedAC ::
  ElectricType
pattern BrushedAC <- BrushedAC_ _
  where BrushedAC = BrushedAC_ ()

pattern BrushlessDC ::
  ElectricType
pattern BrushlessDC <- BrushlessDC_ _
  where BrushlessDC = BrushlessDC_ ()

pattern BrushedDC ::
  ElectricType
pattern BrushedDC <- BrushedDC_ _
  where BrushedDC = BrushedDC_ ()

pattern UniversalACDC ::
  ElectricType
pattern UniversalACDC <- UniversalACDC_ _
  where UniversalACDC = UniversalACDC_ ()

pattern SwitchedReluctance ::
  ElectricType
pattern SwitchedReluctance <- SwitchedReluctance_ _
  where SwitchedReluctance = SwitchedReluctance_ ()

pattern DirectDrive ::
  ElectricType
pattern DirectDrive <- DirectDrive_ _
  where DirectDrive = DirectDrive_ ()

pattern Linear ::
  ElectricType
pattern Linear <- Linear_ _
  where Linear = Linear_ ()

pattern ElectricType ::
  Void
  -> ElectricType
pattern ElectricType v <- ElectricType_ v
  where ElectricType v = ElectricType_ v
