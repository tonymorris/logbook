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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.JetType where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XTurbojet x
type family XTurbofan x
type family XTurboprop x
type family XRamjet x
type family XScramjet x
type family XJetType x

data JetType x =
  Turbojet !(XTurbojet x)
  | Turbofan !(XTurbofan x)
  | Turboprop !(XTurboprop x)
  | Ramjet !(XRamjet x)
  | Scramjet !(XScramjet x)
  | JetType !(XJetType x)
  deriving Generic

deriving instance (Eq (XTurbojet x), Eq (XTurbofan x), Eq (XTurboprop x), Eq (XRamjet x), Eq (XScramjet x), Eq (XJetType x)) =>
  Eq (JetType x)

deriving instance (Ord (XTurbojet x), Ord (XTurbofan x), Ord (XTurboprop x), Ord (XRamjet x), Ord (XScramjet x), Ord (XJetType x)) =>
  Ord (JetType x)

deriving instance (Show (XTurbojet x), Show (XTurbofan x), Show (XTurboprop x), Show (XRamjet x), Show (XScramjet x), Show (XJetType x)) =>
  Show (JetType x)

class HasJetType a e | a -> e where
  jetType ::
    Lens' a (JetType e)
  xJetType ::
    Lens' a (XJetType e)
  xJetType =
    jetType . xJetType

instance HasJetType (JetType e) e where
  jetType =
    id

xJetType' ::
  (
    XTurbojet e ~ x
  , XTurbofan e ~ x
  , XTurboprop e ~ x
  , XRamjet e ~ x
  , XScramjet e ~ x
  , XJetType e ~ Void
  ) =>
  Lens' (JetType e) x
xJetType' f (Turbojet x) =
  fmap Turbojet (f x)
xJetType' f (Turbofan x) =
  fmap Turbofan (f x)
xJetType' f (Turboprop x) =
  fmap Turboprop (f x)
xJetType' f (Ramjet x) =
  fmap Ramjet (f x)
xJetType' f (Scramjet x) =
  fmap Scramjet (f x)
xJetType' _ (JetType x) =
  absurd x

class AsJetType a e | a -> e where
  _JetType ::
    Prism' a (JetType e)
  _Turbojet ::
    Prism' a (XTurbojet e)
  _Turbofan ::
    Prism' a (XTurbofan e)
  _Turboprop ::
    Prism' a (XTurboprop e)
  _Ramjet ::
    Prism' a (XRamjet e)
  _Scramjet ::
    Prism' a (XScramjet e)
  _XJetType ::
    Prism' a (XJetType e)

instance AsJetType (JetType e) e where
  _JetType =
    id
  _Turbojet =
    prism'
      Turbojet
      (
        \case
          Turbojet x ->
            Just x
          _ ->
            Nothing
      )
  _Turbofan =
    prism'
      Turbofan
      (
        \case
          Turbofan x ->
            Just x
          _ ->
            Nothing
      )
  _Turboprop =
    prism'
      Turboprop
      (
        \case
          Turboprop x ->
            Just x
          _ ->
            Nothing
      )
  _Ramjet =
    prism'
      Ramjet
      (
        \case
          Ramjet x ->
            Just x
          _ ->
            Nothing
      )
  _Scramjet =
    prism'
      Scramjet
      (
        \case
          Scramjet x ->
            Just x
          _ ->
            Nothing
      )
  _XJetType =
    prism'
      JetType
      (
        \case
          JetType x ->
            Just x
          _ ->
            Nothing
      )

type JetType_ =
  JetType ()

type instance XTurbojet () =
  ()
type instance XTurbofan () =
  ()
type instance XTurboprop () =
  ()
type instance XRamjet () =
  ()
type instance XScramjet () =
  ()
type instance XJetType () =
  Void

pattern Turbojet_ ::
  JetType_
pattern Turbojet_ <- Turbojet _
  where Turbojet_ = Turbojet ()

pattern Turbofan_ ::
  JetType_
pattern Turbofan_ <- Turbofan _
  where Turbofan_ = Turbofan ()

pattern Turboprop_ ::
  JetType_
pattern Turboprop_ <- Turboprop _
  where Turboprop_ = Turboprop ()

pattern Ramjet_ ::
  JetType_
pattern Ramjet_ <- Ramjet _
  where Ramjet_ = Ramjet ()

pattern Scramjet_ ::
  JetType_
pattern Scramjet_ <- Scramjet _
  where Scramjet_ = Scramjet ()

pattern JetType_ ::
  Void
  -> JetType_
pattern JetType_ v <- JetType v
  where JetType_ v = JetType v
