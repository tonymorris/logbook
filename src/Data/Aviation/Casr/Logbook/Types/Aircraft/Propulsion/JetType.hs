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

data JetType_ x =
  Turbojet_ !(XTurbojet x)
  | Turbofan_ !(XTurbofan x)
  | Turboprop_ !(XTurboprop x)
  | Ramjet_ !(XRamjet x)
  | Scramjet_ !(XScramjet x)
  | JetType_ !(XJetType x)
  deriving Generic

deriving instance (Eq (XTurbojet x), Eq (XTurbofan x), Eq (XTurboprop x), Eq (XRamjet x), Eq (XScramjet x), Eq (XJetType x)) =>
  Eq (JetType_ x)

deriving instance (Ord (XTurbojet x), Ord (XTurbofan x), Ord (XTurboprop x), Ord (XRamjet x), Ord (XScramjet x), Ord (XJetType x)) =>
  Ord (JetType_ x)

deriving instance (Show (XTurbojet x), Show (XTurbofan x), Show (XTurboprop x), Show (XRamjet x), Show (XScramjet x), Show (XJetType x)) =>
  Show (JetType_ x)

class HasJetType a e | a -> e where
  jetType ::
    Lens' a (JetType_ e)
  xJetType ::
    Lens' a (XJetType e)
  default xJetType ::
    Lens' a (XJetType e)
  xJetType =
    jetType . xJetType

instance HasJetType (JetType_ e) e where
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
  Lens' (JetType_ e) x
xJetType' f (Turbojet_ x) =
  fmap Turbojet_ (f x)
xJetType' f (Turbofan_ x) =
  fmap Turbofan_ (f x)
xJetType' f (Turboprop_ x) =
  fmap Turboprop_ (f x)
xJetType' f (Ramjet_ x) =
  fmap Ramjet_ (f x)
xJetType' f (Scramjet_ x) =
  fmap Scramjet_ (f x)
xJetType' _ (JetType_ x) =
  absurd x

class AsJetType a e | a -> e where
  _JetType ::
    Prism' a (JetType_ e)
  _XTurbojet ::
    Prism' a (XTurbojet e)
  _XTurbofan ::
    Prism' a (XTurbofan e)
  _XTurboprop ::
    Prism' a (XTurboprop e)
  _XRamjet ::
    Prism' a (XRamjet e)
  _XScramjet ::
    Prism' a (XScramjet e)
  _XJetType ::
    Prism' a (XJetType e)

instance AsJetType (JetType_ e) e where
  _JetType =
    id
  _XTurbojet =
    prism'
      Turbojet_
      (
        \case
          Turbojet_ x ->
            Just x
          _ ->
            Nothing
      )
  _XTurbofan =
    prism'
      Turbofan_
      (
        \case
          Turbofan_ x ->
            Just x
          _ ->
            Nothing
      )
  _XTurboprop =
    prism'
      Turboprop_
      (
        \case
          Turboprop_ x ->
            Just x
          _ ->
            Nothing
      )
  _XRamjet =
    prism'
      Ramjet_
      (
        \case
          Ramjet_ x ->
            Just x
          _ ->
            Nothing
      )
  _XScramjet =
    prism'
      Scramjet_
      (
        \case
          Scramjet_ x ->
            Just x
          _ ->
            Nothing
      )
  _XJetType =
    prism'
      JetType_
      (
        \case
          JetType_ x ->
            Just x
          _ ->
            Nothing
      )

type JetType =
  JetType_ ()

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

pattern Turbojet ::
  JetType
pattern Turbojet <- Turbojet_ _
  where Turbojet = Turbojet_ ()

pattern Turbofan ::
  JetType
pattern Turbofan <- Turbofan_ _
  where Turbofan = Turbofan_ ()

pattern Turboprop ::
  JetType
pattern Turboprop <- Turboprop_ _
  where Turboprop = Turboprop_ ()

pattern Ramjet ::
  JetType
pattern Ramjet <- Ramjet_ _
  where Ramjet = Ramjet_ ()

pattern Scramjet ::
  JetType
pattern Scramjet <- Scramjet_ _
  where Scramjet = Scramjet_ ()

pattern JetType ::
  Void
  -> JetType
pattern JetType v <- JetType_ v
  where JetType v = JetType_ v
