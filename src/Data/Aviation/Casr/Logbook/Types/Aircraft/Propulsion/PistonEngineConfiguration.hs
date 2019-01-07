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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineConfiguration where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XParallel x
type family XVConfiguration x
type family XWConfiguration x
type family XXConfiguration x
type family XOpposed x
type family XRadial x
type family XPistonEngineConfiguration x

data PistonEngineConfiguration x =
  Parallel !(XParallel x)
  | VConfiguration !(XVConfiguration x)
  | WConfiguration !(XWConfiguration x)
  | XConfiguration !(XXConfiguration x)
  | Opposed !(XOpposed x)
  | Radial !(XRadial x)
  | PistonEngineConfiguration !(XPistonEngineConfiguration x)
  deriving Generic

deriving instance (Eq (XParallel x), Eq (XVConfiguration x), Eq (XWConfiguration x), Eq (XXConfiguration x), Eq (XOpposed x), Eq (XRadial x), Eq (XPistonEngineConfiguration x)) =>
  Eq (PistonEngineConfiguration x)
deriving instance (Ord (XParallel x), Ord (XVConfiguration x), Ord (XWConfiguration x), Ord (XXConfiguration x), Ord (XOpposed x), Ord (XRadial x), Ord (XPistonEngineConfiguration x)) =>
  Ord (PistonEngineConfiguration x)
deriving instance (Show (XParallel x), Show (XVConfiguration x), Show (XWConfiguration x), Show (XXConfiguration x), Show (XOpposed x), Show (XRadial x), Show (XPistonEngineConfiguration x)) =>
  Show (PistonEngineConfiguration x)

class HasPistonEngineConfiguration a e | a -> e where
  pistonEngineConfiguration ::
    Lens' a (PistonEngineConfiguration e)
  xPistonEngineConfiguration ::
    Lens' a (XPistonEngineConfiguration e)
  xPistonEngineConfiguration =
    pistonEngineConfiguration . xPistonEngineConfiguration

instance HasPistonEngineConfiguration (PistonEngineConfiguration e) e where
  pistonEngineConfiguration =
    id

xPistonEngineConfiguration' ::
  (
    XParallel e ~ x
  , XVConfiguration e ~ x
  , XWConfiguration e ~ x
  , XXConfiguration e ~ x
  , XOpposed e ~ x
  , XRadial e ~ x
  , XPistonEngineConfiguration e ~ Void
  ) =>
  Lens' (PistonEngineConfiguration e) x
xPistonEngineConfiguration' f (Parallel x) =
  fmap Parallel (f x)
xPistonEngineConfiguration' f (VConfiguration x) =
  fmap VConfiguration (f x)
xPistonEngineConfiguration' f (WConfiguration x) =
  fmap WConfiguration (f x)
xPistonEngineConfiguration' f (XConfiguration x) =
  fmap XConfiguration (f x)
xPistonEngineConfiguration' f (Opposed x) =
  fmap Opposed (f x)
xPistonEngineConfiguration' f (Radial x) =
  fmap Radial (f x)
xPistonEngineConfiguration' _ (PistonEngineConfiguration x) =
  absurd x

class AsPistonEngineConfiguration a e | a -> e where
  _PistonEngineConfiguration ::
    Prism' a (PistonEngineConfiguration e)
  _Parallel ::
    Prism' a (XParallel e)
  _VConfiguration ::
    Prism' a (XVConfiguration e)
  _WConfiguration ::
    Prism' a (XWConfiguration e)
  _XConfiguration ::
    Prism' a (XXConfiguration e)
  _Opposed ::
    Prism' a (XOpposed e)
  _Radial ::
    Prism' a (XRadial e)
  _XPistonEngineConfiguration ::
    Prism' a (XPistonEngineConfiguration e)

instance AsPistonEngineConfiguration (PistonEngineConfiguration e) e where
  _PistonEngineConfiguration =
    id
  _Parallel =
    prism'
      Parallel
      (
        \case
          Parallel x ->
            Just x
          _ ->
            Nothing
      )
  _VConfiguration =
    prism'
      VConfiguration
      (
        \case
          VConfiguration x ->
            Just x
          _ ->
            Nothing
      )
  _WConfiguration =
    prism'
      WConfiguration
      (
        \case
          WConfiguration x ->
            Just x
          _ ->
            Nothing
      )
  _XConfiguration =
    prism'
      XConfiguration
      (
        \case
          XConfiguration x ->
            Just x
          _ ->
            Nothing
      )
  _Opposed =
    prism'
      Opposed
      (
        \case
          Opposed x ->
            Just x
          _ ->
            Nothing
      )
  _Radial =
    prism'
      Radial
      (
        \case
          Radial x ->
            Just x
          _ ->
            Nothing
      )
  _XPistonEngineConfiguration =
    prism'
      PistonEngineConfiguration
      (
        \case
          PistonEngineConfiguration x ->
            Just x
          _ ->
            Nothing
      )

type PistonEngineConfiguration_ =
  PistonEngineConfiguration ()

type instance XParallel () =
  ()
type instance XVConfiguration () =
  ()
type instance XWConfiguration () =
  ()
type instance XXConfiguration () =
  ()
type instance XOpposed () =
  ()
type instance XRadial () =
  ()
type instance XPistonEngineConfiguration () =
  Void

pattern Parallel_ ::
  PistonEngineConfiguration_
pattern Parallel_ <- Parallel _
  where Parallel_ = Parallel ()

pattern VConfiguration_ ::
  PistonEngineConfiguration_
pattern VConfiguration_ <- VConfiguration _
  where VConfiguration_ = VConfiguration ()

pattern WConfiguration_ ::
  PistonEngineConfiguration_
pattern WConfiguration_ <- WConfiguration _
  where WConfiguration_ = WConfiguration ()

pattern XConfiguration_ ::
  PistonEngineConfiguration_
pattern XConfiguration_ <- XConfiguration _
  where XConfiguration_ = XConfiguration ()

pattern Opposed_ ::
  PistonEngineConfiguration_
pattern Opposed_ <- Opposed _
  where Opposed_ = Opposed ()

pattern Radial_ ::
  PistonEngineConfiguration_
pattern Radial_ <- Radial _
  where Radial_ = Radial ()

pattern PistonEngineConfiguration_ ::
  Void
  -> PistonEngineConfiguration_
pattern PistonEngineConfiguration_ v <- PistonEngineConfiguration v
  where PistonEngineConfiguration_ v = PistonEngineConfiguration v
