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

data PistonEngineConfiguration_ x =
  Parallel_ !(XParallel x)
  | VConfiguration_ !(XVConfiguration x)
  | WConfiguration_ !(XWConfiguration x)
  | XConfiguration_ !(XXConfiguration x)
  | Opposed_ !(XOpposed x)
  | Radial_ !(XRadial x)
  | PistonEngineConfiguration_ !(XPistonEngineConfiguration x)
  deriving Generic

deriving instance (Eq (XParallel x), Eq (XVConfiguration x), Eq (XWConfiguration x), Eq (XXConfiguration x), Eq (XOpposed x), Eq (XRadial x), Eq (XPistonEngineConfiguration x)) =>
  Eq (PistonEngineConfiguration_ x)
deriving instance (Ord (XParallel x), Ord (XVConfiguration x), Ord (XWConfiguration x), Ord (XXConfiguration x), Ord (XOpposed x), Ord (XRadial x), Ord (XPistonEngineConfiguration x)) =>
  Ord (PistonEngineConfiguration_ x)
deriving instance (Show (XParallel x), Show (XVConfiguration x), Show (XWConfiguration x), Show (XXConfiguration x), Show (XOpposed x), Show (XRadial x), Show (XPistonEngineConfiguration x)) =>
  Show (PistonEngineConfiguration_ x)

class HasPistonEngineConfiguration a e | a -> e where
  pistonEngineConfiguration ::
    Lens' a (PistonEngineConfiguration_ e)
  xPistonEngineConfiguration ::
    Lens' a (XPistonEngineConfiguration e)
  default xPistonEngineConfiguration ::
    Lens' a (XPistonEngineConfiguration e)
  xPistonEngineConfiguration =
    pistonEngineConfiguration . xPistonEngineConfiguration

instance HasPistonEngineConfiguration (PistonEngineConfiguration_ e) e where
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
  Lens' (PistonEngineConfiguration_ e) x
xPistonEngineConfiguration' f (Parallel_ x) =
  fmap Parallel_ (f x)
xPistonEngineConfiguration' f (VConfiguration_ x) =
  fmap VConfiguration_ (f x)
xPistonEngineConfiguration' f (WConfiguration_ x) =
  fmap WConfiguration_ (f x)
xPistonEngineConfiguration' f (XConfiguration_ x) =
  fmap XConfiguration_ (f x)
xPistonEngineConfiguration' f (Opposed_ x) =
  fmap Opposed_ (f x)
xPistonEngineConfiguration' f (Radial_ x) =
  fmap Radial_ (f x)
xPistonEngineConfiguration' _ (PistonEngineConfiguration_ x) =
  absurd x

class AsPistonEngineConfiguration a e | a -> e where
  _PistonEngineConfiguration ::
    Prism' a (PistonEngineConfiguration_ e)
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

instance AsPistonEngineConfiguration (PistonEngineConfiguration_ e) e where
  _PistonEngineConfiguration =
    id
  _Parallel =
    prism'
      Parallel_
      (
        \case
          Parallel_ x ->
            Just x
          _ ->
            Nothing
      )
  _VConfiguration =
    prism'
      VConfiguration_
      (
        \case
          VConfiguration_ x ->
            Just x
          _ ->
            Nothing
      )
  _WConfiguration =
    prism'
      WConfiguration_
      (
        \case
          WConfiguration_ x ->
            Just x
          _ ->
            Nothing
      )
  _XConfiguration =
    prism'
      XConfiguration_
      (
        \case
          XConfiguration_ x ->
            Just x
          _ ->
            Nothing
      )
  _Opposed =
    prism'
      Opposed_
      (
        \case
          Opposed_ x ->
            Just x
          _ ->
            Nothing
      )
  _Radial =
    prism'
      Radial_
      (
        \case
          Radial_ x ->
            Just x
          _ ->
            Nothing
      )
  _XPistonEngineConfiguration =
    prism'
      PistonEngineConfiguration_
      (
        \case
          PistonEngineConfiguration_ x ->
            Just x
          _ ->
            Nothing
      )

type PistonEngineConfiguration =
  PistonEngineConfiguration_ ()

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

pattern Parallel ::
  PistonEngineConfiguration
pattern Parallel <- Parallel_ _
  where Parallel = Parallel_ ()

pattern VConfiguration ::
  PistonEngineConfiguration
pattern VConfiguration <- VConfiguration_ _
  where VConfiguration = VConfiguration_ ()

pattern WConfiguration ::
  PistonEngineConfiguration
pattern WConfiguration <- WConfiguration_ _
  where WConfiguration = WConfiguration_ ()

pattern XConfiguration ::
  PistonEngineConfiguration
pattern XConfiguration <- XConfiguration_ _
  where XConfiguration = XConfiguration_ ()

pattern Opposed ::
  PistonEngineConfiguration
pattern Opposed <- Opposed_ _
  where Opposed = Opposed_ ()

pattern Radial ::
  PistonEngineConfiguration
pattern Radial <- Radial_ _
  where Radial = Radial_ ()

pattern PistonEngineConfiguration ::
  Void
  -> PistonEngineConfiguration
pattern PistonEngineConfiguration v <- PistonEngineConfiguration_ v
  where PistonEngineConfiguration v = PistonEngineConfiguration_ v
