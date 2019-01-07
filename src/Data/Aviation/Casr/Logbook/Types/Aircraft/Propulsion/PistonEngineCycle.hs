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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XFourStroke x
type family XTwoStroke x
type family XPistonEngineCycle x

data PistonEngineCycle x =
  FourStroke !(XFourStroke x)
  | TwoStroke !(XTwoStroke x)
  | PistonEngineCycle !(XPistonEngineCycle x)
  deriving Generic

deriving instance (Eq (XFourStroke x), Eq (XTwoStroke x), Eq (XPistonEngineCycle x)) =>
  Eq (PistonEngineCycle x)

deriving instance (Ord (XFourStroke x), Ord (XTwoStroke x), Ord (XPistonEngineCycle x)) =>
  Ord (PistonEngineCycle x)

deriving instance (Show (XFourStroke x), Show (XTwoStroke x), Show (XPistonEngineCycle x)) =>
  Show (PistonEngineCycle x)

class HasPistonEngineCycle a e | a -> e where
  pistonEngineCycle ::
    Lens' a (PistonEngineCycle e)
  xPistonEngineCycle ::
    Lens' a (XPistonEngineCycle e)
  xPistonEngineCycle =
    pistonEngineCycle . xPistonEngineCycle

instance HasPistonEngineCycle (PistonEngineCycle e) e where
  pistonEngineCycle =
    id

xPistonEngineCycle' ::
  (
    XFourStroke e ~ x
  , XTwoStroke e ~ x
  , XPistonEngineCycle e ~ Void
  ) =>
  Lens' (PistonEngineCycle e) x
xPistonEngineCycle' f (FourStroke x) =
  fmap FourStroke (f x)
xPistonEngineCycle' f (TwoStroke x) =
  fmap TwoStroke (f x)
xPistonEngineCycle' _ (PistonEngineCycle x) =
  absurd x

class AsPistonEngineCycle a e | a -> e where
  _PistonEngineCycle ::
    Prism' a (PistonEngineCycle e)
  _XFourStroke ::
    Prism' a (XFourStroke e)
  _XTwoStroke ::
    Prism' a (XTwoStroke e)
  _XPistonEngineCycle ::
    Prism' a (XPistonEngineCycle e)

instance AsPistonEngineCycle (PistonEngineCycle e) e where
  _PistonEngineCycle =
    id
  _XFourStroke =
    prism'
      FourStroke
      (
        \case
          FourStroke x ->
            Just x
          _ ->
            Nothing
      )
  _XTwoStroke =
    prism'
      TwoStroke
      (
        \case
          TwoStroke x ->
            Just x
          _ ->
            Nothing
      )
  _XPistonEngineCycle =
    prism'
      PistonEngineCycle
      (
        \case
          PistonEngineCycle x ->
            Just x
          _ ->
            Nothing
      )

type PistonEngineCycle_ =
  PistonEngineCycle ()

type instance XFourStroke () =
  ()
type instance XTwoStroke () =
  ()
type instance XPistonEngineCycle () =
  Void

pattern FourStroke_ ::
  PistonEngineCycle_
pattern FourStroke_ <- FourStroke _
  where FourStroke_ = FourStroke ()

pattern TwoStroke_ ::
  PistonEngineCycle_
pattern TwoStroke_ <- TwoStroke _
  where TwoStroke_ = TwoStroke ()

pattern PistonEngineCycle_ ::
  Void
  -> PistonEngineCycle_
pattern PistonEngineCycle_ v <- PistonEngineCycle v
  where PistonEngineCycle_ v = PistonEngineCycle v
