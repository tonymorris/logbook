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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XFourStroke x
type family XTwoStroke x
type family XPistonEngineCycle x

data PistonEngineCycle_ x =
  FourStroke_ !(XFourStroke x)
  | TwoStroke_ !(XTwoStroke x)
  | PistonEngineCycle_ !(XPistonEngineCycle x)
  deriving Generic

deriving instance (Eq (XFourStroke x), Eq (XTwoStroke x), Eq (XPistonEngineCycle x)) =>
  Eq (PistonEngineCycle_ x)

deriving instance (Ord (XFourStroke x), Ord (XTwoStroke x), Ord (XPistonEngineCycle x)) =>
  Ord (PistonEngineCycle_ x)

deriving instance (Show (XFourStroke x), Show (XTwoStroke x), Show (XPistonEngineCycle x)) =>
  Show (PistonEngineCycle_ x)

class HasPistonEngineCycle a e | a -> e where
  pistonEngineCycle ::
    Lens' a (PistonEngineCycle_ e)
  xPistonEngineCycle ::
    Lens' a (XPistonEngineCycle e)
  default xPistonEngineCycle ::
    Lens' a (XPistonEngineCycle e)
  xPistonEngineCycle =
    pistonEngineCycle . xPistonEngineCycle

instance HasPistonEngineCycle (PistonEngineCycle_ e) e where
  pistonEngineCycle =
    id

xPistonEngineCycle' ::
  (
    XFourStroke e ~ x
  , XTwoStroke e ~ x
  , XPistonEngineCycle e ~ Void
  ) =>
  Lens' (PistonEngineCycle_ e) x
xPistonEngineCycle' f (FourStroke_ x) =
  fmap FourStroke_ (f x)
xPistonEngineCycle' f (TwoStroke_ x) =
  fmap TwoStroke_ (f x)
xPistonEngineCycle' _ (PistonEngineCycle_ x) =
  absurd x

class AsPistonEngineCycle a e | a -> e where
  _PistonEngineCycle ::
    Prism' a (PistonEngineCycle_ e)
  _XFourStroke ::
    Prism' a (XFourStroke e)
  _XTwoStroke ::
    Prism' a (XTwoStroke e)
  _XPistonEngineCycle ::
    Prism' a (XPistonEngineCycle e)

instance AsPistonEngineCycle (PistonEngineCycle_ e) e where
  _PistonEngineCycle =
    id
  _XFourStroke =
    prism'
      FourStroke_
      (
        \case
          FourStroke_ x ->
            Just x
          _ ->
            Nothing
      )
  _XTwoStroke =
    prism'
      TwoStroke_
      (
        \case
          TwoStroke_ x ->
            Just x
          _ ->
            Nothing
      )
  _XPistonEngineCycle =
    prism'
      PistonEngineCycle_
      (
        \case
          PistonEngineCycle_ x ->
            Just x
          _ ->
            Nothing
      )

type PistonEngineCycle =
  PistonEngineCycle_ ()

type instance XFourStroke () =
  ()
type instance XTwoStroke () =
  ()
type instance XPistonEngineCycle () =
  Void

pattern FourStroke ::
  PistonEngineCycle
pattern FourStroke <- FourStroke_ _
  where FourStroke = FourStroke_ ()

pattern TwoStroke ::
  PistonEngineCycle
pattern TwoStroke <- TwoStroke_ _
  where TwoStroke = TwoStroke_ ()

pattern PistonEngineCycle ::
  Void
  -> PistonEngineCycle
pattern PistonEngineCycle v <- PistonEngineCycle_ v
  where PistonEngineCycle v = PistonEngineCycle_ v
