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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PropulsionPosition where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XCentreline x
type family XLeftPropulsion x
type family XRightPropulsion x
type family XPropulsionPosition x

data PropulsionPosition x =
  Centreline !(XCentreline x)
  | LeftPropulsion !(XLeftPropulsion x)
  | RightPropulsion !(XRightPropulsion x)
  | PropulsionPosition !(XPropulsionPosition x)
  deriving Generic

deriving instance (Eq (XCentreline x), Eq (XLeftPropulsion x), Eq (XRightPropulsion x), Eq (XPropulsionPosition x)) =>
  Eq (PropulsionPosition x)

deriving instance (Ord (XCentreline x), Ord (XLeftPropulsion x), Ord (XRightPropulsion x), Ord (XPropulsionPosition x)) =>
  Ord (PropulsionPosition x)

deriving instance (Show (XCentreline x), Show (XLeftPropulsion x), Show (XRightPropulsion x), Show (XPropulsionPosition x)) =>
  Show (PropulsionPosition x)

class HasPropulsionPosition a e | a -> e where
  propulsionPosition ::
    Lens' a (PropulsionPosition e)
  xPropulsionPosition ::
    Lens' a (XPropulsionPosition e)
  xPropulsionPosition =
    propulsionPosition . xPropulsionPosition

instance HasPropulsionPosition (PropulsionPosition e) e where
  propulsionPosition =
    id

xPropulsionPosition' ::
  (
    XCentreline e ~ x
  , XLeftPropulsion e ~ x
  , XRightPropulsion e ~ x
  , XPropulsionPosition e ~ Void
  ) =>
  Lens' (PropulsionPosition e) x
xPropulsionPosition' f (Centreline x) =
  fmap Centreline (f x)
xPropulsionPosition' f (LeftPropulsion x) =
  fmap LeftPropulsion (f x)
xPropulsionPosition' f (RightPropulsion x) =
  fmap RightPropulsion (f x)
xPropulsionPosition' _ (PropulsionPosition x) =
  absurd x

class AsPropulsionPosition a e | a -> e where
  _PropulsionPosition ::
    Prism' a (PropulsionPosition e)
  _XCentreline ::
    Prism' a (XCentreline e)
  _XLeftPropulsion ::
    Prism' a (XLeftPropulsion e)
  _XRightPropulsion ::
    Prism' a (XRightPropulsion e)
  _XPropulsionPosition ::
    Prism' a (XPropulsionPosition e)

instance AsPropulsionPosition (PropulsionPosition e) e where
  _PropulsionPosition =
    id
  _XCentreline =
    prism'
      Centreline
      (
        \case
          Centreline x ->
            Just x
          _ ->
            Nothing
      )
  _XLeftPropulsion =
    prism'
      LeftPropulsion
      (
        \case
          LeftPropulsion x ->
            Just x
          _ ->
            Nothing
      )
  _XRightPropulsion =
    prism'
      RightPropulsion
      (
        \case
          RightPropulsion x ->
            Just x
          _ ->
            Nothing
      )
  _XPropulsionPosition =
    prism'
      PropulsionPosition
      (
        \case
          PropulsionPosition x ->
            Just x
          _ ->
            Nothing
      )

type PropulsionPosition_ =
  PropulsionPosition ()

type instance XCentreline () =
  ()
type instance XLeftPropulsion () =
  ()
type instance XRightPropulsion () =
  ()
type instance XPropulsionPosition () =
  Void

pattern Centreline_ ::
  PropulsionPosition_
pattern Centreline_ <- Centreline _
  where Centreline_ = Centreline ()

pattern LeftPropulsion_ ::
  PropulsionPosition_
pattern LeftPropulsion_ <- LeftPropulsion _
  where LeftPropulsion_ = LeftPropulsion ()

pattern RightPropulsion_ ::
  PropulsionPosition_
pattern RightPropulsion_ <- RightPropulsion _
  where RightPropulsion_ = RightPropulsion ()

pattern PropulsionPosition_ ::
  Void
  -> PropulsionPosition_
pattern PropulsionPosition_ v <- PropulsionPosition v
  where PropulsionPosition_ v = PropulsionPosition v
