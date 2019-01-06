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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PropulsionPosition where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XCentreline x
type family XLeftPropulsion x
type family XRightPropulsion x
type family XPropulsionPosition x

data PropulsionPosition_ x =
  Centreline_ !(XCentreline x)
  | LeftPropulsion_ !(XLeftPropulsion x)
  | RightPropulsion_ !(XRightPropulsion x)
  | PropulsionPosition_ !(XPropulsionPosition x)
  deriving Generic

deriving instance (Eq (XCentreline x), Eq (XLeftPropulsion x), Eq (XRightPropulsion x), Eq (XPropulsionPosition x)) =>
  Eq (PropulsionPosition_ x)

deriving instance (Ord (XCentreline x), Ord (XLeftPropulsion x), Ord (XRightPropulsion x), Ord (XPropulsionPosition x)) =>
  Ord (PropulsionPosition_ x)

deriving instance (Show (XCentreline x), Show (XLeftPropulsion x), Show (XRightPropulsion x), Show (XPropulsionPosition x)) =>
  Show (PropulsionPosition_ x)

class HasPropulsionPosition a e | a -> e where
  propulsionPosition ::
    Lens' a (PropulsionPosition_ e)
  xPropulsionPosition ::
    Lens' a (XPropulsionPosition e)
  default xPropulsionPosition ::
    Lens' a (XPropulsionPosition e)
  xPropulsionPosition =
    propulsionPosition . xPropulsionPosition

instance HasPropulsionPosition (PropulsionPosition_ e) e where
  propulsionPosition =
    id

xPropulsionPosition' ::
  (
    XCentreline e ~ x
  , XLeftPropulsion e ~ x
  , XRightPropulsion e ~ x
  , XPropulsionPosition e ~ Void
  ) =>
  Lens' (PropulsionPosition_ e) x
xPropulsionPosition' f (Centreline_ x) =
  fmap Centreline_ (f x)
xPropulsionPosition' f (LeftPropulsion_ x) =
  fmap LeftPropulsion_ (f x)
xPropulsionPosition' f (RightPropulsion_ x) =
  fmap RightPropulsion_ (f x)
xPropulsionPosition' _ (PropulsionPosition_ x) =
  absurd x

class AsPropulsionPosition a e | a -> e where
  _PropulsionPosition ::
    Prism' a (PropulsionPosition_ e)
  _XCentreline ::
    Prism' a (XCentreline e)
  _XLeftPropulsion ::
    Prism' a (XLeftPropulsion e)
  _XRightPropulsion ::
    Prism' a (XRightPropulsion e)
  _XPropulsionPosition ::
    Prism' a (XPropulsionPosition e)

instance AsPropulsionPosition (PropulsionPosition_ e) e where
  _PropulsionPosition =
    id
  _XCentreline =
    prism'
      Centreline_
      (
        \case
          Centreline_ x ->
            Just x
          _ ->
            Nothing
      )
  _XLeftPropulsion =
    prism'
      LeftPropulsion_
      (
        \case
          LeftPropulsion_ x ->
            Just x
          _ ->
            Nothing
      )
  _XRightPropulsion =
    prism'
      RightPropulsion_
      (
        \case
          RightPropulsion_ x ->
            Just x
          _ ->
            Nothing
      )
  _XPropulsionPosition =
    prism'
      PropulsionPosition_
      (
        \case
          PropulsionPosition_ x ->
            Just x
          _ ->
            Nothing
      )

type PropulsionPosition =
  PropulsionPosition_ ()

type instance XCentreline () =
  ()
type instance XLeftPropulsion () =
  ()
type instance XRightPropulsion () =
  ()
type instance XPropulsionPosition () =
  Void

pattern Centreline ::
  PropulsionPosition
pattern Centreline <- Centreline_ _
  where Centreline = Centreline_ ()

pattern LeftPropulsion ::
  PropulsionPosition
pattern LeftPropulsion <- LeftPropulsion_ _
  where LeftPropulsion = LeftPropulsion_ ()

pattern RightPropulsion ::
  PropulsionPosition
pattern RightPropulsion <- RightPropulsion_ _
  where RightPropulsion = RightPropulsion_ ()

pattern PropulsionPosition ::
  Void
  -> PropulsionPosition
pattern PropulsionPosition v <- PropulsionPosition_ v
  where PropulsionPosition v = PropulsionPosition_ v
