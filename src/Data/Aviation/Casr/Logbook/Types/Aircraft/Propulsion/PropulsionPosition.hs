{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PropulsionPosition where

import Control.Lens(Lens', Prism', prism')
import GHC.Generics(Generic)
import Prelude

data PropulsionPosition =
  Centreline
  | LeftPropulsion
  | RightPropulsion
  deriving (Eq, Ord, Show, Generic)

class HasPropulsionPosition a where
  propulsionPosition ::
    Lens' a PropulsionPosition

instance HasPropulsionPosition PropulsionPosition where
  propulsionPosition =
    id

class AsPropulsionPosition a where
  _PropulsionPosition ::
    Prism' a PropulsionPosition
  _Centreline ::
    Prism' a ()
  _LeftPropulsion ::
    Prism' a ()
  _RightPropulsion ::
    Prism' a ()

instance AsPropulsionPosition PropulsionPosition where
  _PropulsionPosition =
    id
  _Centreline =
    prism'
      (\() -> Centreline)
      (
        \case
          Centreline ->
            Just ()
          _ ->
            Nothing
      )
  _LeftPropulsion =
    prism'
      (\() -> LeftPropulsion)
      (
        \case
          LeftPropulsion ->
            Just ()
          _ ->
            Nothing
      )
  _RightPropulsion =
    prism'
      (\() -> RightPropulsion)
      (
        \case
          RightPropulsion ->
            Just ()
          _ ->
            Nothing
      )
