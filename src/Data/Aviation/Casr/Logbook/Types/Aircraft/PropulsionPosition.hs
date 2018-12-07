{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition where

import Control.Lens
import GHC.Generics
import Prelude

data PropulsionPosition =
  Centreline
  | LeftPropulsion
  | RightPropulsion
  deriving (Eq, Ord, Show, Generic)

class AsPropulsionPosition a where
  _PropulsionPosition ::
    Prism' a PropulsionPosition
  _Centreline ::
    Prism' a ()
  _LeftPropulsion ::
    Prism' a ()
  _RightPropulsion ::
    Prism' a ()
  _Centreline =
    _PropulsionPosition . _Centreline
  _LeftPropulsion =
    _PropulsionPosition . _LeftPropulsion
  _RightPropulsion = 
    _PropulsionPosition . _RightPropulsion

instance AsPropulsionPosition PropulsionPosition where
  _PropulsionPosition =
    id
  _Centreline =
    prism'
      (\() -> Centreline)
      (\case
        Centreline ->
          Just ()
        _ ->
          Nothing)
  _LeftPropulsion =
    prism'
      (\() -> LeftPropulsion)
      (\case
        LeftPropulsion ->
          Just ()
        _ ->
          Nothing)
  _RightPropulsion =
    prism'
      (\() -> RightPropulsion)
      (\case
        RightPropulsion ->
          Just ()
        _ ->
          Nothing)
