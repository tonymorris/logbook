{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.JetType where

import Control.Lens(Lens', Prism', prism')
import GHC.Generics(Generic)
import Prelude

data JetType =
  Turbojet
  | Turbofan
  | Turboprop
  | Ramjet
  | Scramjet
  deriving (Eq, Ord, Show, Generic)

class HasJetType a where
  jetType ::
    Lens' a JetType

instance HasJetType JetType where
  jetType =
    id

class AsJetType a where
  _JetType ::
    Prism' a JetType
  _Turbojet ::
    Prism' a ()
  _Turbofan ::
    Prism' a ()
  _Turboprop ::
    Prism' a ()
  _Ramjet ::
    Prism' a ()
  _Scramjet ::
    Prism' a ()

instance AsJetType JetType where
  _JetType =
    id
  _Turbojet =
    prism'
      (\() -> Turbojet)
      (
        \case
          Turbojet ->
            Just ()
          _ ->
            Nothing
      )
  _Turbofan =
    prism'
      (\() -> Turbofan)
      (
        \case
          Turbofan ->
            Just ()
          _ ->
            Nothing
      )
  _Turboprop =
    prism'
      (\() -> Turboprop)
      (
        \case
          Turboprop ->
            Just ()
          _ ->
            Nothing
      )
  _Ramjet =
    prism'
      (\() -> Ramjet)
      (
        \case
          Ramjet ->
            Just ()
          _ ->
            Nothing
      )
  _Scramjet =
    prism'
      (\() -> Scramjet)
      (
        \case
          Scramjet ->
            Just ()
          _ ->
            Nothing
      )
