{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.JetType where

import Control.Lens
import GHC.Generics
import Prelude

data JetType =
  Turbojet
  | Turbofan
  | Turboprop
  | Ramjet
  | Scramjet
  deriving (Eq, Ord, Show, Generic)

class ManyJetType a where
  _JetType_ :: 
    Traversal' a JetType

instance ManyJetType JetType where
  _JetType_ =
    id
    
class ManyJetType a => HasJetType a where
  jetType :: 
    Lens' a JetType

instance HasJetType JetType where
  jetType =
    id

class ManyJetType a => AsJetType a where
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
      (\case
        Turbojet ->
          Just ()
        _ ->
          Nothing)
  _Turbofan =
    prism'
      (\() -> Turbofan)
      (\case
        Turbofan ->
          Just ()
        _ ->
          Nothing)
  _Turboprop =
    prism'
      (\() -> Turboprop)
      (\case
        Turboprop ->
          Just ()
        _ ->
          Nothing)
  _Ramjet =
    prism'
      (\() -> Ramjet)
      (\case
        Ramjet ->
          Just ()
        _ ->
          Nothing)
  _Scramjet =
    prism'
      (\() -> Scramjet)
      (\case
        Scramjet ->
          Just ()
        _ ->
          Nothing)
