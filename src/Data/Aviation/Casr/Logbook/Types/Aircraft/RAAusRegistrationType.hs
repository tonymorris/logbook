{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationType where

import Control.Lens
import GHC.Generics
import Prelude

data RAAusRegistrationType =
  RAAusRegistrationTypeProvisional
  | RAAusRegistrationTypeFull
  | RAAusRegistrationTypeSuspended
  | RAAusRegistrationTypeDeregistered
  deriving (Eq, Ord, Show, Generic)

class AsRAAusRegistrationType a where
  _RAAusRegistrationType ::
    Prism' a RAAusRegistrationType
  _RAAusRegistrationTypeProvisional ::
    Prism' a ()
  _RAAusRegistrationTypeFull ::
    Prism' a ()
  _RAAusRegistrationTypeSuspended ::
    Prism' a ()
  _RAAusRegistrationTypeDeregistered ::
    Prism' a ()
  
instance AsRAAusRegistrationType RAAusRegistrationType where
  _RAAusRegistrationType =
    id
  _RAAusRegistrationTypeProvisional =
    prism'
      (\() -> RAAusRegistrationTypeProvisional)
      (\case
        RAAusRegistrationTypeProvisional ->
          Just ()
        _ ->
          Nothing)
  _RAAusRegistrationTypeFull =
    prism'
      (\() -> RAAusRegistrationTypeFull)
      (\case
        RAAusRegistrationTypeFull ->
          Just ()
        _ ->
          Nothing)
  _RAAusRegistrationTypeSuspended =
    prism'
      (\() -> RAAusRegistrationTypeSuspended)
      (\case
        RAAusRegistrationTypeSuspended ->
          Just ()
        _ ->
          Nothing)
  _RAAusRegistrationTypeDeregistered =
    prism'
      (\() -> RAAusRegistrationTypeDeregistered)
      (\case
        RAAusRegistrationTypeDeregistered ->
          Just ()
        _ ->
          Nothing)
