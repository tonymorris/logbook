{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RAAusRegistrationPrefix where

import Control.Lens
import GHC.Generics
import Prelude

data RAAusRegistrationPrefix =
  RAAusRegistrationPrefix10
  | RAAusRegistrationPrefix19
  | RAAusRegistrationPrefix24
  | RAAusRegistrationPrefix28
  | RAAusRegistrationPrefix32
  | RAAusRegistrationPrefix55
  deriving (Eq, Ord, Show, Generic)

class ManyRAAusRegistrationPrefix a where
  _RAAusRegistrationPrefix_ ::
    Traversal' a RAAusRegistrationPrefix

instance ManyRAAusRegistrationPrefix RAAusRegistrationPrefix where
  _RAAusRegistrationPrefix_ =
    id
    
class ManyRAAusRegistrationPrefix a => HasRAAusRegistrationPrefix a where
  rAAusRegistrationPrefix ::
    Lens' a RAAusRegistrationPrefix

instance HasRAAusRegistrationPrefix RAAusRegistrationPrefix where
  rAAusRegistrationPrefix =
    id

class ManyRAAusRegistrationPrefix a => AsRAAusRegistrationPrefix a where
  _RAAusRegistrationPrefix ::
    Prism' a RAAusRegistrationPrefix
  _RAAusRegistrationPrefix10 ::
    Prism' a ()
  _RAAusRegistrationPrefix19 ::
    Prism' a ()
  _RAAusRegistrationPrefix24 ::
    Prism' a ()
  _RAAusRegistrationPrefix28 ::
    Prism' a ()
  _RAAusRegistrationPrefix32 ::
    Prism' a ()
  _RAAusRegistrationPrefix55 ::
    Prism' a ()
  _RAAusRegistrationPrefix10 =
    _RAAusRegistrationPrefix . _RAAusRegistrationPrefix10
  _RAAusRegistrationPrefix19 =
    _RAAusRegistrationPrefix . _RAAusRegistrationPrefix19
  _RAAusRegistrationPrefix24 = 
    _RAAusRegistrationPrefix . _RAAusRegistrationPrefix24
  _RAAusRegistrationPrefix28 = 
    _RAAusRegistrationPrefix . _RAAusRegistrationPrefix28
  _RAAusRegistrationPrefix32 = 
    _RAAusRegistrationPrefix . _RAAusRegistrationPrefix32
  _RAAusRegistrationPrefix55 = 
    _RAAusRegistrationPrefix . _RAAusRegistrationPrefix55

instance AsRAAusRegistrationPrefix RAAusRegistrationPrefix where
  _RAAusRegistrationPrefix =
    id
  _RAAusRegistrationPrefix10 =
    prism'
      (\() -> RAAusRegistrationPrefix10)
      (\case
        RAAusRegistrationPrefix10 ->
          Just ()
        _ ->
          Nothing)
  _RAAusRegistrationPrefix19 =
    prism'
      (\() -> RAAusRegistrationPrefix19)
      (\case
        RAAusRegistrationPrefix19 ->
          Just ()
        _ ->
          Nothing)
  _RAAusRegistrationPrefix24 =
    prism'
      (\() -> RAAusRegistrationPrefix24)
      (\case
        RAAusRegistrationPrefix24 ->
          Just ()
        _ ->
          Nothing)
  _RAAusRegistrationPrefix28 =
    prism'
      (\() -> RAAusRegistrationPrefix28)
      (\case
        RAAusRegistrationPrefix28 ->
          Just ()
        _ ->
          Nothing)
  _RAAusRegistrationPrefix32 =
    prism'
      (\() -> RAAusRegistrationPrefix32)
      (\case
        RAAusRegistrationPrefix32 ->
          Just ()
        _ ->
          Nothing)
  _RAAusRegistrationPrefix55 =
    prism'
      (\() -> RAAusRegistrationPrefix55)
      (\case
        RAAusRegistrationPrefix55 ->
          Just ()
        _ ->
          Nothing)
