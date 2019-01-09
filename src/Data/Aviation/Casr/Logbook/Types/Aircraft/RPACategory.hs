{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsions1(Propulsions1)
import GHC.Generics(Generic)
import Natural(Positive)
import Prelude
 
data RPACategory =
  RPAAeroplane Propulsions1
  | RPACopter Propulsions1 Positive
  | RPAAirship Propulsions1
  | RPAPoweredLift Propulsions1
  | XRPACategory
  deriving (Eq, Ord, Show, Generic)

class HasRPACategory a where
  rpaCategory ::
    Lens' a RPACategory

instance HasRPACategory RPACategory where
  rpaCategory =
    id

class AsRPACategory a where
  _RPACategory ::
    Prism' a RPACategory
  _RPAAeroplane ::
    Prism' a Propulsions1
  _RPACopter ::
    Prism' a  (Propulsions1, Positive)
  _RPAAirship ::
    Prism' a Propulsions1
  _RPAPoweredLift ::
    Prism' a Propulsions1

instance AsRPACategory RPACategory where
  _RPACategory =
    id
  _RPAAeroplane =
    prism'
      RPAAeroplane
      (
        \case
          RPAAeroplane p ->
            Just p
          _ ->
            Nothing
      )
  _RPACopter =
    prism'
      (\(p, n) -> RPACopter p n)
      (
        \case
          RPACopter p n ->
            Just (p, n)
          _ ->
            Nothing
      )
  _RPAAirship =
    prism'
      RPAAirship
      (
        \case
          RPAAirship p ->
            Just p
          _ ->
            Nothing
      )
  _RPAPoweredLift =
    prism'
      RPAPoweredLift
      (
        \case
          RPAPoweredLift p ->
            Just p
          _ ->
            Nothing
      )
