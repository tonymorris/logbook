{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.PistonEngineCycle where

import Control.Lens(Lens', Prism', prism')
import GHC.Generics(Generic)
import Prelude

data PistonEngineCycle =
  FourStroke
  | TwoStroke
  deriving (Eq, Ord, Show, Generic)

class HasPistonEngineCycle a where
  pistonEngineCycle ::
    Lens' a PistonEngineCycle

instance HasPistonEngineCycle PistonEngineCycle where
  pistonEngineCycle =
    id

class AsPistonEngineCycle a where
  _PistonEngineCycle ::
    Prism' a PistonEngineCycle
  _FourStroke ::
    Prism' a ()
  _TwoStroke ::
    Prism' a ()

instance AsPistonEngineCycle PistonEngineCycle where
  _PistonEngineCycle =
    id
  _FourStroke =
    prism'
      (\() -> FourStroke)
      (
        \case
          FourStroke ->
            Just ()
          _ ->
            Nothing
      )
  _TwoStroke =
    prism'
      (\() -> TwoStroke)
      (
        \case
          TwoStroke ->
            Just ()
          _ ->
            Nothing
      )
