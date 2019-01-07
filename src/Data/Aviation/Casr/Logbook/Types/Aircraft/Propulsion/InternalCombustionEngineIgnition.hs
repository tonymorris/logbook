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

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.InternalCombustionEngineIgnition where

import Control.Lens(Lens', Prism', prism')
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Prelude

type family XDiesel x
type family XSpark x
type family XInternalCombustionEngineIgnition x

data InternalCombustionEngineIgnition x =
  Diesel !(XDiesel x)
  | Spark !(XSpark x)
  | InternalCombustionEngineIgnition !(XInternalCombustionEngineIgnition x)
  deriving Generic

deriving instance (Eq (XDiesel x), Eq (XSpark x), Eq (XInternalCombustionEngineIgnition x)) =>
  Eq (InternalCombustionEngineIgnition x)

deriving instance (Ord (XDiesel x), Ord (XSpark x), Ord (XInternalCombustionEngineIgnition x)) =>
  Ord (InternalCombustionEngineIgnition x)

deriving instance (Show (XDiesel x), Show (XSpark x), Show (XInternalCombustionEngineIgnition x)) =>
  Show (InternalCombustionEngineIgnition x)

class HasInternalCombustionEngineIgnition a e | a -> e where
  internalCombustionEngineIgnition ::
    Lens' a (InternalCombustionEngineIgnition e)
  xInternalCombustionEngineIgnition ::
    Lens' a (XInternalCombustionEngineIgnition e)
  xInternalCombustionEngineIgnition =
    internalCombustionEngineIgnition . xInternalCombustionEngineIgnition

instance HasInternalCombustionEngineIgnition (InternalCombustionEngineIgnition e) e where
  internalCombustionEngineIgnition =
    id
    
xInternalCombustionEngineIgnition' ::
  (
    XDiesel e ~ x
  , XSpark e ~ x
  , XInternalCombustionEngineIgnition e ~ Void
  ) =>
  Lens' (InternalCombustionEngineIgnition e) x
xInternalCombustionEngineIgnition' f (Diesel x) =
  fmap Diesel (f x)
xInternalCombustionEngineIgnition' f (Spark x) =
  fmap Spark (f x)
xInternalCombustionEngineIgnition' _ (InternalCombustionEngineIgnition x) =
  absurd x

class AsInternalCombustionEngineIgnition a e | a -> e where
  _InternalCombustionEngineIgnition ::
    Prism' a (InternalCombustionEngineIgnition e)
  _XDiesel ::
    Prism' a (XDiesel e)
  _XSpark ::
    Prism' a (XSpark e)
  _XInternalCombustionEngineIgnition ::
    Prism' a (XInternalCombustionEngineIgnition e)

instance AsInternalCombustionEngineIgnition (InternalCombustionEngineIgnition e) e where
  _InternalCombustionEngineIgnition =
    id
  _XDiesel =
    prism'
      Diesel
      (
        \case
          Diesel x ->
            Just x
          _ ->
            Nothing
      )
  _XSpark =
    prism'
      Spark
      (
        \case
          Spark x ->
            Just x
          _ ->
            Nothing
      )
  _XInternalCombustionEngineIgnition =
    prism'
      InternalCombustionEngineIgnition
      (
        \case
          InternalCombustionEngineIgnition x ->
            Just x
          _ ->
            Nothing
      )

type InternalCombustionEngineIgnition_ =
  InternalCombustionEngineIgnition ()

type instance XDiesel () =
  ()
type instance XSpark () =
  ()
type instance XInternalCombustionEngineIgnition () =
  Void

pattern Diesel_ ::
  InternalCombustionEngineIgnition_
pattern Diesel_ <- Diesel _
  where Diesel_ = Diesel ()

pattern Spark_ ::
  InternalCombustionEngineIgnition_
pattern Spark_ <- Spark _
  where Spark_ = Spark ()

pattern InternalCombustionEngineIgnition_ ::
  Void
  -> InternalCombustionEngineIgnition_
pattern InternalCombustionEngineIgnition_ v <- InternalCombustionEngineIgnition v
  where InternalCombustionEngineIgnition_ v = InternalCombustionEngineIgnition v
