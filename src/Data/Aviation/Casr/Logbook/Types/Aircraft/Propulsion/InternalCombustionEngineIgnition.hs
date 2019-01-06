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

data InternalCombustionEngineIgnition_ x =
  Diesel_ !(XDiesel x)
  | Spark_ !(XSpark x)
  | InternalCombustionEngineIgnition_ !(XInternalCombustionEngineIgnition x)
  deriving Generic

deriving instance (Eq (XDiesel x), Eq (XSpark x), Eq (XInternalCombustionEngineIgnition x)) =>
  Eq (InternalCombustionEngineIgnition_ x)

deriving instance (Ord (XDiesel x), Ord (XSpark x), Ord (XInternalCombustionEngineIgnition x)) =>
  Ord (InternalCombustionEngineIgnition_ x)

deriving instance (Show (XDiesel x), Show (XSpark x), Show (XInternalCombustionEngineIgnition x)) =>
  Show (InternalCombustionEngineIgnition_ x)

class HasInternalCombustionEngineIgnition a e | a -> e where
  internalCombustionEngineIgnition ::
    Lens' a (InternalCombustionEngineIgnition_ e)
  xInternalCombustionEngineIgnition ::
    Lens' a (XInternalCombustionEngineIgnition e)
  xInternalCombustionEngineIgnition =
    internalCombustionEngineIgnition . xInternalCombustionEngineIgnition

instance HasInternalCombustionEngineIgnition (InternalCombustionEngineIgnition_ e) e where
  internalCombustionEngineIgnition =
    id
    
xInternalCombustionEngineIgnition' ::
  (
    XDiesel e ~ x
  , XSpark e ~ x
  , XInternalCombustionEngineIgnition e ~ Void
  ) =>
  Lens' (InternalCombustionEngineIgnition_ e) x
xInternalCombustionEngineIgnition' f (Diesel_ x) =
  fmap Diesel_ (f x)
xInternalCombustionEngineIgnition' f (Spark_ x) =
  fmap Spark_ (f x)
xInternalCombustionEngineIgnition' _ (InternalCombustionEngineIgnition_ x) =
  absurd x

class AsInternalCombustionEngineIgnition a e | a -> e where
  _InternalCombustionEngineIgnition ::
    Prism' a (InternalCombustionEngineIgnition_ e)
  _XDiesel ::
    Prism' a (XDiesel e)
  _XSpark ::
    Prism' a (XSpark e)
  _XInternalCombustionEngineIgnition ::
    Prism' a (XInternalCombustionEngineIgnition e)

instance AsInternalCombustionEngineIgnition (InternalCombustionEngineIgnition_ e) e where
  _InternalCombustionEngineIgnition =
    id
  _XDiesel =
    prism'
      Diesel_
      (
        \case
          Diesel_ x ->
            Just x
          _ ->
            Nothing
      )
  _XSpark =
    prism'
      Spark_
      (
        \case
          Spark_ x ->
            Just x
          _ ->
            Nothing
      )
  _XInternalCombustionEngineIgnition =
    prism'
      InternalCombustionEngineIgnition_
      (
        \case
          InternalCombustionEngineIgnition_ x ->
            Just x
          _ ->
            Nothing
      )

type InternalCombustionEngineIgnition =
  InternalCombustionEngineIgnition_ ()

type instance XDiesel () =
  ()
type instance XSpark () =
  ()
type instance XInternalCombustionEngineIgnition () =
  Void

pattern Diesel ::
  InternalCombustionEngineIgnition
pattern Diesel <- Diesel_ _
  where Diesel = Diesel_ ()

pattern Spark ::
  InternalCombustionEngineIgnition
pattern Spark <- Spark_ _
  where Spark = Spark_ ()

pattern InternalCombustionEngineIgnition ::
  Void
  -> InternalCombustionEngineIgnition
pattern InternalCombustionEngineIgnition v <- InternalCombustionEngineIgnition_ v
  where InternalCombustionEngineIgnition v = InternalCombustionEngineIgnition_ v
