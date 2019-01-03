{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.RPACategory where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.Propulsions1(Propulsions1)
import Data.Void(Void, absurd)
import GHC.Generics(Generic)
import Natural(Positive)
import Prelude
import Util(unproduct)
 
type family XRPAAeroplane x
type family XRPACopter x
type family XRPAAirship x
type family XRPAPoweredLift x
type family XRPACategory x

data RPACategory_ x =
  RPAAeroplane_ !(XRPAAeroplane x) Propulsions1
  | RPACopter_ !(XRPACopter x) Propulsions1 Positive
  | RPAAirship_ !(XRPAAirship x) Propulsions1
  | RPAPoweredLift_ !(XRPAPoweredLift x) Propulsions1
  | XRPACategory_ !(XRPACategory x)
  deriving Generic

deriving instance (Eq (XRPAAeroplane x), Eq (XRPACopter x), Eq (XRPAAirship x), Eq (XRPAPoweredLift x), Eq (XRPACategory x)) =>
  Eq (RPACategory_ x)

deriving instance (Ord (XRPAAeroplane x), Ord (XRPACopter x), Ord (XRPAAirship x), Ord (XRPAPoweredLift x), Ord (XRPACategory x)) =>
  Ord (RPACategory_ x)

deriving instance (Show (XRPAAeroplane x), Show (XRPACopter x), Show (XRPAAirship x), Show (XRPAPoweredLift x), Show (XRPACategory x)) =>
  Show (RPACategory_ x)

class HasRPACategory a e | a -> e where
  rpaCategory ::
    Lens' a (RPACategory_ e)
  xRPACategory ::
    (
      XRPAAeroplane e ~ x
    , XRPACopter e ~ x
    , XRPAAirship e ~ x
    , XRPAPoweredLift e ~ x
    , XRPACategory e ~ Void
    ) =>
    Lens' a x
  default xRPACategory ::
    (
      XRPAAeroplane () ~ x
    , XRPACopter () ~ x
    , XRPAAirship () ~ x
    , XRPAPoweredLift () ~ x
    , XRPACategory e ~ Void
    ) =>
    Lens' a x
  xRPACategory f a =
    fmap (\() -> a) (f ())
  
instance HasRPACategory (RPACategory_ e) e where
  rpaCategory =
    id
  xRPACategory f (RPAAeroplane_ x p) =
    fmap (\x' -> RPAAeroplane_ x' p) (f x)
  xRPACategory f (RPACopter_ x p n) =
    fmap (\x' -> RPACopter_ x' p n) (f x)
  xRPACategory f (RPAAirship_ x p) =
    fmap (\x' -> RPAAirship_ x' p) (f x)
  xRPACategory f (RPAPoweredLift_ x p) =
    fmap (\x' -> RPAPoweredLift_ x' p) (f x)
  xRPACategory _ (XRPACategory_ x) =
    absurd x

class AsRPACategory a e | a -> e where
  _RPACategory ::
    Prism' a (RPACategory_ e)
  _RPAAeroplane ::
    Prism' a (XRPAAeroplane e, Propulsions1)
  _RPAAeroplane' ::
    XRPAAeroplane e ~ () =>
    Prism' a Propulsions1
  _RPAAeroplane' =
    _RPAAeroplane . unproduct
  _RPACopter ::
    Prism' a (XRPACopter e, (Propulsions1, Positive))
  _RPACopter' ::
    XRPACopter e ~ () =>
    Prism' a (Propulsions1, Positive)
  _RPACopter' =
    _RPACopter . unproduct
  _RPAAirship ::
    Prism' a (XRPAAirship e, Propulsions1)
  _RPAAirship' ::    
    XRPAAirship e ~ () =>
    Prism' a Propulsions1
  _RPAAirship' =
    _RPAAirship . unproduct
  _RPAPoweredLift ::
    Prism' a (XRPAPoweredLift e, Propulsions1)
  _RPAPoweredLift' ::    
    XRPAPoweredLift e ~ () =>
    Prism' a Propulsions1
  _RPAPoweredLift' =
    _RPAPoweredLift . unproduct
  _XRPACategory ::
    Prism' a (XRPACategory e)

instance AsRPACategory (RPACategory_ e) e where
  _RPACategory =
    id
  _RPAAeroplane =
    prism'
      (\(x, p) -> RPAAeroplane_ x p)
      (
        \case
          RPAAeroplane_ x p ->
            Just (x, p)
          _ ->
            Nothing
      )
  _RPACopter =
    prism'
      (\(x, (p, n)) -> RPACopter_ x p n)
      (
        \case
          RPACopter_ x p n ->
            Just (x, (p, n))
          _ ->
            Nothing
      )
  _RPAAirship =
    prism'
      (\(x, p) -> RPAAirship_ x p)
      (
        \case
          RPAAirship_ x p ->
            Just (x, p)
          _ ->
            Nothing
      )
  _RPAPoweredLift =
    prism'
      (\(x, p) -> RPAPoweredLift_ x p)
      (
        \case
          RPAPoweredLift_ x p ->
            Just (x, p)
          _ ->
            Nothing
      )
  _XRPACategory =
    prism'
      XRPACategory_
      (
        \case
          XRPACategory_ x ->
            Just x
          _ ->
            Nothing
      )

type RPACategory =
  RPACategory_ ()

type instance XRPAAeroplane () =
  ()
type instance XRPACopter () =
  ()
type instance XRPAAirship () =
  ()
type instance XRPAPoweredLift () =
  ()
type instance XRPACategory () =
  Void

pattern RPAAeroplane ::
  Propulsions1
  -> RPACategory
pattern RPAAeroplane p <- RPAAeroplane_ _ p
  where RPAAeroplane p = RPAAeroplane_ () p

pattern RPACopter ::
  Propulsions1
  -> Positive
  -> RPACategory
pattern RPACopter p n <- RPACopter_ _ p n
  where RPACopter p n = RPACopter_ () p n

pattern RPAAirship ::
  Propulsions1
  -> RPACategory
pattern RPAAirship p <- RPAAirship_ _ p
  where RPAAirship p = RPAAirship_ () p

pattern RPAPoweredLift ::
  Propulsions1
  -> RPACategory
pattern RPAPoweredLift p <- RPAPoweredLift_ _ p
  where RPAPoweredLift p = RPAPoweredLift_ () p

pattern XRPACategory ::
  Void
  -> RPACategory
pattern XRPACategory v <- XRPACategory_ v
  where XRPACategory v = XRPACategory_ v
