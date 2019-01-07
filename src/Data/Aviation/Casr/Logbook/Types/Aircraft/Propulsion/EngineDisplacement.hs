{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement where

import Control.Lens
import GHC.Generics(Generic)
import Natural(Positive, HasPositive(positive))
import Prelude

type family XEngineDisplacement x

data EngineDisplacement x =
  EngineDisplacement
    !(XEngineDisplacement x)
    Positive
  deriving Generic

deriving instance Eq (XEngineDisplacement x) =>
  Eq (EngineDisplacement x)

deriving instance Ord (XEngineDisplacement x) =>
  Ord (EngineDisplacement x)

deriving instance Show (XEngineDisplacement x) =>
  Show (EngineDisplacement x)

class HasEngineDisplacement a e | a -> e where
  engineDisplacement ::
    Lens' a (EngineDisplacement e)
  xEngineDisplacement ::
    Lens' a (XEngineDisplacement e)
  xEngineDisplacement =
    engineDisplacement . xEngineDisplacement

instance HasEngineDisplacement (EngineDisplacement e) e where
  engineDisplacement =
    id
  xEngineDisplacement f (EngineDisplacement x n) =
    fmap (\x' -> EngineDisplacement x' n) (f x)

class AsEngineDisplacement a e | a -> e where
  _EngineDisplacement ::
    Prism' a (EngineDisplacement e)
 
instance AsEngineDisplacement (EngineDisplacement e) e where
  _EngineDisplacement =
    id

instance EngineDisplacement_ ~ x => Rewrapped EngineDisplacement_ x
instance Wrapped EngineDisplacement_ where
  type Unwrapped EngineDisplacement_ =
    Positive
  _Wrapped' =
    iso
      (\(EngineDisplacement () x) -> x)
      (EngineDisplacement ())

type EngineDisplacement_ =
  EngineDisplacement ()

type instance XEngineDisplacement () =
  ()

pattern EngineDisplacement_ ::
  Positive
  -> EngineDisplacement_
pattern EngineDisplacement_ p <- EngineDisplacement _ p
  where EngineDisplacement_ p = EngineDisplacement () p

----

instance HasPositive (EngineDisplacement x) where
  positive f (EngineDisplacement x p) =
    fmap (\p' -> EngineDisplacement x p') (f p)
