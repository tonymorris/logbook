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
{-# LANGUAGE DefaultSignatures #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement where

import Control.Lens
import GHC.Generics(Generic)
import Natural(Positive)
import Prelude

type family XEngineDisplacement x

data EngineDisplacement_ x =
  EngineDisplacement_
    !(XEngineDisplacement x)
    Positive
  deriving Generic

deriving instance Eq (XEngineDisplacement x) =>
  Eq (EngineDisplacement_ x)

deriving instance Ord (XEngineDisplacement x) =>
  Ord (EngineDisplacement_ x)

deriving instance Show (XEngineDisplacement x) =>
  Show (EngineDisplacement_ x)

class HasEngineDisplacement a e | a -> e where
  engineDisplacement ::
    Lens' a (EngineDisplacement_ e)
  xEngineDisplacement ::
    XEngineDisplacement e ~ x =>
    Lens' a x
  default xEngineDisplacement ::
    XEngineDisplacement () ~ x =>
    Lens' a x
  xEngineDisplacement f a =
    fmap (\() -> a) (f ())

instance HasEngineDisplacement (EngineDisplacement_ e) e where
  engineDisplacement =
    id
  xEngineDisplacement f (EngineDisplacement_ x n) =
    fmap (\x' -> EngineDisplacement_ x' n) (f x)

class AsEngineDisplacement a e | a -> e where
  _EngineDisplacement ::
    Prism' a (EngineDisplacement_ e)
 
instance AsEngineDisplacement (EngineDisplacement_ e) e where
  _EngineDisplacement =
    id

instance EngineDisplacement ~ x => Rewrapped EngineDisplacement x
instance Wrapped EngineDisplacement where
  type Unwrapped EngineDisplacement =
    Positive
  _Wrapped' =
    iso
      (\(EngineDisplacement_ () x) -> x)
      (EngineDisplacement_ ())

type EngineDisplacement =
  EngineDisplacement_ ()

type instance XEngineDisplacement () =
  ()

pattern EngineDisplacement ::
  Positive
  -> EngineDisplacement
pattern EngineDisplacement p <- EngineDisplacement_ _ p
  where EngineDisplacement p = EngineDisplacement_ () p
