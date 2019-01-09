{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion.EngineDisplacement where

import Control.Lens
import GHC.Generics(Generic)
import Natural(Positive, HasPositive(positive))
import Prelude

data EngineDisplacement =
  EngineDisplacement
    Positive
  deriving (Eq, Ord, Show, Generic)

class HasEngineDisplacement a where
  engineDisplacement ::
    Lens' a EngineDisplacement

instance HasEngineDisplacement EngineDisplacement where
  engineDisplacement =
    id

class AsEngineDisplacement a where
  _EngineDisplacement ::
    Prism' a EngineDisplacement
 
instance AsEngineDisplacement EngineDisplacement where
  _EngineDisplacement =
    id

instance EngineDisplacement ~ x =>
  Rewrapped EngineDisplacement x

instance Wrapped EngineDisplacement where
  type Unwrapped EngineDisplacement =
    Positive
  _Wrapped' =
    iso
      (\(EngineDisplacement x) -> x)
      EngineDisplacement

----

instance HasPositive EngineDisplacement where
  positive f (EngineDisplacement p) =
    fmap (\p' -> EngineDisplacement p') (f p)
