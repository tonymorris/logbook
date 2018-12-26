{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.Propulsion(
  module Bool
, Propulsion(..)
, Propulsion'
, PropulsionI
, propulsionI
) where

import Control.Applicative(Applicative(pure))
import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Aircraft.JetType
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionPosition
import Data.Aviation.Casr.Logbook.Types.Aircraft.PropulsionType
import GHC.Generics
import Data.Bool as Bool(Bool(False, True))
import Prelude

data Propulsion cylinders displacement jettype position vtol =
  Propulsion {
    _propulsion_type ::
      PropulsionType cylinders displacement jettype
  , _propulsion_position ::
      position PropulsionPosition
  , _vtol ::
      vtol Bool
  } deriving Generic

makeClassy ''Propulsion

__propulsion_type ::
  Lens (Propulsion cylinders displacement jettype position vtol) (Propulsion cylinders' displacement' jettype' position vtol) (PropulsionType cylinders displacement jettype) (PropulsionType cylinders' displacement' jettype')
__propulsion_type f (Propulsion t p v) =
  fmap (\t' -> Propulsion t' p v) (f t)

__propulsion_position ::
  Lens (Propulsion cylinders displacement jettype position vtol) (Propulsion cylinders displacement jettype position' vtol) (position PropulsionPosition) (position' PropulsionPosition)
__propulsion_position f (Propulsion t p v) =
  fmap (\p' -> Propulsion t p' v) (f p)

__vtol ::
  Lens (Propulsion cylinders displacement jettype position vtol) (Propulsion cylinders displacement jettype position vtol') (vtol Bool) (vtol' Bool)
__vtol f (Propulsion t p v) =
  fmap (\v' -> Propulsion t p v') (f v)

class AsPropulsion a cylinders displacement jettype position vtol | a ->  cylinders displacement jettype position vtol where
  _Propulsion ::
    Prism' a (Propulsion cylinders displacement jettype position vtol)

instance AsPropulsion (Propulsion cylinders displacement jettype position vtol) cylinders displacement jettype position vtol where
  _Propulsion =
    id

type Propulsion' a =
  Propulsion a a a a a
  
type PropulsionI =
  Propulsion' Identity

propulsionI ::
  (Applicative position, Applicative vtol) =>
  PropulsionType cylinders displacement jettype
  -> PropulsionPosition
  -> Bool
  -> Propulsion cylinders displacement jettype position vtol
propulsionI propulsiontype_ propulsionposition_ vtol_ =
  Propulsion propulsiontype_ (pure propulsionposition_) (pure vtol_)

deriving instance (Eq (cylinders Positive), Eq (displacement Positive), Eq (jettype JetType), Eq (position PropulsionPosition), Eq (vtol Bool)) => Eq (Propulsion cylinders displacement jettype position vtol)

deriving instance (Ord (cylinders Positive), Ord (displacement Positive), Ord (jettype JetType), Ord (position PropulsionPosition), Ord (vtol Bool)) => Ord (Propulsion cylinders displacement jettype position vtol)

deriving instance (Show (cylinders Positive), Show (displacement Positive), Show (jettype JetType), Show (position PropulsionPosition), Show (vtol Bool)) => Show (Propulsion cylinders displacement jettype position vtol)
