{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.CASARegistration(
  module Alpha
, CASARegistration(..)
) where

import Control.Lens
import Data.Char.Alpha as Alpha
import GHC.Generics
import Prelude

data CASARegistration =
  CASARegistration {
    upper1CASARegistration :: Upper
  , upper2CASARegistration :: Upper
  , upper3CASARegistration :: Upper
  } deriving (Eq, Ord, Show, Generic)

makeClassy ''CASARegistration

class AsCASARegistration a where
  _CASARegistration ::
    Prism' a CASARegistration

instance AsCASARegistration CASARegistration where
  _CASARegistration =
    id

upperCASARegistration ::
  Traversal' CASARegistration Upper
upperCASARegistration f (CASARegistration c1 c2 c3) =
  CASARegistration <$> f c1 <*> f c2 <*> f c3
