{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.CASARegistration(
  module Alpha
, CASARegistration(..)
, ManyCASARegistration(..)
, HasCASARegistration(..)
, AsCASARegistration(..)
, upperCASARegistration
) where

import Control.Lens
import Data.Char.Alpha as Alpha
import GHC.Generics
import Prelude

data CASARegistration =
  CASARegistration
    Upper
    Upper
    Upper
  deriving (Eq, Ord, Show, Generic)

class ManyCASARegistration a where
  _CASARegistration_ ::
    Traversal' a CASARegistration

instance ManyCASARegistration CASARegistration where
  _CASARegistration_ =
    id
    
class ManyCASARegistration a => HasCASARegistration a where
  casaRegistration ::
    Lens' a CASARegistration
  upper1CASARegistration ::
    Lens' a Upper
  upper1CASARegistration =
    casaRegistration . upper1CASARegistration
  upper2CASARegistration ::
    Lens' a Upper
  upper2CASARegistration =
    casaRegistration . upper2CASARegistration
  upper3CASARegistration ::
    Lens' a Upper
  upper3CASARegistration =
    casaRegistration . upper3CASARegistration

instance HasCASARegistration CASARegistration where
  casaRegistration =
    id
  upper1CASARegistration f (CASARegistration c1 c2 c3) =
    fmap (\c1' -> CASARegistration c1' c2 c3) (f c1)
  upper2CASARegistration f (CASARegistration c1 c2 c3) =
    fmap (\c2' -> CASARegistration c1 c2' c3) (f c2)
  upper3CASARegistration f (CASARegistration c1 c2 c3) =
    fmap (\c3' -> CASARegistration c1 c2 c3') (f c3)

class ManyCASARegistration a => AsCASARegistration a where
  _CASARegistration ::
    Prism' a CASARegistration
  
instance AsCASARegistration CASARegistration where
  _CASARegistration =
    id

upperCASARegistration ::
  Traversal' CASARegistration Upper
upperCASARegistration f (CASARegistration c1 c2 c3) =
  CASARegistration <$> f c1 <*> f c2 <*> f c3
