{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigits4(
  module Digit
, DecDigits4(..)
, ManyDecDigits4(..)
, HasDecDigits4(..)
, AsDecDigits4(..)
, digitDecDigits4
) where

import Control.Lens
import Data.Digit as Digit
import GHC.Generics
import Prelude

data DecDigits4 = 
  DecDigits4
    DecDigit
    DecDigit
    DecDigit
    DecDigit
  deriving (Eq, Ord, Show, Generic)

class ManyDecDigits4 a where
  _DecDigits4_ ::
    Traversal' a DecDigits4

instance ManyDecDigits4 DecDigits4 where
  _DecDigits4_ =
    id
    
class ManyDecDigits4 a => HasDecDigits4 a where
  decDigit4 ::
    Lens' a DecDigits4
  digit1_4 ::
    Lens' a DecDigit
  digit1_4 =
    decDigit4 . digit1_4
  digit2_4 ::
    Lens' a DecDigit
  digit2_4 =
    decDigit4 . digit2_4
  digit3_4 ::
    Lens' a DecDigit
  digit3_4 =
    decDigit4 . digit3_4
  digit4_4 ::
    Lens' a DecDigit
  digit4_4 =
    decDigit4 . digit4_4

instance HasDecDigits4 DecDigits4 where
  decDigit4 =
    id
  digit1_4 f (DecDigits4 d1_ d2_ d3_ d4_) =
    fmap (\d1_' -> DecDigits4 d1_' d2_ d3_ d4_) (f d1_)
  digit2_4 f (DecDigits4 d1_ d2_ d3_ d4_) =
    fmap (\d2_' -> DecDigits4 d1_ d2_' d3_ d4_) (f d2_)
  digit3_4 f (DecDigits4 d1_ d2_ d3_ d4_) =
    fmap (\d3_' -> DecDigits4 d1_ d2_ d3_' d4_) (f d3_)
  digit4_4 f (DecDigits4 d1_ d2_ d3_ d4_) =
    fmap (\d4_' -> DecDigits4 d1_ d2_ d3_ d4_') (f d4_)

class ManyDecDigits4 a => AsDecDigits4 a where
  _DecDigits4 ::
    Prism' a DecDigits4
  
instance AsDecDigits4 DecDigits4 where
  _DecDigits4 =
    id

digitDecDigits4 ::
  Traversal' DecDigits4 DecDigit
digitDecDigits4 f (DecDigits4 d1_ d2_ d3_ d4_) =
  DecDigits4 <$> f d1_ <*> f d2_ <*> f d3_ <*> f d4_
