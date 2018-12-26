{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Aircraft.DecDigits4(
  module Digit
, DecDigits4(..)
, digitDecDigits4
) where

import Control.Lens
import Data.Digit as Digit
import GHC.Generics
import Prelude

data DecDigits4 = 
  DecDigits4 {
    _digit1_4 ::
      DecDigit
  , _digit2_4 ::
      DecDigit
  , _digit3_4 ::
      DecDigit
  , _digit4_4 ::
      DecDigit
  } deriving (Eq, Ord, Show, Generic)

makeClassy ''DecDigits4

class AsDecDigits4 a where
  _DecDigits4 ::
    Prism' a DecDigits4

instance AsDecDigits4 DecDigits4 where
  _DecDigits4 =
    id

digitDecDigits4 ::
  Traversal' DecDigits4 DecDigit
digitDecDigits4 f (DecDigits4 d1_ d2_ d3_ d4_) =
  DecDigits4 <$> f d1_ <*> f d2_ <*> f d3_ <*> f d4_
