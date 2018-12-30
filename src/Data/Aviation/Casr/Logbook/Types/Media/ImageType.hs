{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Media.ImageType(
  ImageType(Jpg, Png, Gif)
, HasImageType(..)
, AsImageType(..)
) where

import Control.Lens(makeClassy, makeClassyPrisms)
import Data.Eq(Eq)
import Data.Ord(Ord)
import Prelude(Show)

data ImageType =
  Jpg
  | Png
  | Gif
  deriving (Eq, Ord, Show)

makeClassy ''ImageType
makeClassyPrisms ''ImageType
