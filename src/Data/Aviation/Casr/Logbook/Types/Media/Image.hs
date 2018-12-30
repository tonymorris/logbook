{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Types.Media.Image where

import Control.Lens
import Data.Aviation.Casr.Logbook.Types.Media.ImageType
import Prelude

data Image =
  Image {
    _imageuri :: String
  , _imagetype :: ImageType
  , _imagesource :: String
  , _imagename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''Image

class AsImage a where
  _Image ::
    Prism' a Image

instance AsImage Image where
  _Image =
    id
