{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aviation.Casr.Logbook.Types.Media.MediaType(
  MediaType(..)
, HasMediaType(..)
, AsMediaType(..)
) where

import Control.Lens(makeClassy, makeClassyPrisms)
import Data.Eq(Eq)
import Data.Ord(Ord)
import GHC.Generics
import Prelude(Show)

data MediaType =
  Image
  | Video
  | Audio
  | TrackLog
  | Document
  | Link
  deriving (Eq, Ord, Show, Generic)

makeClassy ''MediaType
makeClassyPrisms ''MediaType
