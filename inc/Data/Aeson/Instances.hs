{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Aeson.Instances () where

import Control.Exception
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Text.Encoding

import KontraPrelude

instance ToJSON ByteString where
  toJSON = toJSON . either throw id . decodeUtf8'

instance FromJSON ByteString where
  parseJSON = fmap encodeUtf8 . parseJSON
