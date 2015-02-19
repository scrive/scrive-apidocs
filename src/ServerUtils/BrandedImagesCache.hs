{-# LANGUAGE ExtendedDefaultRules #-}
module ServerUtils.BrandedImagesCache (
     BrandedImagesCache
   , BrandedImagesCacheKey(..)
  ) where

import qualified Data.ByteString.Lazy as BSL
import MemCache

type BrandedImagesCache = MemCache BrandedImagesCacheKey BSL.ByteString

data BrandedImagesCacheKey = BrandedImagesCacheKey {
    filename :: String
  , color    :: String
} deriving (Eq,Ord)

