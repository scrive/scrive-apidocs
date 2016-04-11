module ServerUtils.BrandedImagesCache (
     BrandedImagesCache
   , BrandedImagesCacheKey(..)
  ) where

import Data.Hashable
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL

import KontraPrelude
import MemCache

type BrandedImagesCache = MemCache BrandedImagesCacheKey BSL.ByteString

data BrandedImagesCacheKey = BrandedImagesCacheKey {
    filename :: String
  , color    :: String
} deriving (Eq, Ord, Generic)

instance Hashable BrandedImagesCacheKey
