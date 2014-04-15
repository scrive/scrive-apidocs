{-# LANGUAGE OverlappingInstances #-}
module Amazon.Class where

import Control.Monad.Trans
import Data.ByteString (ByteString)
import File.FileID
import MemCache

data AmazonConfig = AmazonConfig {
  amazonConfig :: Maybe (String, String, String)
, fileCache    :: MemCache FileID ByteString
}

class AmazonMonad m where
  getAmazonConfig :: m AmazonConfig

-- | Generic, overlapping instance.
instance (
    Monad m
  , MonadTrans t
  , AmazonMonad m
  ) => AmazonMonad (t m) where
    getAmazonConfig = lift getAmazonConfig
