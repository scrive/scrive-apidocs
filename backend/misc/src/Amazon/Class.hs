{-# LANGUAGE OverlappingInstances #-}
module Amazon.Class where

import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Database.Redis as R

import File.FileID
import KontraPrelude
import MemCache

data AmazonConfig = AmazonConfig {
    awsConfig      :: !(Maybe (String, String, String))
  , awsLocalCache  :: !(MemCache FileID ByteString)
  , awsGlobalCache :: !(Maybe R.Connection)
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
