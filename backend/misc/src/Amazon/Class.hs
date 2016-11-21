{-# LANGUAGE OverlappingInstances #-}
module Amazon.Class where

import Control.Exception.Lifted
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Typeable
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

data AmazonException = AmazonException String
  deriving (Eq, Ord, Show, Typeable)
instance Exception AmazonException

-- | Generic, overlapping instance.
instance (
    Monad m
  , MonadTrans t
  , AmazonMonad m
  ) => AmazonMonad (t m) where
    getAmazonConfig = lift getAmazonConfig
