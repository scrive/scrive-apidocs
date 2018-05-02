{- |
 - This module provides a monad transformer to manage objects in S3 or Ceph
 - using caches if its configuration is given. The first cache is in memory and
 - the second in Redis. See the corresponding module for more details.
 -
 - It instantiates 'MonadFileStorage' which provides a common interface to store
 - some contents.
 -
 - /!\ WARNING: This might not be what you're looking for! If you seek to
 - manipulate files in the sense of Scrive, that is the ones defined in
 - 'File.File', you should go to 'File.Storage'.
 -}

module FileStorage
  ( module FileStorage.Class
  , FileStorageConfig
  , FileStorageT
  , runFileStorageT
  , FileMemCache
  , newFileMemCache
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Log
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Redis as R

import DB
import FileStorage.Amazon
import FileStorage.Amazon.Config
import FileStorage.Class
import qualified FileStorage.RedisCache as Redis
import qualified MemCache

type FileMemCache = MemCache.MemCache String BSL.ByteString

newFileMemCache :: MonadBase IO m => Int -> m FileMemCache
newFileMemCache = MemCache.new (fromInteger . toInteger . BSL.length)

type FileStorageConfig = (AmazonConfig, Maybe R.Connection, FileMemCache)

newtype FileStorageT m a
  = FileStorageT { unFileStorageT :: ReaderT FileStorageConfig m a }
  deriving ( Alternative, Applicative, Functor, Monad, MonadDB, MonadIO
           , MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b
           , MonadBaseControl b, MonadThrow, MonadCatch, MonadMask
           , MonadTransControl )

runFileStorageT :: Monad m => FileStorageConfig -> FileStorageT m a -> m a
runFileStorageT config = flip runReaderT config . unFileStorageT

getFileStorageConfig :: Monad m => FileStorageT m FileStorageConfig
getFileStorageConfig = FileStorageT ask

instance {-# OVERLAPPING #-} ( MonadBase IO m, MonadBaseControl IO m, MonadLog m
                             , MonadMask m )
    => MonadFileStorage (FileStorageT m) where
  saveNewContents url contents = do
    (amazonConfig, mRedisCache, memCache) <- getFileStorageConfig

    MemCache.invalidate memCache url
    whenJust mRedisCache $ \conn -> do
      Redis.deleteKey conn $ Redis.redisKeyFromURL url

    runAmazonMonadT amazonConfig $ saveNewContents url contents

    _ <- MemCache.fetch memCache url (return contents)
    whenJust mRedisCache $ \conn -> do
      Redis.redisPut "contents" (BSL.toStrict contents)
                     (conn, Redis.redisKeyFromURL url)

  getSavedContents url = do
    (amazonConfig, mRedisCache, memCache) <- getFileStorageConfig

    MemCache.fetch_ memCache url $ do
      Redis.mfetch mRedisCache (Redis.redisKeyFromURL url)
        Redis.getFileFromRedis
        (\mConnKey -> do
          contents <- runAmazonMonadT amazonConfig $ getSavedContents url
          whenJust mConnKey $ Redis.redisPut "contents" $ BSL.toStrict contents
          return contents)

  deleteSavedContents url = do
    (amazonConfig, mRedisCache, memCache) <- getFileStorageConfig

    MemCache.invalidate memCache url
    whenJust mRedisCache $ \conn ->
      Redis.deleteKey conn $ Redis.redisKeyFromURL url
    runAmazonMonadT amazonConfig $ deleteSavedContents url
