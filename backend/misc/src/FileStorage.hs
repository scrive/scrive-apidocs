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
 - 'File.Types', you should go to 'File.Storage'.
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
import Data.IORef.Lifted
import Log
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Redis as R

import DB
import FileStorage.Amazon
import FileStorage.Amazon.S3Env
import FileStorage.Class
import Log.Utils
import qualified FileStorage.RedisCache as Redis
import qualified MemCache

type FileMemCache = MemCache.MemCache Text BSL.ByteString

newFileMemCache :: MonadBase IO m => Int -> m FileMemCache
newFileMemCache = MemCache.new (fromInteger . toInteger . BSL.length)

type FileStorageConfig = (AmazonS3Env, Maybe R.Connection, FileMemCache)

newtype FileStorageT m a
  = FileStorageT { unFileStorageT :: ReaderT FileStorageConfig m a }
  deriving ( Alternative, Applicative, Functor, Monad, MonadFail, MonadDB, MonadIO
           , MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b
           , MonadBaseControl b, MonadThrow, MonadCatch, MonadMask
           , MonadTransControl)

runFileStorageT :: Monad m => FileStorageConfig -> FileStorageT m a -> m a
runFileStorageT config = flip runReaderT config . unFileStorageT

getFileStorageConfig :: Monad m => FileStorageT m FileStorageConfig
getFileStorageConfig = FileStorageT ask

-- 'saveNewContents' invalidates the data already stored in the caches, uploads
-- the data to Amazon and puts them in the caches.
--
-- 'getSavedContents' tries to fetch the contents from the memory cache then
-- Redis and, if it's not in either of them, fetches them from Amazon. The data
-- will be added to the caches from which they were missing.
--
-- 'deleteSavedContents' first deletes the data from the caches then from Amazon
-- to prevent inconsistency.
instance (MonadBaseControl IO m, MonadCatch m, MonadLog m, MonadMask m)
    => MonadFileStorage (FileStorageT m) where
  saveNewContents     = saveNewContents_
  getSavedContents    = getSavedContents_
  deleteSavedContents = deleteSavedContents_

saveNewContents_
  :: (MonadBaseControl IO m, MonadCatch m, MonadLog m, MonadMask m)
  => Text
  -> BSL.ByteString
  -> FileStorageT m ()
saveNewContents_ url contents = do
  (amazonEnv, mRedisCache, memCache) <- getFileStorageConfig

  MemCache.invalidate memCache url
  whenJust mRedisCache $ \conn -> do
    Redis.deleteKey conn $ Redis.redisKeyFromURL url

  saveContentsToAmazon amazonEnv url contents

  void (MemCache.fetch memCache url (return contents)) `catch` \e ->
    logInfo "Failed to save in memory cache"
      $ object ["error" .= show (e :: SomeException)]

  whenJust mRedisCache $ \conn -> do
    Redis.redisPut "contents" (BSL.toStrict contents) (conn, Redis.redisKeyFromURL url)
      `catch` \e -> logInfo "Failed to save to Redis cache"
                $ object ["error" .= show (e :: SomeException)]

data FileOrigin = MemCacheOrigin | RedisOrigin | AmazonOrigin

getSavedContents_
  :: forall m
   . (MonadBaseControl IO m, MonadCatch m, MonadLog m, MonadMask m, MonadThrow m)
  => Text
  -> FileStorageT m BSL.ByteString
getSavedContents_ url = do
  (amazonEnv, mRedisCache, memCache) <- getFileStorageConfig
  rOrigin          <- newIORef MemCacheOrigin
  (contents, diff) <- timed . MemCache.fetch_ memCache url $ do
    (origin, contents) <- fetchFromRedisOrAmazon mRedisCache amazonEnv
    writeIORef rOrigin origin
    return contents
  origin <- readIORef rOrigin
  logInfo "File contents fetched successfully"
    $ object ["url" .= url, "origin" .= logOrigin origin, "time" .= diff]
  return contents
  where
    logOrigin :: FileOrigin -> Text
    logOrigin MemCacheOrigin = "memcache"
    logOrigin RedisOrigin    = "redis"
    logOrigin AmazonOrigin   = "amazon"

    fetchFromRedisOrAmazon
      :: Maybe R.Connection -> AmazonS3Env -> FileStorageT m (FileOrigin, BSL.ByteString)
    fetchFromRedisOrAmazon mRedisCache amazonEnv =
      Redis.mfetch
          mRedisCache
          (Redis.redisKeyFromURL url)
          (fetchFromRedis amazonEnv)
          (\mConnKey -> do
            (origin, contents) <- fetchFromAmazon amazonEnv
            whenJust mConnKey $ \connKey -> do
              Redis.redisPut "contents" (BSL.toStrict contents) connKey `catch` \e ->
                logInfo "Failed to save to Redis cache"
                  $ object ["error" .= show (e :: SomeException)]
            return (origin, contents)
          )

        `catch` \e -> case fromException e of
                  Just e' -> throwM (e' :: FileStorageException)
                  Nothing -> do
                    logInfo "Failed to fetch from Redis cache"
                      $ object ["error" .= show e]
                    fetchFromAmazon amazonEnv

    fetchFromRedis
      :: AmazonS3Env
      -> R.Connection
      -> Redis.RedisKey
      -> FileStorageT m (FileOrigin, BSL.ByteString)
    fetchFromRedis amazonEnv conn key =
      ((RedisOrigin, ) <$> Redis.getFileFromRedis conn key) `catch` \e -> do
        logInfo "Failed to fetch from Redis cache"
          $ object ["error" .= show (e :: SomeException)]
        fetchFromAmazon amazonEnv

    fetchFromAmazon :: AmazonS3Env -> FileStorageT m (FileOrigin, BSL.ByteString)
    fetchFromAmazon amazonEnv = do
      ((AmazonOrigin, ) <$> getContentsFromAmazon amazonEnv url) `catch` \e -> do
        case fromException e of
          Just e' -> throwM (e' :: FileStorageException)
          Nothing -> throwM . FileStorageException $ showt e

deleteSavedContents_
  :: (MonadBaseControl IO m, MonadLog m, MonadMask m, MonadThrow m)
  => Text
  -> FileStorageT m ()
deleteSavedContents_ url = do
  (amazonEnv, mRedisCache, memCache) <- getFileStorageConfig

  MemCache.invalidate memCache url
  whenJust mRedisCache $ \conn -> Redis.deleteKey conn $ Redis.redisKeyFromURL url
  deleteContentsFromAmazon amazonEnv url
