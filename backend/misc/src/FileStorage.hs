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

import Control.Monad (void)
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

saveNewContents_ :: ( MonadBaseControl IO m, MonadCatch m, MonadLog m
                    , MonadMask m )
                 => String -> BSL.ByteString -> FileStorageT m ()
saveNewContents_ url contents = do
  (amazonConfig, mRedisCache, memCache) <- getFileStorageConfig

  MemCache.invalidate memCache url
  whenJust mRedisCache $ \conn -> do
    Redis.deleteKey conn $ Redis.redisKeyFromURL url

  runAmazonMonadT amazonConfig $ saveNewContents url contents

  void (MemCache.fetch memCache url (return contents)) `catch` \e ->
    logInfo "Failed to save in memory cache" $ object
      [ "error" .= show (e :: SomeException) ]

  whenJust mRedisCache $ \conn -> do
    Redis.redisPut "contents" (BSL.toStrict contents)
                   (conn, Redis.redisKeyFromURL url) `catch` \e ->
      logInfo "Failed to save to Redis cache" $ object
        [ "error" .= show (e :: SomeException) ]

getSavedContents_ :: forall m. ( MonadBaseControl IO m, MonadCatch m, MonadLog m
                               , MonadMask m, MonadThrow m )
                  => String -> FileStorageT m BSL.ByteString
getSavedContents_ url = do
    (amazonConfig, mRedisCache, memCache) <- getFileStorageConfig

    MemCache.fetch_ memCache url
      (fetchFromRedisOrAmazon mRedisCache amazonConfig) `catch` \e -> do
        case fromException e of
          -- It's coming from fetchFromRedisOrAmazon
          Just e' -> throwM (e' :: FileStorageException)
          -- It's coming from MemCache.fetch_
          Nothing -> do
            logInfo "Failed to fetch from memory cache" $ object
              [ "error" .= show e ]
            fetchFromRedisOrAmazon mRedisCache amazonConfig

  where
    fetchFromRedisOrAmazon :: Maybe R.Connection -> AmazonConfig
                           -> FileStorageT m BSL.ByteString
    fetchFromRedisOrAmazon mRedisCache amazonConfig =
      Redis.mfetch mRedisCache (Redis.redisKeyFromURL url)
        (fetchFromRedis amazonConfig)
        (\mConnKey -> do
          contents <- fetchFromAmazon amazonConfig
          whenJust mConnKey $ \connKey -> do
            Redis.redisPut "contents" (BSL.toStrict contents) connKey
              `catch` \e ->
                logInfo "Failed to save to Redis cache" $ object
                  [ "error" .= show (e :: SomeException) ]
          return contents)

        `catch` \e -> case fromException e of
          Just e' -> throwM (e' :: FileStorageException)
          Nothing -> do
            logInfo "Failed to fetch from Redis cache" $ object
              [ "error" .= show e ]
            fetchFromAmazon amazonConfig

    fetchFromRedis :: AmazonConfig -> R.Connection -> Redis.RedisKey
                   -> FileStorageT m BSL.ByteString
    fetchFromRedis amazonConfig conn key =
      Redis.getFileFromRedis conn key `catch` \e -> do
        logInfo "Failed to fetch from Redis cache" $ object
          [ "error" .= show (e :: SomeException) ]
        fetchFromAmazon amazonConfig

    fetchFromAmazon :: AmazonConfig -> FileStorageT m BSL.ByteString
    fetchFromAmazon amazonConfig = do
      runAmazonMonadT amazonConfig (getSavedContents url) `catch` \e -> do
        case fromException e of
          Just e' -> throwM (e' :: FileStorageException)
          Nothing -> throwM $ FileStorageException $ show e

deleteSavedContents_ :: ( MonadBaseControl IO m, MonadLog m, MonadMask m
                        , MonadThrow m )
                     => String -> FileStorageT m ()
deleteSavedContents_ url = do
  (amazonConfig, mRedisCache, memCache) <- getFileStorageConfig

  MemCache.invalidate memCache url
  whenJust mRedisCache $ \conn ->
    Redis.deleteKey conn $ Redis.redisKeyFromURL url
  runAmazonMonadT amazonConfig $ deleteSavedContents url
