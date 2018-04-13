{- |
 - This module provides a monad transformer to manage objects in S3 or Ceph
 - using caches if its configuration is given. The first cache is in memory and
 - the second in Redis. See the corresponding module for more details.
 -
 - It instantiates 'MonadFileStorage' which provides a common interface to store
 - files. Note that, here, a file is some text contents with a name.
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
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Log
import qualified Database.Redis as R

import DB
import FileStorage.Amazon
import FileStorage.Amazon.Config
import FileStorage.Class
import FileStorage.MemCache
import FileStorage.RedisCache

type FileStorageConfig =
  (Maybe AmazonConfig, Maybe R.Connection, Maybe FileMemCache)

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
  saveNewFile url contents = inAvailableLayers $ saveNewFile url contents
  getFileContents url      = inAvailableLayers $ getFileContents url
  deleteFile url           = inAvailableLayers $ deleteFile url

inAvailableLayers :: ( MonadBase IO m, MonadBaseControl IO m, MonadLog m
                     , MonadMask m )
                  => (forall n. MonadFileStorage n => n a) -> FileStorageT m a
inAvailableLayers action = do
  config <- getFileStorageConfig
  case config of
    (Nothing, _, _) -> throwM $ FileStorageException "no Amazon config"
    (Just amazonConfig, Nothing, Nothing) ->
      runAmazonMonadT amazonConfig action
    (Just amazonConfig, Just conn, Nothing) ->
      runAmazonMonadT amazonConfig $ runRedisCacheT conn action
    (Just amazonConfig, Nothing, Just memcache) ->
      runAmazonMonadT amazonConfig $ runMemCacheT memcache action
    (Just amazonConfig, Just conn, Just memcache) ->
      runAmazonMonadT amazonConfig $ runRedisCacheT conn $
        runMemCacheT memcache action
