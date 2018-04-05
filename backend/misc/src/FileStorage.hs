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

instance (MonadBase IO m, MonadBaseControl IO m, MonadLog m, MonadMask m)
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
