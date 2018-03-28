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
import FileStorage.RedisCache

type FileStorageConfig = (Maybe AmazonConfig, Maybe R.Connection)

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
                  => (forall n. MonadFileStorage n => n (Either String a))
                  -> FileStorageT m (Either String a)
inAvailableLayers action = do
  config <- getFileStorageConfig
  case config of
    (Nothing, _) -> return $ Left "no Amazon config"
    (Just amazonConfig, Nothing) ->
      runAmazonMonadT amazonConfig action
    (Just amazonConfig, Just conn) ->
      runAmazonMonadT amazonConfig $ runRedisCacheT conn action
