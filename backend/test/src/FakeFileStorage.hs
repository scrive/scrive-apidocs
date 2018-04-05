module FakeFileStorage
  ( module FileStorage.Class
  , FakeFileStorageT
  , evalFakeFileStorageT
  , runFakeFileStorageT
  , liftFakeFileStorageT
  , getFakeFSTVar
  ) where

import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Log
import Prelude hiding (get)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

import DB
import FileStorage.Class

newtype FakeFileStorageT m a = FakeFileStorageT
  { unFakeFileStorageT :: ReaderT (TVar FakeFS) m a }
  deriving ( Alternative, Applicative, Functor, Monad, MonadDB, MonadIO
           , MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b
           , MonadBaseControl b, MonadThrow, MonadCatch, MonadMask
           , MonadTransControl )

evalFakeFileStorageT :: MonadIO m => FakeFileStorageT m a -> m a
evalFakeFileStorageT action = do
  stVar <- liftIO $ newTVarIO HM.empty
  runFakeFileStorageT action stVar

runFakeFileStorageT :: MonadIO m => FakeFileStorageT m a -> TVar FakeFS -> m a
runFakeFileStorageT action stVar = runReaderT (unFakeFileStorageT action) stVar

liftFakeFileStorageT :: Monad m => (TVar FakeFS -> m a) -> FakeFileStorageT m a
liftFakeFileStorageT = FakeFileStorageT . ReaderT

getFakeFSTVar :: Monad m => FakeFileStorageT m (TVar FakeFS)
getFakeFSTVar = FakeFileStorageT ask

type FakeFS = HM.HashMap String BS.ByteString

instance (MonadIO m, MonadThrow m)
    => MonadFileStorage (FakeFileStorageT m) where
  saveNewFile url contents = FakeFileStorageT $ ReaderT $ \tvar -> do
    liftIO $ atomically $ modifyTVar' tvar $ HM.insert url contents

  getFileContents url = FakeFileStorageT $ ReaderT $ \tvar -> do
    fs <- liftIO $ readTVarIO tvar
    case HM.lookup url fs of
      Just contents -> return contents
      Nothing ->
        throwM $ FileStorageException $ "object " ++ url ++ " not found"

  deleteFile url = FakeFileStorageT $ ReaderT $ \tvar -> do
    liftIO $ atomically $ modifyTVar' tvar $ HM.delete url
