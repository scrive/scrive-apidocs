module TestFileStorage
  ( module FileStorage.Class
  , TestFileStorageT
  , evalTestFileStorageT
  , runTestFileStorageT
  , liftTestFileStorageT
  , getTestFSEnv
  ) where

import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Log
import Prelude hiding (get)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM

import DB
import FileStorage.Amazon
import FileStorage.Amazon.Config
import FileStorage.Class

newtype TestFileStorageT m a = TestFileStorageT
  { unTestFileStorageT :: ReaderT (Either (TVar FakeFS) AmazonConfig) m a }
  deriving ( Alternative, Applicative, Functor, Monad, MonadDB, MonadIO
           , MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b
           , MonadBaseControl b, MonadThrow, MonadCatch, MonadMask
           , MonadTransControl )

evalTestFileStorageT :: MonadIO m => Maybe AmazonConfig -> TestFileStorageT m a
                     -> m a
evalTestFileStorageT mConfig action = do
  env <- case mConfig of
    Nothing -> fmap Left . liftIO $ newTVarIO HM.empty
    Just conf -> return $ Right conf
  runTestFileStorageT action env

runTestFileStorageT :: MonadIO m => TestFileStorageT m a
                    -> Either (TVar FakeFS) AmazonConfig -> m a
runTestFileStorageT = runReaderT . unTestFileStorageT

liftTestFileStorageT :: Monad m => (Either (TVar FakeFS) AmazonConfig -> m a)
                     -> TestFileStorageT m a
liftTestFileStorageT = TestFileStorageT . ReaderT

getTestFSEnv :: Monad m
             => TestFileStorageT m (Either (TVar FakeFS) AmazonConfig)
getTestFSEnv = TestFileStorageT ask

type FakeFS = HM.HashMap String BSL.ByteString

instance (MonadBase IO m, MonadLog m, MonadThrow m)
    => MonadFileStorage (TestFileStorageT m) where
  saveNewContents url contents = TestFileStorageT $ ReaderT $ \case
    Left tvar ->
      liftBase $ atomically $ modifyTVar' tvar $ HM.insert url contents
    Right conf -> runAmazonMonadT conf $ saveNewContents url contents

  getSavedContents url = TestFileStorageT $ ReaderT $ \case
    Left tvar -> do
      fs <- liftBase $ readTVarIO tvar
      case HM.lookup url fs of
        Just contents -> return contents
        Nothing ->
          throwM $ FileStorageException $ "object " ++ url ++ " not found"
    Right conf -> runAmazonMonadT conf $ getSavedContents url

  deleteSavedContents url = TestFileStorageT $ ReaderT $ \case
    Left tvar -> liftBase $ atomically $ modifyTVar' tvar $ HM.delete url
    Right conf -> runAmazonMonadT conf $ deleteSavedContents url
