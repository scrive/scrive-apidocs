module FileStorage.MemCache
  ( MemCacheT
  , runMemCacheT
  , FileMemCache
  , newFileMemCache
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Log
import qualified Data.ByteString as BS

import DB
import FileStorage.Class
import MemCache

type FileMemCache = MemCache String BS.ByteString

newFileMemCache :: MonadBase IO m => Int -> m FileMemCache
newFileMemCache = new BS.length

newtype MemCacheT m a
  = MemCacheT { unMemCacheT :: ReaderT FileMemCache m a }
  deriving ( Alternative, Applicative, Functor, Monad, MonadDB, MonadIO
           , MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b
           , MonadBaseControl b, MonadThrow, MonadCatch, MonadMask )

runMemCacheT :: FileMemCache -> MemCacheT m a -> m a
runMemCacheT cache = flip runReaderT cache . unMemCacheT

instance {-# OVERLAPPING #-} (MonadBaseControl IO m, MonadFileStorage m)
    => MonadFileStorage (MemCacheT m) where
  saveNewFile     = saveNewFileWithMemCache
  getFileContents = getFileContentsWithMemCache
  deleteFile      = deleteFileWithMemCache

saveNewFileWithMemCache :: (MonadBaseControl IO m, MonadFileStorage m)
                        => String -> BS.ByteString -> MemCacheT m ()
saveNewFileWithMemCache url contents = do
  cache <- MemCacheT ask
  invalidate cache url
  lift $ saveNewFile url contents

getFileContentsWithMemCache :: (MonadBaseControl IO m, MonadFileStorage m)
                            => String -> MemCacheT m BS.ByteString
getFileContentsWithMemCache url = do
  cache <- MemCacheT ask
  fetch_ cache url $ lift $ getFileContents url

deleteFileWithMemCache :: (MonadBaseControl IO m, MonadFileStorage m)
                       => String -> MemCacheT m ()
deleteFileWithMemCache url = do
  cache <- MemCacheT ask
  invalidate cache url
  lift $ deleteFile url
