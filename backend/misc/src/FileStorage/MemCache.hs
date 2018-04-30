{- |
 - This modules allows to add a cache in memory to a monad instantiating
 - 'MonadFileStorage'.
 -}

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
import qualified Data.ByteString.Lazy as BSL

import DB
import FileStorage.Class
import MemCache

type FileMemCache = MemCache String BSL.ByteString

newFileMemCache :: MonadBase IO m => Int -> m FileMemCache
newFileMemCache = new (fromInteger . toInteger . BSL.length)

-- | Transform a monad instantiating 'MonadFileStorage' by adding a memory cache
-- in front of it.
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
                        => String -> BSL.ByteString -> MemCacheT m ()
saveNewFileWithMemCache url contents = do
  cache <- MemCacheT ask
  invalidate cache url
  lift $ saveNewFile url contents

getFileContentsWithMemCache :: (MonadBaseControl IO m, MonadFileStorage m)
                            => String -> MemCacheT m BSL.ByteString
getFileContentsWithMemCache url = do
  cache <- MemCacheT ask
  fetch_ cache url $ lift $ getFileContents url

deleteFileWithMemCache :: (MonadBaseControl IO m, MonadFileStorage m)
                       => String -> MemCacheT m ()
deleteFileWithMemCache url = do
  cache <- MemCacheT ask
  invalidate cache url
  lift $ deleteFile url
