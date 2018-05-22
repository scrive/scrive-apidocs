module FileStorage.Class where

import Control.Exception
import Control.Monad.Reader
import Data.Typeable
import qualified Data.ByteString.Lazy as BSL

data FileStorageException
  = FileStorageException String
  deriving (Eq, Ord, Show, Typeable)

instance Exception FileStorageException

-- | A monad in which we can store files.
--
-- Different layers of storage can be used and we stack them upon each other,
-- e.g. Memcache, Redis and S3.
--
-- Instances should use FileStorageException to throw exceptions.
--
-- For tests, we can fake this storage and store files in memory.
-- See FakeFileStorage.
class Monad m => MonadFileStorage m where
  saveNewFile :: String         -- ^ Object key (URL-encoded)
              -> BSL.ByteString -- ^ Contents (needs to be encrypted first)
              -> m ()

  getFileContents :: String           -- ^ Object key (URL-encoded)
                  -> m BSL.ByteString -- ^ File contents

  deleteFile :: String -- ^ Object key (URL-encoded)
             -> m ()

instance {-# OVERLAPS #-} (MonadFileStorage m, MonadTrans t, Monad (t m))
    => MonadFileStorage (t m) where
  saveNewFile url contents = lift $ saveNewFile url contents
  getFileContents          = lift . getFileContents
  deleteFile               = lift . deleteFile
