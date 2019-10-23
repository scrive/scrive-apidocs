module FileStorage.RedisCache
  ( getFileFromRedis
  , redisKeyFromURL
  , module Database.Redis.Cache
  , module Database.Redis.Helpers
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Log
import System.Timeout.Lifted
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Database.Redis as R

import Database.Redis.Cache
import Database.Redis.Helpers

-- | Fetch the contents of a file from Redis retrying every second.
getFileFromRedis
  :: (MonadBaseControl IO m, MonadLog m) => R.Connection -> RedisKey -> m BSL.ByteString
getFileFromRedis conn rkey = do
  -- This is used to make Redis race with a timeout.
  semaphore <- newEmptyMVar
  withAsync (listener semaphore) $ \_ -> fix $ \loop -> do

    mContents <- runRedis conn $ R.hget key "contents"
    case mContents of
      Just contents -> do
        logInfo_ "File retrieved successfully"
        return $ BSL.fromStrict contents
      Nothing -> do
        logInfo_ "Waiting for file"
        void . timeout 1000000 $ takeMVar semaphore
        loop

  where
    key :: BS.ByteString
    key = fromRedisKey rkey

    -- This interrupts the timeout every time we receive a message from Redis.
    listener :: (MonadBaseControl IO m, MonadLog m) => MVar () -> m ()
    listener semaphore = runRedis' conn $ do
      R.pubSub (R.subscribe [key]) $ \_msg -> do
        void $ tryPutMVar semaphore ()
        return mempty

redisKeyFromURL :: Text -> RedisKey
redisKeyFromURL url = mkRedisKey ["files", BSC.pack $ T.unpack url]
