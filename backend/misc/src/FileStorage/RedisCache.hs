module FileStorage.RedisCache
  ( RedisCacheT
  , runRedisCacheT
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Either (isRight)
import Log
import System.Timeout.Lifted
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Database.Redis as R

import Database.Redis.Cache
import Database.Redis.Helpers
import DB
import FileStorage.Class

newtype RedisCacheT m a
  = RedisCacheT { unRedisCacheT :: ReaderT R.Connection m a }
  deriving ( Alternative, Applicative, Functor, Monad, MonadDB, MonadIO
           , MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b
           , MonadBaseControl b, MonadThrow, MonadCatch, MonadMask )

runRedisCacheT :: R.Connection -> RedisCacheT m a -> m a
runRedisCacheT config = flip runReaderT config . unRedisCacheT

getRedisConnection :: Monad m => RedisCacheT m R.Connection
getRedisConnection = RedisCacheT ask

instance {-# OVERLAPPING #-} ( MonadFileStorage m, MonadBaseControl IO m
                             , MonadLog m, MonadMask m )
    => MonadFileStorage (RedisCacheT m) where
  saveNewFile     = saveNewFileWithRedis
  getFileContents = getFileContentsWithRedis
  deleteFile      = deleteFileWithRedis

saveNewFileWithRedis :: (MonadFileStorage m, MonadBase IO m)
                     => String -> BS.ByteString
                     -> RedisCacheT m (Either String ())
saveNewFileWithRedis url contents = do
  eRes <- lift $ saveNewFile url contents
  when (isRight eRes) $ do
    conn <- getRedisConnection
    redisPut "contents" contents (conn, redisKeyFromURL url)
  return eRes

getFileContentsWithRedis
  :: (MonadFileStorage m, MonadBaseControl IO m, MonadLog m, MonadMask m)
  => String -> RedisCacheT m (Either String BS.ByteString)
getFileContentsWithRedis url = do
  conn <- getRedisConnection
  let key = redisKeyFromURL url
  mfetch (Just conn) key
         (\_ _ -> Right `liftM` getFileFromRedis conn key)
         (\_ -> lift $ getFileContents url)

deleteFileWithRedis :: (Monad m, MonadFileStorage m) => String
                    -> RedisCacheT m (Either String ())
deleteFileWithRedis url = do
  -- FIXME: delete from Redis
  lift $ deleteFile url

-- | Fetch the contents of a file from Redis retrying every second.
getFileFromRedis :: (MonadBaseControl IO m, MonadLog m)
                 => R.Connection -> RedisKey -> m BS.ByteString
getFileFromRedis conn rkey = do
  -- This is used to make Redis race with a timeout.
  semaphore <- newEmptyMVar
  withAsync (listener semaphore) $ \_ -> fix $ \loop -> do

    mContents <- runRedis conn $ R.hget key "contents"
    case mContents of
      Just contents -> do
        logInfo_ "File retrieved successfully"
        return contents
      Nothing -> do
        logInfo_ "Waiting for file"
        void . timeout 1000000 $ takeMVar semaphore
        loop

  where
    key :: BS.ByteString
    key = fromRedisKey rkey

    -- This interrupts the timeout every time we receive a message from Redis.
    listener :: (MonadBaseControl IO m, MonadLog m) => MVar () -> m ()
    listener semaphore = runRedis_ conn $ do
      R.pubSub (R.subscribe [key]) $ \_msg -> do
        void $ tryPutMVar semaphore ()
        return mempty

redisKeyFromURL :: String -> RedisKey
redisKeyFromURL url = mkRedisKey ["files", BSC.pack url]
