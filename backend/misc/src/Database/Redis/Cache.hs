module Database.Redis.Cache
  ( mfetch
  , deleteKey
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Function (fix)
import Log
import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as R

import Database.Redis.Helpers
import Log.Utils
import Utils.Exception

data KeyInvalidated = NotExists | TTLNotSet
  deriving (Eq, Show)

-- | Given a 'RedisKey', try to fetch it from the cache and run the
-- corresponding handler afterwards.
mfetch
  :: forall m r
   . (MonadBaseControl IO m, MonadLog m, MonadMask m)
  => Maybe R.Connection                       -- ^ Redis connection
  -> RedisKey                                 -- ^ Redis key
  -> (R.Connection -> RedisKey -> m r)        -- ^ What to do if key is in cache
  -> (Maybe (R.Connection, RedisKey) -> m r)  -- ^ What to do if there is no
                                 -- Redis connection or key is not in cache
  -> m r
mfetch mredis rkey actGet construct = case mredis of
  Nothing    -> construct Nothing
  Just cache -> tryAny (fetch cache) >>= \case
    -- In case fetching/generating values with Redis fails, retry
    -- without it. If a key is left in partial or invalid state, it
    -- will be eventually cleaned up by one of the instances.
    Left ex -> do
      logAttention "Error when using Redis, trying cacheless version"
        $ object ["error" .= show ex]
      construct Nothing
    Right (Left err) -> do
      logAttention "Key invalidated while its value was being fetched"
        $ object ["reason" .= show err]
      mfetch mredis rkey actGet construct
    Right (Right res) -> return res
  where
    fetch :: R.Connection -> m (Either KeyInvalidated r)
    fetch cache = mask $ \release -> do
      keySet <- runRedis cache $ R.hsetnx key "__set" ""
      if keySet
        then (`onException` deleteKey cache rkey) . release $ do
          logInfo "Key not found in global cache" $ object ["key" `equalsExternalBS` key]
          -- Create a thread that continually prolongs key's TTL by a few
          -- seconds as long as the value is being generated so that if an
          -- instance unexpectedly terminates, other instances can quickly
          -- detect the accident and clean up. After we're successful, we set
          -- key's TTL to 24 hours or remove it from cache if any error has
          -- occured in between.
          res <- withAsync ttlRefresher $ \_ -> construct $ Just (cache, rkey)
          void . runRedis cache $ R.expire key oneDay
          return $ Right res
        else release $ do
          logInfo "Key found in global cache" $ object ["key" `equalsExternalBS` key]
          -- While fetching value(s) associated with the key, monitor key's
          -- TTL. If key gets deleted or TTL is not set (it may happen if the
          -- appropriate instance dies between setting the key with setnx and
          -- setting its TTL) it means that either it was removed from cache or
          -- instance responsible for its generation unexpectedly terminated and
          -- we need to restart the whole process.
          eres <- withAsync ttlMonitor $ \monitor -> do
            withAsync (actGet cache rkey) $ \getter -> do
              waitEither monitor getter

          case eres of
            -- If fetch was successful and remaining TTL is shorter than half a
            -- day, update it to its original value.
            Right _ -> runRedis' cache $ do
              ttl <- fromRedisResp <=< R.ttl $ key
              when (ttl < oneDay `div` 2) $ do
                void $ R.expire key oneDay
            -- If key with no TTL set exists, delete it as it's invalid.
            Left TTLNotSet -> deleteKey cache rkey
            Left NotExists -> return ()

          return eres
      where
        ttlRefresher :: m ()
        ttlRefresher = forever $ do
          void . runRedis cache $ R.expire key 10
          threadDelay $ 5 * 1000000

        ttlMonitor :: m KeyInvalidated
        ttlMonitor = fix $ \loop -> do
          -- Wait first to possibly allow the other instance to set TTL.
          threadDelay $ 5 * 1000000
          runRedis cache (R.ttl key) >>= \case
            -2 -> return NotExists
            -1 -> return TTLNotSet
            _  -> loop

        key :: BS.ByteString
        key = fromRedisKey rkey

        oneDay :: Integer
        oneDay = 86400

deleteKey
  :: (MonadBaseControl IO m, MonadLog m, MonadMask m) => R.Connection -> RedisKey -> m ()
deleteKey conn = void . runRedis conn . R.del . pure . fromRedisKey
