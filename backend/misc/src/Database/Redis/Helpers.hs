module Database.Redis.Helpers (
    RedisKey
  , mkRedisKey
  , fromRedisKey
  , runRedis
  , runRedis'
  , fromRedisResp
  , checkRedisConnection
  , redisPut
  ) where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Typeable
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Database.Redis as R

import Utils.Exception

newtype RedisKey = RedisKey BS.ByteString

mkRedisKey :: [BS.ByteString] -> RedisKey
mkRedisKey = RedisKey . BS.intercalate ":"

fromRedisKey :: RedisKey -> BS.ByteString
fromRedisKey (RedisKey rk) = rk

----------------------------------------

data UnknownRedisReply = UnknownRedisReply R.Reply
  deriving (Eq, Show, Typeable)

instance Exception UnknownRedisReply

runRedis :: MonadBase IO m => R.Connection -> R.Redis (Either R.Reply a) -> m a
runRedis conn re = runRedis' conn (re >>= fromRedisResp)

runRedis' :: MonadBase IO m => R.Connection -> R.Redis a -> m a
runRedis' conn r = liftBase . R.runRedis conn $ r

fromRedisResp :: Monad m => Either R.Reply a -> m a
fromRedisResp e = either (throw . UnknownRedisReply) (return) e

checkRedisConnection :: MonadBaseControl IO m => R.Connection -> m ()
checkRedisConnection conn = do
  ereply <- tryAny $ runRedis conn R.ping
  case ereply :: Either SomeException R.Status of
    Right R.Pong -> return ()
    Right status -> unexpectedError $ "expected Pong, got" <+> (showt status)
    Left err     -> unexpectedError $ T.concat [
        "couldn't execute command 'ping':"
      , showt err
      , "(make sure configuration is correct and Redis server is running)"
      ]

----------------------------------------

-- | Put value into a specific hash field of a key and publish an empty message
-- under channel named as the key signifying that the key was updated.
redisPut :: MonadBase IO m => BS.ByteString -> BS.ByteString
         -> (R.Connection, RedisKey) -> m ()
redisPut field value (cache, rkey) = runRedis' cache $ do
  let key = fromRedisKey rkey
  void $ R.hset key field value
  void $ R.publish key ""
