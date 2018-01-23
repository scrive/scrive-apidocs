module Database.Redis.Configuration (
    RedisConfig(..)
  , mkRedisConnection
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Default
import Data.Unjson
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Redis as R

import Database.Redis.Helpers

data RedisConfig = RedisConfig {
    rcHost     :: !T.Text
  , rcPort     :: !Word16
  , rcAuth     :: !(Maybe T.Text)
  , rcDatabase :: !Integer
  } deriving (Eq, Ord, Show)

instance Unjson RedisConfig where
  unjsonDef = objectOf $ RedisConfig
    <$> field "host"
        rcHost
        "Host"
    <*> field "port"
        rcPort
        "Port"
    <*> fieldOpt "password"
        rcAuth
        "Password"
    <*> field "database"
        rcDatabase
        "Database number"

instance Default RedisConfig where
  def = RedisConfig {
      rcHost     = "localhost"
    , rcPort     = 6379
    , rcAuth     = Nothing
    , rcDatabase = 0
    }

mkRedisConnection :: MonadBaseControl IO m => RedisConfig -> m R.Connection
mkRedisConnection rc = do
  cache <- liftBase . R.connect $ configToConnectInfo rc
  checkRedisConnection cache
  return cache
  where
    configToConnectInfo RedisConfig{..} = R.ConnInfo {
        connectHost           = T.unpack rcHost
      , connectPort           = R.PortNumber $ fromIntegral rcPort
      , connectAuth           = T.encodeUtf8 <$> rcAuth
      , connectDatabase       = rcDatabase
      , connectMaxConnections = 100
      , connectMaxIdleTime    = 10
      }
