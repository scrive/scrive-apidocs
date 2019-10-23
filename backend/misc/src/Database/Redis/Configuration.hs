{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Database.Redis.Configuration (
    RedisConfig(..)
  , mkRedisConnection
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Unjson
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Redis as R

import Database.Redis.Helpers

data RedisConfig = RedisConfig
  { rcHost     :: !Text
  , rcPort     :: !Word16
  , rcAuth     :: !(Maybe Text)
  , rcDatabase :: !Integer

  } deriving (Eq, Ord, Show)
instance Unjson RedisConfig where
  unjsonDef =
    objectOf
      $   RedisConfig
      <$> field "host" rcHost "Host"
      <*> field "port" rcPort "Port"
      <*> fieldOpt "password" rcAuth "Password"
      <*> field "database" rcDatabase "Database number"

mkRedisConnection :: MonadBaseControl IO m => RedisConfig -> m R.Connection
mkRedisConnection rc = do
  cache <- liftBase . R.connect $ configToConnectInfo rc
  checkRedisConnection cache
  return cache
  where
    configToConnectInfo RedisConfig {..} = R.defaultConnectInfo
      { R.connectHost           = T.unpack rcHost
      , R.connectPort           = R.PortNumber $ fromIntegral rcPort
      , R.connectAuth           = T.encodeUtf8 <$> rcAuth
      , R.connectDatabase       = rcDatabase
      , R.connectMaxConnections = 100
      , R.connectMaxIdleTime    = 10
      }
