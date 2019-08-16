module AppDBConfig where

import Data.Unjson

import Log.Configuration

-- | Subset of 'AppConf' needed for running database migrations.
data AppDBConf = AppDBConf {
    dbConfig  :: Text    -- ^ postgresql configuration
  , logConfig :: LogConfig -- ^ logging configuration
  } deriving (Eq, Show)

instance Unjson AppDBConf where
  unjsonDef = objectOf $ AppDBConf
    <$> field "database"
        dbConfig
        "Database connection string"
    <*> field "logging"
        logConfig
        "Logging configuration"
