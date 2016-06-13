module AppDBConfig where

import Data.Default
import Data.Unjson
import qualified Data.Text as T

import KontraPrelude
import Log.Configuration

-- | Subset of 'AppConf' needed for running database migrations.
data AppDBConf = AppDBConf {
    dbConfig  :: T.Text    -- ^ postgresql configuration
  , logConfig :: LogConfig -- ^ logging configuration
  } deriving (Eq, Show)

instance Unjson AppDBConf where
  unjsonDef = objectOf $ AppDBConf
    <$> fieldBy "database"
        dbConfig
        "Database connection string"
        unjsonAeson
    <*> field "logging"
        logConfig
        "Logging configuration"

instance Default AppDBConf where
  def = AppDBConf {
    dbConfig  = "user='kontra' password='kontra' dbname='kontrakcja'"
  , logConfig = def
  }
