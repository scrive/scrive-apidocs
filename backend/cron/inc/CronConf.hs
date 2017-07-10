module CronConf (
      CronConf(..)
      , unjsonCronConf
  ) where

import Data.Default
import Data.Unjson
import qualified Data.Text as T

import Amazon.Config
import Database.Redis.Configuration
import EID.CGI.GRP.Config
import GuardTime (GuardTimeConf(..))
import HostClock.System (defaultNtpServers)
import KontraPrelude
import Log.Configuration
import Salesforce.Conf
import SFTPConfig

-- | Cron configuration: things like AWS, Postgres and Redis, NTP servers, etc.
data CronConf = CronConf {
    amazonConfig       :: Maybe AmazonConfig
    -- ^ AWS configuration (bucket, access key, secret key).
  , dbConfig           :: T.Text               -- ^ Postgresql configuration.
  , maxDBConnections   :: Int                  -- ^ Limit of db connections.
  , redisCacheConfig   :: Maybe RedisConfig    -- ^ Redis configuration.
  , localFileCacheSize :: Int                  -- ^ Size of local cache for files.
  , logConfig          :: LogConfig            -- ^ Logging configuration.
  , guardTimeConf      :: GuardTimeConf        -- ^ GuardTime configuration.
  , cgiGrpConfig       :: Maybe CgiGrpConfig   -- ^ CGI GRP (E-ID) configuration.
  , mixpanelToken      :: Maybe String         -- ^ For Mixpanel integration.
  , ntpServers         :: [String]
    -- ^ List of NTP servers to contact to get an estimate of host clock
    -- error.
  , salesforceConf     :: Maybe SalesforceConf -- ^ Salesforce configuration.
  , invoicingSFTPConf  :: Maybe SFTPConfig
    -- ^ SFTP server for invoicing uploads.
  } deriving (Eq, Show)

unjsonCronConf :: UnjsonDef CronConf
unjsonCronConf = objectOf $ pure CronConf
 <*> fieldOptBy "amazon"
      amazonConfig
      "Amazon configuration"
      (objectOf $ pure (,,)
       <*> field "bucket"
         (\(x,_,_) -> x)
         "In which bucket stored files exist"
       <*> field "access_key"
         (\(_,x,_) -> x)
         "Amazon access key"
       <*> field "secret_key"
         (\(_,_,x) -> x)
         "Amazon secret key")
  <*> field "database"
      dbConfig
      "Database connection string"
  <*> field "max_db_connections"
      maxDBConnections
      "Database connections limit"
  <*> fieldOpt "redis_cache"
      redisCacheConfig
      "Redis cache configuration"
  <*> field "local_file_cache_size"
      localFileCacheSize
      "Local file cache size in bytes"
  <*> field "logging"
      logConfig
      "Logging configuration"
  <*> field "guardtime"
      guardTimeConf
      "GuardTime configuration"
  <*> fieldOpt "cgi_grp"
      cgiGrpConfig
      "CGI GRP (E-ID) configuration"
  <*> fieldOpt "mixpanel"
      mixpanelToken
      "Token for Mixpanel"
  <*> field "ntp_servers"
      ntpServers
      "List of NTP servers to contact to get estimate of host clock error"
  <*> fieldOpt "salesforce"
      salesforceConf
      "Configuration of salesforce"
  <*> fieldOpt "invoicing_sftp_for_salesforce"
      invoicingSFTPConf
      "Configuration for SFTP:ing invoicing reports"

instance Unjson CronConf where
  unjsonDef = unjsonCronConf

-- | Default cron configuration that does nothing.
instance Default CronConf where
  def = CronConf {
      amazonConfig       = Nothing
    , dbConfig           = "user='kontra' password='kontra' dbname='kontrakcja'"
    , maxDBConnections   = 100
    , redisCacheConfig   = Nothing
    , localFileCacheSize = 200000000
    , logConfig          = def
    , guardTimeConf      = def
    , cgiGrpConfig       = Nothing
    , mixpanelToken      = Nothing
    , ntpServers         = defaultNtpServers
    , salesforceConf     = Nothing
    , invoicingSFTPConf  = Nothing
    }
