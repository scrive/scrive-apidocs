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
import Monitoring
import Planhat
import Salesforce.Conf
import SFTPConfig

-- | Cron configuration: things like AWS, Postgres and Redis, NTP servers, etc.
data CronConf = CronConf {
    cronAmazonConfig       :: !(Maybe AmazonConfig)
    -- ^ AWS configuration (bucket, access key, secret key).
  , cronDBConfig           :: !T.Text               -- ^ Postgresql configuration.
  , cronMaxDBConnections   :: !Int                  -- ^ Limit of db connections.
  , cronRedisCacheConfig   :: !(Maybe RedisConfig)  -- ^ Redis configuration.
  , cronLocalFileCacheSize :: !Int                  -- ^ Size of local cache for files.
  , cronLogConfig          :: !LogConfig            -- ^ Logging configuration.
  , cronGuardTimeConf      :: !GuardTimeConf        -- ^ GuardTime configuration.
  , cronCgiGrpConfig       :: !(Maybe CgiGrpConfig) -- ^ CGI GRP (E-ID) configuration.
  , cronMixpanelToken      :: !(Maybe String)       -- ^ For Mixpanel integration.
  , cronNtpServers         :: ![String]
    -- ^ List of NTP servers to contact to get an estimate of host clock
    -- error.
  , cronSalesforceConf     :: !(Maybe SalesforceConf) -- ^ Salesforce configuration.
  , cronInvoicingSFTPConf  :: !(Maybe SFTPConfig)
    -- ^ SFTP server for invoicing uploads.
  , cronPlanhatConf        :: !(Maybe PlanhatConf)
    -- ^ To enable data push to Planhat
  , cronMonitoringConf     :: !(Maybe MonitoringConf)
  , cronMailNoreplyAddress :: String               -- ^ Noreply address used when sending email
  } deriving (Eq, Show)

unjsonCronConf :: UnjsonDef CronConf
unjsonCronConf = objectOf $ pure CronConf
 <*> fieldOptBy "amazon"
      cronAmazonConfig
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
      cronDBConfig
      "Database connection string"
  <*> field "max_db_connections"
      cronMaxDBConnections
      "Database connections limit"
  <*> fieldOpt "redis_cache"
      cronRedisCacheConfig
      "Redis cache configuration"
  <*> field "local_file_cache_size"
      cronLocalFileCacheSize
      "Local file cache size in bytes"
  <*> field "logging"
      cronLogConfig
      "Logging configuration"
  <*> field "guardtime"
      cronGuardTimeConf
      "GuardTime configuration"
  <*> fieldOpt "cgi_grp"
      cronCgiGrpConfig
      "CGI GRP (E-ID) configuration"
  <*> fieldOpt "mixpanel"
      cronMixpanelToken
      "Token for Mixpanel"
  <*> field "ntp_servers"
      cronNtpServers
      "List of NTP servers to contact to get estimate of host clock error"
  <*> fieldOpt "salesforce"
      cronSalesforceConf
      "Configuration of salesforce"
  <*> fieldOpt "invoicing_sftp_for_salesforce"
      cronInvoicingSFTPConf
      "Configuration for SFTP:ing invoicing reports"
  <*> fieldOpt "planhat"
      cronPlanhatConf
      "Configuration for pushing data to Planhat"
  <*> fieldOpt "monitoring"
      cronMonitoringConf
      "Configuration of the ekg-statsd-based monitoring."
  <*> field "mail_noreply_address"
      cronMailNoreplyAddress
      "Noreply address used when sending email"

instance Unjson CronConf where
  unjsonDef = unjsonCronConf

-- | Default cron configuration that does nothing.
instance Default CronConf where
  def = CronConf {
      cronAmazonConfig       = Nothing
    , cronDBConfig           =
        "user='kontra' password='kontra' dbname='kontrakcja'"
    , cronMaxDBConnections   = 100
    , cronRedisCacheConfig   = Nothing
    , cronLocalFileCacheSize = 200000000
    , cronLogConfig          = def
    , cronGuardTimeConf      = def
    , cronCgiGrpConfig       = Nothing
    , cronMixpanelToken      = Nothing
    , cronNtpServers         = defaultNtpServers
    , cronSalesforceConf     = Nothing
    , cronInvoicingSFTPConf  = Nothing
    , cronPlanhatConf        = Nothing
    , cronMonitoringConf     = Nothing
    , cronMailNoreplyAddress = "noreply@scrive.com"
    }
