module CronConf (
        CronConf(..)
      , unjsonCronConf
  ) where

import Data.Unjson

import Database.Redis.Configuration
import EID.CGI.GRP.Config
import EID.EIDService.Conf
import EID.Nets.Config
import EventStream.Kinesis
import FileStorage.Amazon.Config
import GuardTime (GuardTimeConf(..))
import Log.Configuration
import Monitoring
import MonthlyInvoice.Config
import PdfToolsLambda.Conf
import Planhat
import Salesforce.Conf
import StatsD.Config

-- | Cron configuration: things like AWS, Postgres and Redis, NTP servers, etc.
data CronConf = CronConf
  { cronAmazonConfig               :: AmazonConfig -- ^ AWS configuration
                                                   -- (bucket, access key,
                                                   -- secret key) (lazy as the
                                                   -- field is undefined in
                                                   -- tests)
  , cronDBConfig                   :: !Text               -- ^ Postgresql configuration.
  , cronMaxDBConnections           :: !Int                  -- ^ Limit of db connections.
  , cronRedisCacheConfig           :: !(Maybe RedisConfig)  -- ^ Redis configuration.
  , cronLocalFileCacheSize         :: !Int                  -- ^ Size of local cache for files.
  , cronLogConfig                  :: !LogConfig            -- ^ Logging configuration.
  , cronGuardTimeConf              :: !GuardTimeConf        -- ^ GuardTime configuration.
  , cronCgiGrpConfig               :: !(Maybe CgiGrpConfig) -- ^ CGI GRP (E-ID) configuration.
  , cronMixpanelToken              :: !(Maybe Text)       -- ^ For Mixpanel integration.
  , cronNtpServers                 :: ![Text]
    -- ^ List of NTP servers to contact to get an estimate of host clock
    -- error.
  , cronSalesforceConf             :: !(Maybe SalesforceConf) -- ^ Salesforce configuration.
  , cronPlanhatConf                :: !(Maybe PlanhatConf)
    -- ^ To enable data push to Planhat
  , cronMonitoringConf             :: !(Maybe MonitoringConf)
  , cronMailNoreplyAddress         :: Text               -- ^ Noreply address used when sending email
    -- ^ Configure number of running jobs for individual Consumers
  , cronConsumerCronMaxJobs        :: !Int
  , cronConsumerSealingMaxJobs     :: !Int
  , cronConsumerSigningMaxJobs     :: !Int
  , cronConsumerExtendingMaxJobs   :: !Int
  , cronConsumerAPICallbackMaxJobs :: !Int
  , cronConsumerFilePurgingMaxJobs :: !Int
  , cronNetsSignConfig             :: !(Maybe NetsSignConfig)
  , cronPdfToolsLambdaConf         :: PdfToolsLambdaConf -- ^ AWS configuration
                                                         -- for lambda (lazy as
                                                         -- the field is
                                                         -- undefined in tests)
  , cronMonthlyInvoiceConf         :: !(Maybe MonthlyInvoiceConf)
  , cronStatsDConf                 :: !(Maybe StatsDConf)
  , cronEIDServiceConf             :: Maybe EIDServiceConf
  , cronKinesisStream             :: Maybe KinesisConf
  } deriving (Eq, Show)

unjsonCronConf :: UnjsonDef CronConf
unjsonCronConf =
  objectOf
    $   CronConf
    <$> field "amazon"             cronAmazonConfig     "Amazon configuration"
    <*> field "database"           cronDBConfig         "Database connection string"
    <*> field "max_db_connections" cronMaxDBConnections "Database connections limit"
    <*> fieldOpt "redis_cache" cronRedisCacheConfig "Redis cache configuration"
    <*> field "local_file_cache_size"
              cronLocalFileCacheSize
              "Local file cache size in bytes"
    <*> field "logging"   cronLogConfig     "Logging configuration"
    <*> field "guardtime" cronGuardTimeConf "GuardTime configuration"
    <*> fieldOpt "cgi_grp"  cronCgiGrpConfig  "CGI GRP (E-ID) configuration"
    <*> fieldOpt "mixpanel" cronMixpanelToken "Token for Mixpanel"
    <*> field "ntp_servers"
              cronNtpServers
              "List of NTP servers to contact to get estimate of host clock error"
    <*> fieldOpt "salesforce" cronSalesforceConf "Configuration of salesforce"
    <*> fieldOpt "planhat" cronPlanhatConf "Configuration for pushing data to Planhat"
    <*> fieldOpt "monitoring"
                 cronMonitoringConf
                 "Configuration of the ekg-statsd-based monitoring."
    <*> field "mail_noreply_address"
              cronMailNoreplyAddress
              "Noreply address used when sending email"
    <*> field "consumer_cron_max_jobs"
              cronConsumerCronMaxJobs
              "Maximum number of jobs running cron itself"
    <*> field "consumer_sealing_max_jobs"
              cronConsumerSealingMaxJobs
              "Maximum number of jobs running the Document Sealing Consumer"
    <*> field "consumer_signing_max_jobs"
              cronConsumerSigningMaxJobs
              "Maximum number of jobs running the Document Signing Consumer"
    <*> field "consumer_extending_max_jobs"
              cronConsumerExtendingMaxJobs
              "Maximum number of jobs running the Document Extending Consumer"
    <*> field "consumer_api_callback_max_jobs"
              cronConsumerAPICallbackMaxJobs
              "Maximum number of jobs running the Document API Callback Consumer"
    <*> field "consumer_file_purging_max_jobs"
              cronConsumerFilePurgingMaxJobs
              "Maximum number of jobs running the File Purging Consumer"
    <*> fieldOpt "nets_sign" cronNetsSignConfig "Configuration of Nets for ESigning"
    <*> field "pdftools_lambda" cronPdfToolsLambdaConf "Configuration of PdfTools Lambda"
    <*> fieldOpt "monthly_invoice"
                 cronMonthlyInvoiceConf
                 "Monthly-invoice job configuration"
    <*> fieldOpt "statsd"      cronStatsDConf     "StatsD configuration"
    <*> fieldOpt "eid_service" cronEIDServiceConf "Configuration of eid service"
    <*> fieldOpt "kinesis_stream"
                 cronKinesisStream
                 "Configuration of kinesis message stream"

instance Unjson CronConf where
  unjsonDef = unjsonCronConf
