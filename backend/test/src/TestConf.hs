module TestConf (
    TestConf(..)
  , unjsonTestConf
  ) where

import Data.Unjson

import Database.Redis.Configuration
import FileStorage.Amazon.Config
import MonthlyInvoice.Config (MonthlyInvoiceConf(..))
import PdfToolsLambda.Conf

-- | Main application configuration.  This includes amongst other
-- things the http port number, AWS, GuardTime, E-ID and email
-- configuraton, as well as a handy boolean indicating whether this is
-- a production or development instance.
data TestConf = TestConf
  { testDBConfig               :: Text               -- ^ test postgresql configuration
  , testPdfToolsLambdaConf     :: PdfToolsLambdaConf   -- ^ pdf tools lambda configuration for tests
  , testAmazonConfig           :: Maybe AmazonConfig   -- ^ Optional configuration for S3
  , testLocalFileCacheSize     :: Maybe Int            -- ^ Optional size for the local file cache
  , testRedisCacheConfig       :: Maybe RedisConfig    -- ^ Optional configuration for the Redis file cache
  , testMonthlyInvoiceConf     :: Maybe MonthlyInvoiceConf
  , testFlowPort               :: Int
  } deriving (Eq, Show)

unjsonTestConf :: UnjsonDef TestConf
unjsonTestConf =
  objectOf
    $   TestConf
    <$> field "database"        testDBConfig           "Database connection string"
    <*> field "pdftools_lambda" testPdfToolsLambdaConf "Configuration of PdfTools Lambda"
    <*> fieldOpt "amazon" testAmazonConfig "Optional configuration for S3"
    <*> fieldOpt "local_file_cache_size"
                 testLocalFileCacheSize
                 "Optional size for the local file cache"
    <*> fieldOpt "redis_cache"
                 testRedisCacheConfig
                 "Optional configuration for the Redis file cache"
    <*> fieldOpt "cron_monthly_invoice"
                 testMonthlyInvoiceConf
                 "Monthly-invoice cron job configuration"
    -- TODO: This port is different from the one in AppConf because flow is
    -- started as part of test as well. Thus we are preventing port clash.
    <*> fieldDef "flow_port" 9174 testFlowPort "Flow listening port"

instance Unjson TestConf where
  unjsonDef = unjsonTestConf
