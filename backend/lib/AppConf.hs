module AppConf (
    AppConf(..)
  , AmazonConfig
  , unjsonAppConf
  ) where

import Data.Default
import Data.Unjson
import Data.Word
import qualified Data.Text as T

import Amazon.Config
import Database.Redis.Configuration
import EID.CGI.GRP.Config
import EID.Nets.Config
import GuardTime (GuardTimeConf(..))
import HubSpot.Conf (HubSpotConf(..))
import KontraPrelude
import Log.Configuration
import Monitoring (MonitoringConf(..))
import Salesforce.Conf
import User.Email

-- | Main application configuration.  This includes amongst other
-- things the http port number, AWS, GuardTime, E-ID and email
-- configuraton, as well as a handy boolean indicating whether this is
-- a production or development instance.
data AppConf = AppConf {
    httpBindAddress    :: (Word32, Word16)
    -- ^ TCP address to bind to and port to listen on (0x7f000001,
    -- 8000) localhost:8000 (default) (0, 80) all interfaces port 80.
  , mainDomainUrl      :: String               -- ^ base url of the main domain
  , useHttps           :: Bool                 -- ^ should we redirect to https?
  , amazonConfig       :: Maybe AmazonConfig
  -- ^ AWS configuration (bucket, access key, secret key).
  , dbConfig           :: T.Text               -- ^ postgresql configuration
  , maxDBConnections   :: Int                  -- ^ limit of db connections
  , redisCacheConfig   :: Maybe RedisConfig    -- ^ redis configuration
  , localFileCacheSize :: Int                  -- ^ size of local cache for files
  , logConfig          :: LogConfig            -- ^ logging configuration
  , production         :: Bool
    -- ^ production flag, enables some production stuff, disables some
    -- development stuff
  , cdnBaseUrl         :: Maybe String         -- ^ for CDN content in prod mode
  , guardTimeConf      :: GuardTimeConf
  , isMailBackdoorOpen :: Bool
    -- ^ If true allows admins to access last mail send. Used by
    -- selenium.
  , mailNoreplyAddress   :: String             -- ^ Noreply address used when sending email
  , cgiGrpConfig       :: Maybe CgiGrpConfig   -- ^ CGI GRP (E-ID) configuration
  , admins             :: [Email]
    -- ^ E-mail addresses of people regarded as admins.
  , sales              :: [Email]
    -- ^ E-mail addresses of people regarded as sales admins.
  , initialUsers       :: [(Email,String)]
    -- ^ E-mail and passwords for initial users.
  , mixpanelToken      :: Maybe String         -- ^ For mixpanel integration.
  , trackjsToken       :: Maybe String         -- ^ For Track.js integration.
  , hubspotConf        :: Maybe HubSpotConf    -- ^ For Hubspot integration.
  , salesforceConf     :: Maybe SalesforceConf -- ^ Configuration of Salesforce.
  , netsConfig         :: Maybe NetsConfig
    -- ^ Configuration of Nets - .DK/.NO BankID provider.
  , monitoringConfig   :: Maybe MonitoringConf
    -- ^ Configuration of the ekg-statsd-based monitoring.
  } deriving (Eq, Show)

unjsonAppConf :: UnjsonDef AppConf
unjsonAppConf = objectOf $ pure AppConf
  <*> (pure (,)
         <*> fieldDefBy "bind_ip" 0
            (fst . httpBindAddress)
            "IP to listen on, defaults to 0.0.0.0"
            unjsonIPv4AsWord32
         <*> field "bind_port"
            (snd . httpBindAddress)
            "Port to listen on")
  <*> field "main_domain_url"
      mainDomainUrl
      "Base URL of the main domain"
  <*> fieldDef "https" True
      useHttps
      "Should use https"
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
  <*> fieldDef "production" False
      production
      "Is this production server"
  <*> fieldOpt "cdn_base_url"
      cdnBaseUrl
      "CDN base URL"
  <*> field "guardtime"
      guardTimeConf
      "GuardTime configuration"
  <*> fieldDef "mail_backdoor_open" False
      isMailBackdoorOpen
      "Enabling mails backdoor for test"
  <*> fieldDef "mail_noreply_address" (mailNoreplyAddress def)
      mailNoreplyAddress
      "Noreply address used when sending email"
  <*> fieldOpt "cgi_grp"
      cgiGrpConfig
      "CGI GRP (E-ID) configuration"
  <*> field "admins"
      admins
      "email addresses of people regarded as admins"
  <*> field "sales"
      sales
      "email addresses of people regarded as sales admins"
  <*> field "initial_users"
      initialUsers
      "email and passwords for initial users"
  <*> fieldOpt "mixpanel"
      mixpanelToken
      "Token for Mixpanel"
  <*> fieldOpt "trackjs"
      trackjsToken
      "API Token for Track.js"
  <*> fieldOpt "hubspot"
      hubspotConf
      "Configuration of HubSpot"
  <*> fieldOpt "salesforce"
      salesforceConf
      "Configuration of salesforce"
  <*> fieldOpt "nets"
      netsConfig
      "Configuration of Nets - NO BankID provider"
  <*> fieldOpt "monitoring"
      monitoringConfig
      "Configuration of the ekg-statsd-based monitoring."

instance Unjson AppConf where
  unjsonDef = unjsonAppConf

-- | Default application configuration that does nothing.
instance Default AppConf where
  def = AppConf {
      httpBindAddress    = (0x7f000001, 8000)
    , mainDomainUrl      = "http://localhost:8000"
    , useHttps           = False
    , amazonConfig       = Nothing
    , dbConfig           = "user='kontra' password='kontra' dbname='kontrakcja'"
    , maxDBConnections   = 100
    , redisCacheConfig   = Nothing
    , localFileCacheSize = 200000000
    , logConfig          = def
    , production         = False
    , cdnBaseUrl         = Nothing
    , guardTimeConf      = def
    , isMailBackdoorOpen = False
    , mailNoreplyAddress = "noreply@scrive.com"
    , cgiGrpConfig       = Nothing
    , admins             = []
    , sales              = []
    , initialUsers       = []
    , mixpanelToken      = Nothing
    , trackjsToken       = Nothing
    , hubspotConf        = Nothing
    , salesforceConf     = Nothing
    , netsConfig         = Nothing
    , monitoringConfig   = Nothing
    }
