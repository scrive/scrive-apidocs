module AppConf (
      AppConf(..)
      , unjsonAppConf
  ) where

import Data.Default
import Data.Unjson
import Data.Word
import qualified Data.Text as T

import Database.Redis.Configuration
import EID.CGI.GRP.Config
import EID.Nets.Config
import GuardTime (GuardTimeConf(..))
import HostClock.System (defaultNtpServers)
import HubSpot.Conf (HubSpotConf(..))
import KontraPrelude
import LiveDocx (LiveDocxConf(..))
import Log.Configuration
import Payments.Config (RecurlyConfig(..))
import Salesforce.Conf
import User.Email

-- | Defines the application's configuration.  This includes amongst
-- other things the http port number, amazon, trust weaver and email
-- configuraton, as well as a handy boolean indicating whether this is
-- a production or development instance.
data AppConf = AppConf {
    httpBindAddress    :: (Word32, Word16)             -- ^ tcp address to bind to and port to listen on
                                                       -- (0x7f000001, 8000) localhost:8000 (default)
                                                       -- (0, 80)   all interfaces port 80
  , mainDomainUrl      :: String                       -- ^ base url of the main domain
  , useHttps           :: Bool                         -- ^ should we redirect to https?
  , amazonConfig       :: Maybe (String,String,String) -- ^ bucket, access key, secret key
  , dbConfig           :: T.Text                       -- ^ postgresql configuration
  , redisCacheConfig   :: Maybe RedisConfig            -- ^ redis configuration
  , logConfig          :: LogConfig                    -- ^ logging configuration
  , production         :: Bool                         -- ^ production flag, enables some production stuff, disables some development
  , cdnBaseUrl         :: Maybe String                 -- ^ for CDN content in prod mode
  , guardTimeConf      :: GuardTimeConf
  , isMailBackdoorOpen :: Bool                         -- ^ If true allows admins to access last mail send. Used by selenium
  , liveDocxConfig     :: Maybe LiveDocxConf           -- ^ LiveDocx doc conversion configuration
  , cgiGrpConfig       :: Maybe CgiGrpConfig           -- ^ CGI GRP (E-ID) configuration
  , admins             :: [Email]                      -- ^ email addresses of people regarded as admins
  , sales              :: [Email]                      -- ^ email addresses of people regarded as sales admins
  , initialUsers       :: [(Email,String)]             -- ^ email and passwords for initial users
  , recurlyConfig      :: Maybe RecurlyConfig          -- ^ for payments (api key + private key)
  , mixpanelToken      :: Maybe String                 -- ^ for mixpanel integration
  , trackjsToken       :: Maybe String                 -- ^ for Track.js integration
  , hubspotConf        :: Maybe HubSpotConf            -- ^ for hubspot integration
  , googleanalyticsToken      :: String                -- ^ for google-analytics integration
  , ntpServers         :: [String]                     -- ^ List of NTP servers to contact to get estimate of host clock error
  , salesforceConf     :: SalesforceConf               -- ^ Configuration of salesforce
  , netsConfig         :: Maybe NetsConfig             -- ^ Configuration of Nets - NO BankID provider
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
  <*> fieldOpt "redis_cache"
      redisCacheConfig
      "Redis cache configuration"
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
  <*> fieldOpt "livedocx"
      liveDocxConfig
      "LiveDocx doc conversion configuration"
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
  <*> fieldOpt "recurly"
      recurlyConfig
      "Recurly configuration for payments"
  <*> fieldOpt "mixpanel"
      mixpanelToken
      "Token for Mixpanel"
  <*> fieldOpt "trackjs"
      trackjsToken
      "API Token for Track.js"
  <*> fieldOpt "hubspot"
      hubspotConf
      "Configuration of HubSpot"
  <*> field "google_analytics"
      googleanalyticsToken
      "Token for Google Analytics"
  <*> field "ntp_servers"
      ntpServers
      "List of NTP servers to contact to get estimate of host clock error"
  <*> field "salesforce"
      salesforceConf
      "Configuration of salesforce"
  <*> fieldOpt "nets"
      netsConfig
      "Configuration of Nets - NO BankID provider"

instance Unjson AppConf where
  unjsonDef = unjsonAppConf

-- | Default application configuration that does nothing.
instance Default AppConf where
  def = AppConf {
      httpBindAddress    = (0x7f000001, 8000)
    , mainDomainUrl      = "http://localhost:8000"
    , useHttps           = True
    , amazonConfig       = Nothing
    , dbConfig           = "user='kontra' password='kontra' dbname='kontrakcja'"
    , redisCacheConfig   = Just def
    , logConfig          = def
    , production         = True
    , cdnBaseUrl         = Nothing
    , guardTimeConf      = GuardTimeConf { guardTimeURL="http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-signingservice"
                                         , guardTimeExtendingServiceURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-extendingservice"
                                         , guardTimeControlPublicationsURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-controlpublications.bin"
                                         }
    , isMailBackdoorOpen = False
    , liveDocxConfig     = Nothing
    , cgiGrpConfig       = Nothing
    , admins             = map Email ["gracjanpolak@gmail.com", "lukas@skrivapa.se"]
    , sales              = []
    , initialUsers       = []
    , recurlyConfig      = Nothing
    , mixpanelToken      = Nothing
    , trackjsToken       = Nothing
    , hubspotConf        = Nothing
    , googleanalyticsToken = "f25e59c70a8570a12fe57e7835d1d881"
    , ntpServers         = defaultNtpServers
    , salesforceConf     = SalesforceConf
                              { salesforceAuthenticationUrl = "https://login.salesforce.com/services/oauth2/authorize"
                              , salesforceTokenUrl = "https://login.salesforce.com/services/oauth2/token"
                              , salesforceConsumerKey = "3MVG9A2kN3Bn17htNVdtvb5RT3xDFJXCsLqYZX0eYz18WEOqZcOCwrusUxSEOanVBEZRYhhFpZbtjEQGJI7Db"
                              , salesforceConsumerSecret = "5081538550608494771"
                              , salesforceRedirectUrl = "https://scrive.com/salesforce/integration"
                              , salesforceIntegrationAPIToken  = "12ef3_22"
                              , salesforceIntegrationAPISecret = "a1033b2caa"
                              , salesforceErrorEmail = Just "info@scrive.com"
                              }
     , netsConfig        = Just $ NetsConfig {
                                netsMerchantIdentifier = "SRWUOEDLAXDV"
                              , netsMerchantPassword = "21r1ee95bp4n"
                              , netsIdentifyUrl = "https://ti-pp.bbs.no/its/index.html"
                              , netsAssertionUrl = "https://ti-pp.bbs.no/saml1resp/getassertion"
                              , netsTrustedDomain = "https://dev.scrive.com"
                              }

    }

instance HasSalesforceConf AppConf where
  getSalesforceConf =  salesforceConf
