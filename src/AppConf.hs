{-# OPTIONS_GHC -fno-warn-orphans #-}
module AppConf (
      AppConf(..)
      , unjsonAppConf
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Unjson
import Data.Word
import qualified Data.Map as Map

import EID.CGI.GRP.Config
import GuardTime (GuardTimeConf(..))
import HostClock.System (defaultNtpServers)
import HubSpot.Conf (HubSpotConf(..))
import LiveDocx (LiveDocxConf(..))
import Mails.MailsConfig
import Payments.Config (RecurlyConfig(..))
import Salesforce.Conf
import User.Email
import Utils.Default

-- | Defines the application's configuration.  This includes amongst
-- other things the http port number, amazon, trust weaver and email
-- configuraton, as well as a handy boolean indicating whether this is
-- a production or development instance.
data AppConf = AppConf {
    httpBindAddress    :: (Word32, Word16)             -- ^ tcp address to bind to and port to listen on
                                                       -- (0x7f000001, 8000) localhost:8000 (default)
                                                       -- (0, 80)   all interfaces port 80
  , hostpart           :: String                       -- ^ hostname as it should looklike in emails for example
  , useHttps           :: Bool                         -- ^ should we redirect to https?
  , store              :: FilePath                     -- ^ where to put database files
  , amazonConfig       :: Maybe (String,String,String) -- ^ bucket, access key, secret key
  , dbConfig           :: ByteString                   -- ^ postgresql configuration
  , production         :: Bool                         -- ^ production flag, enables some production stuff, disables some development
  , guardTimeConf      :: GuardTimeConf
  , mailsConfig        :: MailsConfig                  -- ^ mail sendout configuration
  , liveDocxConfig     :: LiveDocxConf                 -- ^ LiveDocx doc conversion configuration
  , cgiGrpConfig       :: CgiGrpConfig                 -- ^ CGI GRP (E-ID) configuration
  , admins             :: [Email]                      -- ^ email addresses of people regarded as admins
  , sales              :: [Email]                      -- ^ email addresses of people regarded as sales admins
  , initialUsers       :: [(Email,String)]             -- ^ email and passwords for initial users
  , recurlyConfig      :: RecurlyConfig                -- ^ for payments (api key + private key)
  , mixpanelToken      :: String                       -- ^ for mixpanel integration
  , hubspotConf        :: HubSpotConf                  -- ^ for hubspot integration
  , googleanalyticsToken      :: String                -- ^ for google-analytics integration
  , homebase           :: String                       -- ^ url fragment where to fetch scripts
  , ntpServers         :: [String]                     -- ^ List of NTP servers to contact to get estimate of host clock error
  , salesforceConf     :: SalesforceConf             -- ^ Configuration of salesforce
  } deriving (Eq, Ord, Show)

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
  <*> field "hostpart"
      hostpart
      "A string how this server is visible on outside"
  <*> fieldDef "https" True
      useHttps
      "Should use https"
  <*> field "store"
      store
      "Where to put database files"
  <*> fieldOptBy "amazon"
      amazonConfig
      "Amazon configuration"
      (objectOf $ pure (,,)
       <*> field "bucket"
         (\(x,_,_) -> x)
         "In which bucket to store new files"
       <*> field "access_key"
         (\(_,x,_) -> x)
         "Amazon access key"
       <*> field "secret_key"
         (\(_,_,x) -> x)
         "Amazon secret key")
  <*> fieldBy "database"
      dbConfig
      "Database connection string"
      unjsonAeson
  <*> fieldDef "production" False
      production
      "Is this production server"
  <*> field "guardtime"
      guardTimeConf
      "GuardTime configuration"
  <*> field "mails"
      mailsConfig
      "Mails configuration"
  <*> field "livedocx"
      liveDocxConfig
      "LiveDocx doc conversion configuration"
  <*> field "cgi_grp"
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
  <*> field "recurly"
      recurlyConfig
      "Recurly configuration for payments"
  <*> field "mixpanel"
      mixpanelToken
      "Token for Mixpanel"
  <*> field "hubspot"
      hubspotConf
      "Configuration of HubSpot"
  <*> field "google_analytics"
      googleanalyticsToken
      "Token for Google Analytics"
  <*> field "homebase"
      homebase
      "url fragment where to fetch scripts"
  <*> field "ntp_servers"
      ntpServers
      "List of NTP servers to contact to get estimate of host clock error"
  <*> field "salesforce"
      salesforceConf
      "Configuration of salesforce"

instance Unjson AppConf where
  unjsonDef = unjsonAppConf

-- | Default application configuration that does nothing.
--
instance HasDefaultValue AppConf where
  defaultValue = AppConf {
      httpBindAddress    = (0x7f000001, 8000)
    , hostpart           = "http://localhost:8000"
    , useHttps           = True
    , store              = "_local/kontrakcja/_state"
    , amazonConfig       = Nothing
    , dbConfig           = "user='kontra' password='kontra' dbname='kontrakcja'"
    , production         = True
    , guardTimeConf      = GuardTimeConf { guardTimeURL="http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-signingservice"
                                         , guardTimeExtendingServiceURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-extendingservice"
                                         , guardTimeControlPublicationsURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-controlpublications.bin"
                                         }
    , mailsConfig        = defaultMailsConfig
    , liveDocxConfig     = defaultValue
    , cgiGrpConfig       = CgiGrpConfig {
        cgGateway = "https://grpt.funktionstjanster.se:18898/grp/v1"
      , cgCertFile = "certs/steria3.pem"
      , cgServiceID = "logtest004"
      , cgDisplayName = "Funktionstjänster Test"
      }
    , admins             = map Email ["gracjanpolak@gmail.com", "lukas@skrivapa.se"]
    , sales              = []
    , initialUsers       = []
    , recurlyConfig      = RecurlyConfig { recurlySubdomain  = "scrive-test"
                                         , recurlyAPIKey     = "c31afaf14af3457895ee93e7e08e4451"
                                         , recurlyPrivateKey = "49c1b30592fa475b8535a0ca04f88e65"
                                         }
    , mixpanelToken      = "5b04329b972851feac0e9b853738e742"
    , hubspotConf        = HubSpotConf "" Map.empty 
    , googleanalyticsToken = "f25e59c70a8570a12fe57e7835d1d881"
    , homebase           = "https://staging.scrive.com"
    , ntpServers         = defaultNtpServers
    , salesforceConf     = SalesforceConf
                              { salesforceAuthenticationUrl = "https://login.salesforce.com/services/oauth2/authorize"
                              , salesforceTokenUrl = "https://login.salesforce.com/services/oauth2/token"
                              , salesforceConsumerKey = "3MVG9A2kN3Bn17htNVdtvb5RT3xDFJXCsLqYZX0eYz18WEOqZcOCwrusUxSEOanVBEZRYhhFpZbtjEQGJI7Db"
                              , salesforceConsumerSecret = "5081538550608494771"
                              , salesforceRedirectUrl = "https://scrive.com/salesforce/integration"
                              , salesforceIntegrationAPIToken  = "12ef3_22"
                              , salesforceIntegrationAPISecret = "a1033b2caa"
                              }
    }

instance HasSalesforceConf AppConf where
  getSalesforceConf =  salesforceConf
