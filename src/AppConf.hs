module AppConf (
    AppConf(..)
  ) where

import Configuration
import User.Model
import Mails.MailsConfig
import Data.Word
import System.Console.GetOpt
import LiveDocx (LiveDocxConf(..))
import ELegitimation.Config (LogicaConfig(..))
import GuardTime (GuardTimeConf(..))
import Payments.Config (RecurlyConfig(..))

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
  , static             :: FilePath                     -- ^ static files directory
  , amazonConfig       :: Maybe (String,String,String) -- ^ bucket, access key, secret key
  , dbConfig           :: String                       -- ^ postgresql configuration
  , srConfig           :: String                       -- ^ static resource spec file
  , production         :: Bool                         -- ^ production flag, enables some production stuff, disables some development
  , guardTimeConf      :: GuardTimeConf
  , mailsConfig        :: MailsConfig                  -- ^ mail sendout configuration
  , liveDocxConfig     :: LiveDocxConf                 -- ^ LiveDocx doc conversion configuration
  , logicaConfig       :: LogicaConfig                 -- ^ Logica (Elegitimation) configuration
  , admins             :: [Email]                      -- ^ email addresses of people regarded as admins
  , sales              :: [Email]                      -- ^ email addresses of people regarded as sales admins
  , initialUsers       :: [(Email,String)]             -- ^ email and passwords for initial users
  , recurlyConfig      :: RecurlyConfig                -- ^ for payments (api key + private key)
  , mixpanelToken      :: String                       -- ^ for mixpanel integration
  , homebase           :: String                       -- ^ url fragment where to fetch scripts
  , precogKey          :: String                       -- ^ API key for Precog
  , precogRootPath     :: String                       -- ^ API root path for Precog
  } deriving (Read, Eq, Ord, Show)

-- | Default application configuration that does nothing.
--
instance Configuration AppConf where
  confDefault = AppConf {
      httpBindAddress    = (0x7f000001, 8000)
    , hostpart           = "http://localhost:8000"
    , useHttps           = True
    , store              = "_local/kontrakcja/_state"
    , static             = "public"
    , amazonConfig       = Nothing
    , dbConfig           = "user='kontra' password='kontra' dbname='kontrakcja'"
    , srConfig           = "public/resources.spec"
    , production         = True
    , guardTimeConf      = GuardTimeConf { guardTimeURL = "http://stamper.guardtime.net/gt-signingservice" }
    , mailsConfig        = defaultMailsConfig
    , liveDocxConfig     = confDefault
    , logicaConfig       = LogicaConfig { logicaEndpoint = "https://eidt.funktionstjanster.se:18898/osif"
                                        , logicaServiceID = "logtest004"
                                        , logicaCertFile = "certs/steria3.pem"
                                        , logicaMBIEndpoint = "http://eidt.funktionstjanster.se:18899/mbi/service"
                                        , logicaMBIDisplayName = "Test av Mobilt BankID"
                                        }
    , admins             = map Email ["gracjanpolak@gmail.com", "lukas@skrivapa.se"]
    , sales              = []
    , initialUsers       = []
    , recurlyConfig      = RecurlyConfig { recurlySubdomain  = "scrive-test"
                                         , recurlyAPIKey     = "c31afaf14af3457895ee93e7e08e4451"
                                         , recurlyPrivateKey = "49c1b30592fa475b8535a0ca04f88e65"
                                         }
    , mixpanelToken      = "5b04329b972851feac0e9b853738e742"
    , homebase           = "https://staging.scrive.com"
    , precogKey = ""
    , precogRootPath = ""
    }
  confOptions = [
  {-
    , Option [] ["no-validate"]
      (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Nothing } }))
      "Turn off HTML validation"
    , Option [] ["validate"]
      (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Just wdgHTMLValidator } }))
      "Turn on HTML validation"
    -}
      Option [] ["store"]
      (ReqArg (\h c -> c {store = h}) "PATH")
      "The directory used for database storage."
    , Option [] ["static"]
      (ReqArg (\h c -> c {static = h}) "PATH")
      "The directory searched for static files"
    , Option [] ["production"]
      (NoArg (\ c -> c { production = True }))
      "Turn on production environment"
    ]
  confVerify _ = return $ Right ()
