module AppConf (
    AppConf(..)
  ) where

import Configuration
import Crypto
import User.Model
import Mails.MailsConfig
import Data.Word
import System.Console.GetOpt
import qualified Data.ByteString.Char8 as BS

-- | Defines the application's configuration.  This includes amongst
-- other things the http port number, amazon, trust weaver and email
-- configuraton, as well as a handy boolean indicating whether this is
-- a production or development instance.
data AppConf = AppConf {
    httpBindAddress    :: (Word32, Word16)             -- ^ tcp address to bind to and port to listen on
                                                       -- (0x7f000001, 8000) localhost:8000 (default)
                                                       -- (0, 80)   all interfaces port 80
  , hostpart           :: String                       -- ^ hostname as it should looklike in emails for example
  , store              :: FilePath                     -- ^ where to put database files
  , docstore           :: FilePath                     -- ^ where to put files (active if amazonConfig is Nothing)
  , static             :: FilePath                     -- ^ static files directory
  , amazonConfig       :: Maybe (String,String,String) -- ^ bucket, access key, secret key
  , dbConfig           :: String                       -- ^ postgresql configuration
  , gsCmd              :: String
  , srConfig           :: String                       -- ^ static resource spec file
  , production         :: Bool                         -- ^ production flag, enables some production stuff, disables some development
  , trustWeaverSign    :: Maybe (String,String,String) -- ^ TrustWeaver sign service (URL,pem file path,pem private key password)
  , trustWeaverAdmin   :: Maybe (String,String,String) -- ^ TrustWeaver admin service (URL,pem file path,pem private key password)
  , trustWeaverStorage :: Maybe (String,String,String) -- ^ TrustWeaver storage service (URL,pem file path,pem private key password)
  , mailsConfig        :: MailsConfig                  -- ^ mail sendout configuration
  , aesConfig          :: AESConf                      -- ^ aes key/iv for encryption
  , admins             :: [Email]                      -- ^ email addresses of people regarded as admins
  , sales              :: [Email]                      -- ^ email addresses of people regarded as sales admins
  , initialUsers       :: [(Email,String)]             -- ^ email and passwords for initial users
  } deriving (Read, Eq, Ord, Show)

-- | Default application configuration that does nothing.
--
-- sign url    "https://tseiod-dev.trustweaver.com/ts/svs.asmx"
-- admin url   "https://twa-test-db.trustweaver.com/ta_hubservices/Admin/AdminService.svc"
-- storage url "https://twa-test-db.trustweaver.com/ta_hubservices/Storage/StorageService.svc"
instance Configuration AppConf where
  confDefault = AppConf {
      httpBindAddress    = (0x7f000001, 8000)
    , hostpart           = "http://localhost:8000"
    , store              = "_local/kontrakcja/_state"
    , docstore           = "_local/documents"
    , static             = "public"
    , amazonConfig       = Nothing
    , dbConfig           = "user='kontra' password='kontra' dbname='kontrakcja'"
    , gsCmd              = "gs"
    , srConfig           = "public/resources.spec"
    , production         = True
    , trustWeaverSign    = Nothing
    , trustWeaverAdmin   = Nothing
    , trustWeaverStorage = Nothing
    , mailsConfig        = defaultMailsConfig
    , aesConfig          = AESConf {
        aesKey = BS.pack "}>\230\206>_\222\STX\218\SI\159i\DC1H\DC3Q\ENQK\r\169\183\133bu\211\NUL\251s|\207\245J"
      , aesIV = BS.pack "\205\168\250\172\CAN\177\213\EOT\254\190\157SY3i\160"
      }
    , admins             = map Email ["gracjanpolak@gmail.com", "lukas@skrivapa.se"]
    , sales              = []
    , initialUsers       = []
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
  confVerify conf = case verifyAESConf $ aesConfig conf of
    Left err -> return $ Left err
    _        -> return $ Right ()
