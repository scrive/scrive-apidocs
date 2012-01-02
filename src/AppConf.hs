{-# LANGUAGE RecordWildCards #-}


module AppConf
    ( AppConf(..)
    ) where

import Crypto
import User.Model
import Mails.MailsConfig
import Data.Word

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
