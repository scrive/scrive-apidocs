module Context (
      Context(..)
    ) where

import Control.Concurrent.MVar
import DB.Classes (DBEnv)
import File.FileID
import Doc.JpegPages
import Doc.SignatoryLinkID
import MinutesTime
import User.Model
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import Templates.Templates
import qualified TrustWeaver as TW
import ELegitimation.ELegTransaction
import qualified MemCache
import FlashMessage
import Mails.MailsConfig
import API.Service.Model
import Company.Model
import MagicHash (MagicHash)
import IPAddress

data Context = Context
    { ctxmaybeuser           :: Maybe User -- ^ The logged in user. Is Nothing when there is no one logged in.
    , ctxhostpart            :: String -- ^ The hostname of the URL for the request.
    , ctxflashmessages       :: [FlashMessage] -- ^ The flash messages for the NEXT request.
    , ctxtime                :: MinutesTime -- ^ The time of the request.
    , ctxnormalizeddocuments :: MVar (Map.Map FileID JpegPages) -- ^
    , ctxipnumber            :: IPAddress -- ^ The ip number of the client.
    , ctxdbenv               :: DBEnv -- ^ PostgreSQL database environment
    , ctxdocstore            :: FilePath -- ^ The temporary document directory.
    , ctxs3action            :: AWS.S3Action -- ^
    , ctxgscmd               :: String -- ^
    , ctxproduction          :: Bool -- ^ Is this server the production server?
    , ctxtemplates           :: KontrakcjaTemplates -- ^ The set of templates to render text for the ctxlocale
    , ctxglobaltemplates     :: KontrakcjaGlobalTemplates -- ^ All of the templates for all valid locales
    , ctxlocale              :: Locale -- ^ The current context locale
    , ctxlocaleswitch        :: Bool -- ^ Whether locale switching is available on this page
    , ctxmailsconfig         :: MailsConfig
    , ctxtwconf              :: TW.TrustWeaverConf -- ^ TrustWeaver configuration
    , ctxelegtransactions    :: [ELegTransaction] -- ^ Transactions for connections to the Logica server
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , ctxcompany             :: Maybe Company -- ^
    , ctxservice             :: Maybe Service -- ^
    , ctxlocation            :: String -- ^
    , ctxadminaccounts       :: [Email] -- ^
    , ctxsalesaccounts       :: [Email] -- ^
    , ctxmagichashes         :: Map.Map SignatoryLinkID MagicHash
    }
