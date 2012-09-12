module Context (
      Context(..)
    ) where

import File.FileID
import Doc.JpegPages
import Doc.SignatoryLinkID
import MinutesTime
import User.Model
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import Templates.TemplatesLoader
import ELegitimation.ELegTransaction
import qualified MemCache
import FlashMessage
import Mails.MailsConfig
import LiveDocx (LiveDocxConf(..))
import Company.Model
import MagicHash (MagicHash)
import IPAddress
import qualified Static.Resources as SR
import ELegitimation.Config (LogicaConfig(..))
import GuardTime (GuardTimeConf(..))
import Payments.Config (RecurlyConfig)

data Context = Context
    { ctxmaybeuser           :: Maybe User -- ^ The logged in user. Is Nothing when there is no one logged in.
    , ctxhostpart            :: String -- ^ The hostname of the URL for the request.
    , ctxresourcehostpart    :: String -- ^ The hostname for the resources (will be https if possible)
    , ctxflashmessages       :: [FlashMessage] -- ^ The flash messages for the NEXT request.
    , ctxtime                :: MinutesTime -- ^ The time of the request.
    , ctxnormalizeddocuments :: MemCache.MemCache FileID JpegPages -- ^ Rendered jpeg pages
    , ctxipnumber            :: IPAddress -- ^ The ip number of the client.
    , ctxdocstore            :: FilePath -- ^ The temporary document directory.
    , ctxs3action            :: AWS.S3Action -- ^
    , ctxgscmd               :: String -- ^
    , ctxproduction          :: Bool -- ^ Is this server the production server?
    , ctxtemplates           :: KontrakcjaTemplates -- ^ The set of templates to render text for the ctxlocale
    , ctxglobaltemplates     :: KontrakcjaGlobalTemplates -- ^ All of the templates for all valid locales
    , ctxlocale              :: Locale -- ^ The current context locale
    , ctxmailsconfig         :: MailsConfig
    , ctxlivedocxconf        :: LiveDocxConf -- ^ LiveDocx configuration (does doc conversion)
    , ctxlogicaconf          :: LogicaConfig -- ^ Logica configuration (eleg)
    , ctxgtconf              :: GuardTimeConf -- ^ GuardTime configuration
    , ctxelegtransactions    :: [ELegTransaction] -- ^ Transactions for connections to the Logica server
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , ctxcompany             :: Maybe Company -- ^
    , ctxlocation            :: String -- ^
    , ctxadminaccounts       :: [Email] -- ^
    , ctxsalesaccounts       :: [Email] -- ^
    , ctxmagichashes         :: Map.Map SignatoryLinkID MagicHash
    , ctxmaybepaduser        :: Maybe User -- ^ If we are loged in to the pad view
    , ctxstaticresources     :: SR.ResourceSetsForImport
    , ctxusehttps            :: Bool
    , ctxrecurlyconfig       :: RecurlyConfig
    }
