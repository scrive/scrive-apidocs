module Context (
      Context(..),
      anonymousContext,
      currentBrandedDomain,
      MailContext(..),
      HasMailContext(..)
    ) where

import File.FileID
import Doc.JpegPages
import MinutesTime
import User.Model
import qualified Data.ByteString as BS
import Templates
import qualified MemCache
import FlashMessage
import Mails.MailsConfig
import LiveDocx (LiveDocxConf(..))
import MagicHash (MagicHash)
import IPAddress
import Session.SessionID
import qualified Static.Resources as SR
import ELegitimation.Config (LogicaConfig(..))
import GuardTime (GuardTimeConf(..))
import Payments.Config (RecurlyConfig)
import BrandedDomains
import Salesforce.Conf

data Context = Context
    { ctxmaybeuser           :: Maybe User -- ^ The logged in user. Is Nothing when there is no one logged in.
    , ctxhostpart            :: String -- ^ The hostname of the URL for the request.
    , ctxresourcehostpart    :: String -- ^ The hostname for the resources (will be https if possible)
    , ctxflashmessages       :: [FlashMessage] -- ^ The flash messages for the NEXT request.
    , ctxtime                :: MinutesTime -- ^ The time of the request.
    , ctxnormalizeddocuments :: MemCache.MemCache FileID JpegPages -- ^ Rendered jpeg pages
    , ctxipnumber            :: IPAddress -- ^ The ip number of the client.
    , ctxproduction          :: Bool -- ^ Is this server the production server?
    , ctxtemplates           :: KontrakcjaTemplates -- ^ The set of templates to render text for the ctxlang
    , ctxglobaltemplates     :: KontrakcjaGlobalTemplates -- ^ All of the templates for all valid langs
    , ctxlang                :: Lang -- ^ The current context lang
    , ctxmailsconfig         :: MailsConfig
    , ctxlivedocxconf        :: LiveDocxConf -- ^ LiveDocx configuration (does doc conversion)
    , ctxlogicaconf          :: LogicaConfig -- ^ Logica configuration (eleg)
    , ctxgtconf              :: GuardTimeConf -- ^ GuardTime configuration
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , ctxadminaccounts       :: [Email] -- ^
    , ctxsalesaccounts       :: [Email] -- ^
    , ctxmaybepaduser        :: Maybe User -- ^ If we are loged in to the pad view
    , ctxstaticresources     :: SR.ResourceSetsForImport
    , ctxusehttps            :: Bool
    , ctxrecurlyconfig       :: RecurlyConfig
    , ctxsessionid           :: SessionID
    , ctxmixpaneltoken       :: String
    , ctxhomebase            :: String
    , ctxbrandeddomains      :: BrandedDomains
    , ctxsalesforceconf      :: SalesforceConf
    }

-- | anonymousContext changes given context into one that does not hold any user details.
-- | use this if your action requires different for of authentication
anonymousContext :: Context -> Context
anonymousContext ctx = ctx { ctxmaybeuser = Nothing, ctxmaybepaduser = Nothing, ctxsessionid = tempSessionID }

-- | Current branded domain
currentBrandedDomain :: Context -> Maybe BrandedDomain
currentBrandedDomain ctx = findBrandedDomain (ctxhostpart ctx) (ctxbrandeddomains ctx)

-- TODO: consider breaking KontraMonad into smaller parts for more
-- fine-grained access to context, because some parts are needed
-- outside request handlers.
-- | Subset of context that is used in cron process
data MailContext = MailContext
  { mctxhostpart :: String
  , mctxmailsconfig :: MailsConfig
  , mctxlang :: Lang
  , mctxcurrentBrandedDomain :: Maybe BrandedDomain
  , mctxipnumber :: IPAddress
  , mctxtime :: MinutesTime
  , mctxmaybeuser :: Maybe User
  }
  deriving Show

class HasMailContext c where
  mailContext :: c -> MailContext

instance HasMailContext MailContext where
  mailContext = id
instance HasMailContext Context where
  mailContext ctx = MailContext { mctxhostpart = ctxhostpart ctx
                                , mctxmailsconfig = ctxmailsconfig ctx
                                , mctxlang = ctxlang ctx
                                , mctxcurrentBrandedDomain = currentBrandedDomain ctx
                                , mctxipnumber = ctxipnumber ctx
                                , mctxtime = ctxtime ctx
                                , mctxmaybeuser = ctxmaybeuser ctx
                                }

instance HasSalesforceConf Context where
  getSalesforceConf = ctxsalesforceconf
