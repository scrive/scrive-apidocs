module Context (
    Context(..)
  , ctxDomainUrl
  , getContextUser
  , anonymousContext
  , contextToMailContext
  ) where

import qualified Control.Concurrent.Thread as T
import qualified Data.ByteString as BS

import BrandedDomain.BrandedDomain
import Branding.Cache
import Doc.RenderedPages
import EID.CGI.GRP.Config
import EID.Nets.Config
import File.FileID
import FlashMessage
import GuardTime (GuardTimeConf(..))
import HubSpot.Conf
import IPAddress
import KontraPrelude
import LiveDocx (LiveDocxConf(..))
import MagicHash (MagicHash)
import MailContext (MailContext(..))
import Mails.MailsConfig
import MinutesTime
import Payments.Config (RecurlyConfig)
import Salesforce.Conf
import ServerUtils.BrandedImagesCache
import Session.SessionID
import Templates
import User.Email
import User.Model
import qualified MemCache

data Context = Context
    { ctxmaybeuser           :: Maybe User -- ^ The logged in user. Is Nothing when there is no one logged in.
    , ctxflashmessages       :: [FlashMessage] -- ^ The flash messages for the NEXT request.
    , ctxtime                :: UTCTime -- ^ The time of the request.
    , ctxclientname          :: Maybe String -- ^ Client identification from header Client-Name or if that's missing: User-Agent
    , ctxclienttime          :: Maybe UTCTime -- ^ Client-local time when an action was performed (e.g. signing a document)
    , ctxnormalizeddocuments :: RenderedPagesCache -- ^ Rendered jpeg pages
    , ctxipnumber            :: IPAddress -- ^ The ip number of the client.
    , ctxproduction          :: Bool -- ^ Is this server the production server?
    , ctxcdnbaseurl          :: Maybe String -- ^ CDN base URL if one shouild be used
    , ctxtemplates           :: KontrakcjaTemplates -- ^ The set of templates to render text for the ctxlang
    , ctxglobaltemplates     :: KontrakcjaGlobalTemplates -- ^ All of the templates for all valid langs
    , ctxlang                :: Lang -- ^ The current context lang
    , ctxmailsconfig         :: MailsConfig
    , ctxlivedocxconf        :: LiveDocxConf -- ^ LiveDocx configuration (does doc conversion)
    , ctxcgigrpconfig        :: CgiGrpConfig
    , ctxgtconf              :: GuardTimeConf -- ^ GuardTime configuration
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , ctxlesscache           :: LessCache -- ^
    , ctxbrandedimagescache  :: BrandedImagesCache -- ^
    , ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , ctxadminaccounts       :: [Email] -- ^
    , ctxsalesaccounts       :: [Email] -- ^
    , ctxmaybepaduser        :: Maybe User -- ^ If we are loged in to the pad view
    , ctxusehttps            :: Bool
    , ctxrecurlyconfig       :: RecurlyConfig
    , ctxsessionid           :: SessionID
    , ctxmixpaneltoken       :: String
    , ctxhubspotconf         :: HubSpotConf
    , ctxgoogleanalyticstoken :: String
    , ctxbrandeddomain       :: BrandedDomain
    , ctxsalesforceconf      :: SalesforceConf
    , ctxnetsconfig          :: Maybe NetsConfig
    -- | Contains actions that join threads spawned with forkAction
    , ctxthreadjoins       :: [IO (T.Result ())]
    }

ctxDomainUrl :: Context -> String
ctxDomainUrl = bdUrl . ctxbrandeddomain

-- | Get a user from `Context` (user takes precedence over pad user).
getContextUser :: Context -> Maybe User
getContextUser Context{..} = ctxmaybeuser `mplus` ctxmaybepaduser

-- | anonymousContext changes given context into one that does not hold any user details.
-- | use this if your action requires different for of authentication
anonymousContext :: Context -> Context
anonymousContext ctx = ctx { ctxmaybeuser = Nothing, ctxmaybepaduser = Nothing, ctxsessionid = tempSessionID }

contextToMailContext :: Context -> MailContext
contextToMailContext ctx = MailContext {
    mctxmailsconfig = ctxmailsconfig ctx
  , mctxlang = ctxlang ctx
  , mctxcurrentBrandedDomain = ctxbrandeddomain ctx
  , mctxtime = ctxtime ctx
  }

instance HasSalesforceConf Context where
  getSalesforceConf = ctxsalesforceconf
