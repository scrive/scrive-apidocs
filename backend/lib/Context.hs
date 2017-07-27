module Context (
    Context(..)
  , ctxDomainUrl
  , getContextUser
  , anonymousContext
  , contextToMailContext
  ) where

import Log.Class
import qualified Data.ByteString as BS
import qualified Database.Redis as R

import BrandedDomain.BrandedDomain
import EID.CGI.GRP.Config
import EID.Nets.Config
import File.FileID
import GuardTime (GuardTimeConf(..))
import HubSpot.Conf
import IPAddress
import KontraPrelude
import MagicHash (MagicHash)
import MailContext (MailContext(..))
import Salesforce.Conf
import Session.SessionID
import Templates
import User.Email
import User.Model
import qualified MemCache

data Context = Context
    { ctxmaybeuser           :: Maybe User -- ^ The logged in user. Is Nothing when there is no one logged in.
    , ctxtime                :: UTCTime -- ^ The time of the request.
    , ctxclientname          :: Maybe String -- ^ Client identification from header Client-Name or if that's missing: User-Agent
    , ctxclienttime          :: Maybe UTCTime -- ^ Client-local time when an action was performed (e.g. signing a document)
    , ctxipnumber            :: IPAddress -- ^ The ip number of the client.
    , ctxproduction          :: Bool -- ^ Is this server the production server?
    , ctxcdnbaseurl          :: Maybe String -- ^ CDN base URL if one shouild be used
    , ctxtemplates           :: KontrakcjaTemplates -- ^ The set of templates to render text for the ctxlang
    , ctxglobaltemplates     :: KontrakcjaGlobalTemplates -- ^ All of the templates for all valid langs
    , ctxlang                :: Lang -- ^ The current context lang
    , ctxismailbackdooropen  :: Bool
    , ctxmailnoreplyaddress  :: String -- ^ The "noreply" address used when sending email
    , ctxcgigrpconfig        :: Maybe CgiGrpConfig
    , ctxgtconf              :: GuardTimeConf -- ^ GuardTime configuration
    , ctxmrediscache         :: Maybe R.Connection
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , ctxadminaccounts       :: [Email] -- ^
    , ctxsalesaccounts       :: [Email] -- ^
    , ctxmaybepaduser        :: Maybe User -- ^ If we are loged in to the pad view
    , ctxusehttps            :: Bool
    , ctxsessionid           :: SessionID
    , ctxtrackjstoken        :: Maybe String
    , ctxmixpaneltoken       :: Maybe String
    , ctxhubspotconf         :: Maybe HubSpotConf
    , ctxbrandeddomain       :: BrandedDomain
    , ctxsalesforceconf      :: Maybe SalesforceConf
    , ctxnetsconfig          :: Maybe NetsConfig
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
    mctxlang = ctxlang ctx
  , mctxcurrentBrandedDomain = ctxbrandeddomain ctx
  , mctxtime = ctxtime ctx
  , mctxmailNoreplyAddress = ctxmailnoreplyaddress ctx
  }
