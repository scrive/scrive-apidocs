module Context (
    DelayedResponse(..)
  , Context(..)
  , ctxDomainUrl
  , getContextUser
  , anonymousContext
  , contextToMailContext
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Happstack.Server (Response)
import Log.Class
import qualified Control.Concurrent.Thread as T
import qualified Data.ByteString as BS
import qualified Database.Redis as R

import BrandedDomain.BrandedDomain
import Doc.RenderedPages
import EID.CGI.GRP.Config
import EID.Nets.Config
import File.FileID
import FlashMessage
import GuardTime (GuardTimeConf(..))
import HubSpot.Conf
import IPAddress
import KontraPrelude
import MagicHash (MagicHash)
import MailContext (MailContext(..))
import Payments.Config (RecurlyConfig)
import Salesforce.Conf
import Session.SessionID
import Templates
import User.Email
import User.Model
import qualified MemCache

-- | Action that generates Response after database connection is no longer held
-- by the request handler. If present in 'Context', it will overwrite Response
-- object returned by standard means.
newtype DelayedResponse = DelayedResponse {
    unDelayedResponse :: forall m. (MonadBaseControl IO m, MonadLog m, MonadThrow m) => m (Maybe Response)
  }

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
    , ctxismailbackdooropen  :: Bool
    , ctxcgigrpconfig        :: Maybe CgiGrpConfig
    , ctxgtconf              :: GuardTimeConf -- ^ GuardTime configuration
    , ctxmrediscache         :: Maybe R.Connection
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , ctxadminaccounts       :: [Email] -- ^
    , ctxsalesaccounts       :: [Email] -- ^
    , ctxmaybepaduser        :: Maybe User -- ^ If we are loged in to the pad view
    , ctxusehttps            :: Bool
    , ctxrecurlyconfig       :: Maybe RecurlyConfig
    , ctxsessionid           :: SessionID
    , ctxtrackjstoken        :: Maybe String
    , ctxmixpaneltoken       :: Maybe String
    , ctxhubspotconf         :: Maybe HubSpotConf
    , ctxbrandeddomain       :: BrandedDomain
    , ctxsalesforceconf      :: Maybe SalesforceConf
    , ctxnetsconfig          :: Maybe NetsConfig
    , ctxdelayedresponse     :: Maybe DelayedResponse
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
    mctxlang = ctxlang ctx
  , mctxcurrentBrandedDomain = ctxbrandeddomain ctx
  , mctxtime = ctxtime ctx
  }
