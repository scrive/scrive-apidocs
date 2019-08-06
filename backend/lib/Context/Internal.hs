module Context.Internal where

import Log.Class
import qualified Database.Redis as R

import BrandedDomain.BrandedDomain
import EID.CGI.GRP.Config
import EID.EIDService.Conf
import EID.Nets.Config
import FileStorage
import GuardTime (GuardTimeConf(..))
import HubSpot.Conf
import IPAddress
import MagicHash (MagicHash)
import PasswordService.Conf
import PdfToolsLambda.Conf
import Salesforce.Conf
import Session.SessionID as SessionID
import Templates
import User.Email
import User.Model

data Context = Context
    { _ctxmaybeuser           :: Maybe User
      -- ^ The logged in user. Is Nothing when there is no one logged in.
    , _ctxtime                :: UTCTime
      -- ^ The time of the request.
    , _ctxclientname          :: Maybe String
      -- ^ Client identification from header Client-Name or if that's
      -- missing: User-Agent.
    , _ctxclienttime          :: Maybe UTCTime
      -- ^ Client-local time when an action was performed
      -- (e.g. signing a document).
    , _ctxipnumber            :: IPAddress
      -- ^ The IP number of the client.
    , _ctxproduction          :: Bool
      -- ^ Is this server the production server?
    , _ctxcdnbaseurl          :: Maybe String
      -- ^ CDN base URL if one shouild be used.
    , _ctxtemplates           :: KontrakcjaTemplates
      -- ^ The set of templates to render text for the context's language.
    , _ctxglobaltemplates     :: KontrakcjaGlobalTemplates
      -- ^ All of the templates for all valid languages.
    , _ctxlang                :: Lang
      -- ^ The current context's language.
    , _ctxismailbackdooropen  :: Bool
    , _ctxmailnoreplyaddress  :: String
      -- ^ The "noreply" address used when sending email.
    , _ctxcgigrpconfig        :: Maybe CgiGrpConfig
    , _ctxgtconf              :: GuardTimeConf
      -- ^ GuardTime configuration.
    , _ctxmrediscache         :: Maybe R.Connection
    , _ctxfilecache           :: FileMemCache
    , _ctxxtoken              :: MagicHash
      -- ^ The XToken for combating CSRF.
    , _ctxadminaccounts       :: [Email]
    , _ctxsalesaccounts       :: [Email]
    , _ctxmaybepaduser        :: Maybe User
      -- ^ If we are logged in to the pad view.
    , _ctxusehttps            :: Bool
    , _ctxsessionid           :: SessionID
    , _ctxtrackjstoken        :: Maybe String
    , _ctxzendeskkey          :: Maybe String
    , _ctxmixpaneltoken       :: Maybe String
    , _ctxgatoken             :: Maybe String
    , _ctxhubspotconf         :: Maybe HubSpotConf
    , _ctxbrandeddomain       :: BrandedDomain
    , _ctxsalesforceconf      :: Maybe SalesforceConf
    , _ctxnetsconfig          :: Maybe NetsConfig
    , _ctxisapilogenabled     :: Bool
    , _ctxnetssignconfig      :: Maybe NetsSignConfig
    , _ctxpdftoolslambdaenv   :: PdfToolsLambdaEnv
    , _ctxpasswordserviceconf :: PasswordServiceConf
    , _ctxeidserviceconf      :: Maybe EIDServiceConf
    , _ctxmaybeapiuser        :: Maybe User
    -- ^ The user which was effectively used for API call (this
    -- includes api/frontend) This might be the user from session, if
    -- the OAuth authorization was missing.
    }

-- | 'anonymiseContext' changes given context into a one that does not
-- hold any user credentials.  Use this if your action requires
-- different form of authentication.
anonymiseContext :: Context -> Context
anonymiseContext ctx = ctx { _ctxmaybeuser    = Nothing
                           , _ctxmaybepaduser = Nothing
                           , _ctxmaybeapiuser = Nothing
                           , _ctxsessionid    = SessionID.tempSessionID }
