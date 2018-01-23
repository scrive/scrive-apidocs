module Context.Internal where

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
import MagicHash (MagicHash)
import Salesforce.Conf
import Session.SessionID
import Templates
import User.Email
import User.Model
import qualified MemCache

data Context = Context
    { _ctxmaybeuser           :: Maybe User -- ^ The logged in user. Is Nothing when there is no one logged in.
    , _ctxtime                :: UTCTime -- ^ The time of the request.
    , _ctxclientname          :: Maybe String -- ^ Client identification from header Client-Name or if that's missing: User-Agent
    , _ctxclienttime          :: Maybe UTCTime -- ^ Client-local time when an action was performed (e.g. signing a document)
    , _ctxipnumber            :: IPAddress -- ^ The ip number of the client.
    , _ctxproduction          :: Bool -- ^ Is this server the production server?
    , _ctxcdnbaseurl          :: Maybe String -- ^ CDN base URL if one shouild be used
    , _ctxtemplates           :: KontrakcjaTemplates -- ^ The set of templates to render text for the ctxlang
    , _ctxglobaltemplates     :: KontrakcjaGlobalTemplates -- ^ All of the templates for all valid langs
    , _ctxlang                :: Lang -- ^ The current context lang
    , _ctxismailbackdooropen  :: Bool
    , _ctxmailnoreplyaddress  :: String -- ^ The "noreply" address used when sending email
    , _ctxcgigrpconfig        :: Maybe CgiGrpConfig
    , _ctxgtconf              :: GuardTimeConf -- ^ GuardTime configuration
    , _ctxmrediscache         :: Maybe R.Connection
    , _ctxfilecache           :: MemCache.MemCache FileID BS.ByteString -- ^
    , _ctxxtoken              :: MagicHash -- ^ The XToken for combatting CSRF
    , _ctxadminaccounts       :: [Email] -- ^
    , _ctxsalesaccounts       :: [Email] -- ^
    , _ctxmaybepaduser        :: Maybe User -- ^ If we are loged in to the pad view
    , _ctxusehttps            :: Bool
    , _ctxsessionid           :: SessionID
    , _ctxtrackjstoken        :: Maybe String
    , _ctxmixpaneltoken       :: Maybe String
    , _ctxgatoken             :: Maybe String
    , _ctxhubspotconf         :: Maybe HubSpotConf
    , _ctxbrandeddomain       :: BrandedDomain
    , _ctxsalesforceconf      :: Maybe SalesforceConf
    , _ctxnetsconfig          :: Maybe NetsConfig
    , _ctxisapilogenabled     :: Bool
    , _ctxnetssignconfig      :: Maybe NetsSignConfig
    }

-- | anonymousContext changes given context into one that does not hold any user details.
-- | use this if your action requires different for of authentication
anonymousContext :: Context -> Context
anonymousContext ctx = ctx { _ctxmaybeuser    = Nothing
                           , _ctxmaybepaduser = Nothing
                           , _ctxsessionid    = tempSessionID }
