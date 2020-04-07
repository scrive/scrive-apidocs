{-# LANGUAGE TemplateHaskell #-}
module Context.Internal where

import Log.Class
import Optics.TH
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
import SSO.Conf
import Templates
import User.Email
import User.Model

data Context = Context
    { maybeUser           :: Maybe User
      -- ^ The logged in user. Is Nothing when there is no one logged in.
    , time                :: UTCTime
      -- ^ The time of the request.
    , clientName          :: Maybe Text
      -- ^ Client identification from header Client-Name or if that's
      -- missing: User-Agent.
    , clientTime          :: Maybe UTCTime
      -- ^ Client-local time when an action was performed
      -- (e.g. signing a document).
    , ipAddr            :: IPAddress
      -- ^ The IP number of the client.
    , production          :: Bool
      -- ^ Is this server the production server?
    , cdnBaseUrl          :: Maybe Text
      -- ^ CDN base URL if one shouild be used.
    , templates           :: KontrakcjaTemplates
      -- ^ The set of templates to render text for the context's language.
    , globalTemplates     :: KontrakcjaGlobalTemplates
      -- ^ All of the templates for all valid languages.
    , lang                :: Lang
      -- ^ The current context's language.
    , isMailBackdoorOpen  :: Bool
    , mailNoreplyAddress  :: Text
      -- ^ The "noreply" address used when sending email.
    , cgiGrpConfig        :: Maybe CgiGrpConfig
    , gtConf              :: GuardTimeConf
      -- ^ GuardTime configuration.
    , redisCache          :: Maybe R.Connection
    , fileCache           :: FileMemCache
    , xToken              :: ~MagicHash
      -- ^ The XToken for combating CSRF (lazy as undefined in tests).
    , adminAccounts       :: [Email]
    , salesAccounts       :: [Email]
    , maybePadUser        :: Maybe User
      -- ^ If we are logged in to the pad view.
    , useHttps            :: Bool
    , sessionID           :: SessionID
    , trackJsToken        :: Maybe Text
    , zendeskKey          :: Maybe Text
    , mixpanelToken       :: Maybe Text
    , gaToken             :: Maybe Text
    , hubspotConf         :: Maybe HubSpotConf
    , brandedDomain       :: BrandedDomain
    , salesforceConf      :: Maybe SalesforceConf
    , netsConfig          :: Maybe NetsConfig
    , isApiLogEnabled     :: Bool
    , netsSignConfig      :: Maybe NetsSignConfig
    , pdfToolsLambdaEnv   :: PdfToolsLambdaEnv
    , passwordServiceConf :: PasswordServiceConf
    , eidServiceConf      :: Maybe EIDServiceConf
    , ssoConf             :: Maybe SSOConf
    , maybeApiUser        :: Maybe User
    -- ^ The user which was effectively used for API call (this includes
    -- api/frontend) This might be the user from session, if the OAuth
    -- authorization was missing.
    , postSignViewRedirectURL :: Text
    }

makeFieldLabelsWith noPrefixFieldLabels ''Context

-- | 'anonymiseContext' changes given context into a one that does not hold any
-- user credentials.  Use this if your action requires different form of
-- authentication.
anonymiseContext :: Context -> Context
anonymiseContext ctx = ctx { maybeUser    = Nothing
                           , maybePadUser = Nothing
                           , maybeApiUser = Nothing
                           , sessionID    = SessionID.tempSessionID
                           }
