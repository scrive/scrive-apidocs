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
import Templates
import User.Email
import User.Model

data Context = Context
    { ctxMaybeUser           :: Maybe User
      -- ^ The logged in user. Is Nothing when there is no one logged in.
    , ctxTime                :: UTCTime
      -- ^ The time of the request.
    , ctxClientName          :: Maybe Text
      -- ^ Client identification from header Client-Name or if that's
      -- missing: User-Agent.
    , ctxClientTime          :: Maybe UTCTime
      -- ^ Client-local time when an action was performed
      -- (e.g. signing a document).
    , ctxIpNumber            :: IPAddress
      -- ^ The IP number of the client.
    , ctxProduction          :: Bool
      -- ^ Is this server the production server?
    , ctxCdnBaseUrl          :: Maybe Text
      -- ^ CDN base URL if one shouild be used.
    , ctxTemplates           :: KontrakcjaTemplates
      -- ^ The set of templates to render text for the context's language.
    , ctxGlobalTemplates     :: KontrakcjaGlobalTemplates
      -- ^ All of the templates for all valid languages.
    , ctxLang                :: Lang
      -- ^ The current context's language.
    , ctxIsMailBackdoorOpen  :: Bool
    , ctxMailNoreplyAddress  :: Text
      -- ^ The "noreply" address used when sending email.
    , ctxCgiGrpConfig        :: Maybe CgiGrpConfig
    , ctxGtConf              :: GuardTimeConf
      -- ^ GuardTime configuration.
    , ctxRedisCache          :: Maybe R.Connection
    , ctxFileCache           :: FileMemCache
    , ctxXToken              :: MagicHash
      -- ^ The XToken for combating CSRF.
    , ctxAdminAccounts       :: [Email]
    , ctxSalesAccounts       :: [Email]
    , ctxMaybePadUser        :: Maybe User
      -- ^ If we are logged in to the pad view.
    , ctxUseHttps            :: Bool
    , ctxSessionID           :: SessionID
    , ctxTrackJsToken        :: Maybe Text
    , ctxZendeskKey          :: Maybe Text
    , ctxMixpanelToken       :: Maybe Text
    , ctxGaToken             :: Maybe Text
    , ctxHubspotConf         :: Maybe HubSpotConf
    , ctxBrandedDomain       :: BrandedDomain
    , ctxSalesforceConf      :: Maybe SalesforceConf
    , ctxNetsConfig          :: Maybe NetsConfig
    , ctxIsApiLogEnabled     :: Bool
    , ctxNetsSignConfig      :: Maybe NetsSignConfig
    , ctxPdfToolsLambdaEnv   :: PdfToolsLambdaEnv
    , ctxPasswordServiceConf :: PasswordServiceConf
    , ctxEidServiceConf      :: Maybe EIDServiceConf
    , ctxMaybeApiUser        :: Maybe User
    -- ^ The user which was effectively used for API call (this
    -- includes api/frontend) This might be the user from session, if
    -- the OAuth authorization was missing.
    }

makeFieldLabelsWith noPrefixFieldLabels ''Context

-- | 'anonymiseContext' changes given context into a one that does not
-- hold any user credentials.  Use this if your action requires
-- different form of authentication.
anonymiseContext :: Context -> Context
anonymiseContext ctx = ctx { ctxMaybeUser    = Nothing
                           , ctxMaybePadUser = Nothing
                           , ctxMaybeApiUser = Nothing
                           , ctxSessionID    = SessionID.tempSessionID
                           }
