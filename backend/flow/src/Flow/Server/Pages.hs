module Flow.Server.Pages where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Reader (ask)
import Database.PostgreSQL.PQTypes
import Log.Class
import Servant
import Text.Blaze.Html5 hiding (head)
import Web.Cookie
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import AppView (brandedPageTitle)
import Auth.MagicHash
import Auth.Session
import Auth.Session.SessionID
import BrandedDomain.Model
import Branding.Adler32
import DB (dbQuery, dbUpdate)
import Doc.DocControl
  ( AddDocumentSessionError(..), checkBeforeAddingDocumentSession
  )
import Doc.Model.Query
import Doc.Tokens.Model
import Doc.Types.SignatoryLink
import EID.Authentication.Model
import EID.EIDService.Types
import Flow.Aggregator
import Flow.EID.AuthConfig
import Flow.EID.Authentication
import Flow.Error
import Flow.HighTongue
import Flow.Id
import Flow.Model.Types
import Flow.OrphanInstances ()
import Flow.Routes.Pages
import Flow.Routes.Types
import Flow.Server.Cookies
import Flow.Server.Types
import Flow.Server.Utils
import UserGroup.Model
import UserGroup.Types
import VersionTH (versionID)
import qualified Auth.Model as AuthModel
import qualified Flow.Html as Html
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model

pages :: ServerT FlowPages AppM
pages = instanceOverview :<|> instanceOverviewMagicHash

-- brittany-disable-next-binding
instanceOverviewMagicHash
  :: InstanceId
  -> UserName
  -> MagicHash
  -> Maybe Cookies'
  -> Maybe Host
  -> IsSecure
  -> AppM
       ( Headers
           '[ Header "Location" Text
            , Header "Set-Cookie" SetCookie
            , Header "Set-Cookie" SetCookie
            ]
           NoContent
       )
instanceOverviewMagicHash instanceId userName hash mCookies mHost isSecure = do
  FlowContext { mainDomainUrl } <- ask
  let baseUrl = mkBaseUrl mainDomainUrl (isSecure == Secure) mHost
  bd <- dbQuery $ GetBrandedDomainByURL baseUrl

  void
    . fromMaybeM (throwAuthenticationErrorHTML bd InvalidInstanceAccessTokenError)
    $ Model.verifyInstanceAccessToken instanceId userName hash

  mSessionId <- getMSessionID mCookies mHost

  -- If we don't have an existing Kontrakcja session - start a new one
  -- and add sessionId and xtoken cookies to the response.
  -- TODO: It would be better to let Kontrakcja handle creating the session
  (sessionId, maybeAddCookieHeaders) <- case mSessionId of
    Just sessionId -> pure (sessionId, noHeader . noHeader)
    Nothing        -> do
      newAuthCookies <- AuthModel.insertNewSession (cookieDomain mHost) Nothing
      pure
        ( cookieSessionID (authCookieSession newAuthCookies)
        , addAuthCookieHeaders (isSecure == Secure) newAuthCookies
        )

  -- TODO: Move this to instanceOverview after auth to view check.
  -- Otherwise the user will be able to visit the signview before authenticating
  -- if they happen to know the doc id and slid.
  slids <- Model.selectSignatoryIdsByInstanceUser instanceId userName
  mapM_ (addDocumentSession bd sessionId) slids

  -- The Flow user's access token has been verified so insert an "instance session"
  -- which is used for cookie authentication in subsequent calls.
  Model.upsertInstanceSession sessionId instanceId userName

  pure . addHeader redirectUrl $ maybeAddCookieHeaders NoContent
  where
    redirectUrl = mkInstanceOverviewUrl instanceId userName
    addDocumentSession bd sid slid = do
      doc <- dbQuery $ GetDocumentBySignatoryLinkID slid -- Throws SomeDBExtraException
      case checkBeforeAddingDocumentSession doc slid of
        Just err -> do
          logInfo_ $ "Unable to add document session: " <> showt err
          case err of
            DocumentNotAccessibleBySignatories _ -> throwProcessFailedHTML bd
            _ -> throwUnableToAddDocumentSessionHTML bd
        Nothing -> dbUpdate $ AddDocumentSession sid slid

instanceOverview
  :: InstanceUserHTML
  -> InstanceId
  -> UserName
  -> Bool
  -> Maybe Cookies'
  -> Maybe Host
  -> IsSecure
  -> AppM Html
instanceOverview (InstanceUserHTML InstanceUser {..}) instanceId' _ bypassIdentify mCookies mHost isSecure
  = do
    FlowContext { mainDomainUrl, cdnBaseUrl, production } <- ask
    let baseUrl = mkBaseUrl mainDomainUrl (isSecure == Secure) mHost
    bd <- dbQuery $ GetBrandedDomainByURL baseUrl

    when (instanceId /= instanceId') $ throwAuthenticationErrorHTML bd AccessControlError

    sessionId <-
      getMSessionID mCookies mHost
      -- The error handling is not necessary, this is authenticated endpoint already.
      -- When there is a better way to get sessionId, then it will be possible to fix this.
        >>= maybe (throwAuthenticationErrorHTML bd InvalidAuthCookiesError) return

    -- We know the instance exists because of the authenticated InstanceUser
    fullInstance <- fromJust <$> Model.selectFullInstance instanceId
    let authorUserId = fullInstance ^. #template % #userId

    brandingDocId    <- head <$> Model.selectDocumentIdsByInstanceId instanceId
    brandingUgwp     <- dbQuery $ UserGroupGetWithParentsByUserID authorUserId
    brandingHash     <- brandingAdler32 bd Nothing (Just $ ugwpUIWithID brandingUgwp)
    -- brandingHash is used for browser cache busting

    -- TODO: Temporary: These Ids are here just to make IdentifyView elm app run.
    -- It hasn't been modified to accept Flow-specific Ids yet.
    identifyViewSlid <-
      head <$> Model.selectSignatoryIdsByInstanceUser instanceId userName
    identifyViewDocId <- head <$> Model.selectDocumentIdsByInstanceId instanceId

    let cdnBaseUrl'    = fromMaybe "" cdnBaseUrl

        brandingCssUrl = T.intercalate
          "/"
          [ cdnBaseUrl'
          , "document_signview_branding"
          , showt (bd ^. #id)
          , showt brandingDocId
          , brandingHash <> "-branding.css"
          ]

        logoUrl = T.intercalate
          "/"
          [ cdnBaseUrl'
          , "signview_logo"
          , showt (bd ^. #id)
          , showt brandingDocId
          , brandingHash
          ]

        mainCssUrl = if production
          then cdnBaseUrl' <> "/" <> versionCode <> ".signview-all-styling-minified.css"
          else "/less/signview-less-compiled.css"

        versionCode = T.decodeUtf8 . B16.encode $ BS.fromString versionID

        commonVars  = Html.CommonPageVars
          { cdnBaseUrl     = cdnBaseUrl'
          , mainCssUrl     = mainCssUrl
          , brandingCssUrl = brandingCssUrl
          , logoUrl        = logoUrl
          , versionCode    = versionCode
          , browserTitle   = brandedPageTitle bd (Just $ ugwpUI brandingUgwp)
          }

        identifyViewVars = Html.IdentifyViewVars { commonVars      = commonVars
                                                 , brandedDomainId = bd ^. #id
                                                 , brandingHash    = brandingHash
                                                 , documentId      = identifyViewDocId       -- TODO: Temporary
                                                 , signatoryLinkId = identifyViewSlid   -- TODO: Temporary
                                                 , flowInstanceId  = instanceId'
                                                 }

        instanceOverviewVars = Html.InstanceOverviewPageVars
          { commonVars     = commonVars
          , kontraApiUrl   = "/api/v2"
          , flowApiUrl     = "/" <> flowPath
          , flowInstanceId = instanceId'
          }

    mUserAuthConfig <- Model.selectUserAuthConfig instanceId userName
    needsToIdentify <- participantNeedsToIdentifyToView mUserAuthConfig
                                                        fullInstance
                                                        userName
                                                        sessionId

    -- let needsToIdentify = isJust $ do
    --       uac <- mUserAuthConfig
    --       uac ^. #authToView <|> uac ^. #authToViewArchived

    -- TODO: Temporary: Remove bypassIdentify when the Elm identify-view is usable.
    if needsToIdentify && not bypassIdentify
      then return $ Html.renderIdentifyView identifyViewVars
      else return $ Html.renderInstanceOverview instanceOverviewVars

participantNeedsToIdentifyToView
  :: (MonadDB m, MonadThrow m, MonadMask m)
  => Maybe UserAuthConfig
  -> FullInstance
  -> UserName
  -> SessionID
  -> m Bool
participantNeedsToIdentifyToView mUserAuthConfig fullInstance userName sid =
  if isComplete $ instanceToAggregator fullInstance
    then check AuthenticationToViewArchived (^. #authToViewArchived)
    else check AuthenticationToView (^. #authToView)
  where
    instanceId = fullInstance ^. #flowInstance % #id
    check authKind getParticipantAuthConf =
      case mUserAuthConfig >>= getParticipantAuthConf of
        Nothing       -> return False
        Just authConf -> do
          let authProvider = provider authConf
          mAuthInDb <- selectFlowEidAuthentication instanceId userName authKind sid
          case mAuthInDb of
            Nothing -> do
              -- TODO: Temporary: This is a dummy way of authenticating
              -- call me first time: no value in flow_eid_authentications => put something there and serve identifyview
              -- call me again: some value in flow_eid_authentications => serve flow overview
              putDummyValueIntoFlowEAuthentication authKind
              return True
            Just authInDb ->
              return . not . authProviderMatchesAuth authProvider $ authInDb

    putDummyValueIntoFlowEAuthentication authKind =
      updateFlowEidAuthentication instanceId userName authKind sid
        $ EIDServiceSEBankIDAuthentication_ EIDServiceSEBankIDAuthentication
            { eidServiceSEBankIDSignatoryName           = "Tester 'Dummy' Testovicz"
            , eidServiceSEBankIDSignatoryPersonalNumber = "123456"
            , eidServiceSEBankIDSignatoryIP             = "0.0.0.0"
            , eidServiceSEBankIDSignature               = "dummySignature"
            , eidServiceSEBankIDOcspResponse            = "dummyOcspResponse"
            }

    -- TODO: Temporary: Use Util.SignatoryLinkUtils:authViewMatchesAuth
    authProviderMatchesAuth _ _ = True

getMSessionID
  :: (MonadDB m, MonadThrow m, MonadTime m)
  => Maybe Cookies'
  -> Maybe Host
  -> m (Maybe SessionID)
getMSessionID mCookies mHost = do
  let mAuthCookies = do
        Cookies' cookies <- mCookies
        readAuthCookies cookies
  case mAuthCookies of
    Just authCookies -> AuthModel.getSessionIDByCookies authCookies (cookieDomain mHost)
    Nothing          -> pure Nothing
