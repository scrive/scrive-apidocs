module Flow.Server.Pages where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Maybe
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
import EID.Authentication.Model hiding (AuthenticationProvider)
import EID.EIDService.Types
import Flow.Aggregator
import Flow.Core.Type.AuthenticationConfiguration
import Flow.EID.Authentication
import Flow.EID.EIDService.Model
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
import User.Model.Query
import User.UserID
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import VersionTH (versionID)
import qualified Auth.Model as AuthModel
import qualified Flow.EID.EIDService.Types as FEET
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

    brandingDocId <- head <$> Model.selectDocumentIdsByInstanceId instanceId
    brandingUgwp  <- dbQuery $ UserGroupGetWithParentsByUserID authorUserId
    brandingHash  <- brandingAdler32 bd Nothing (Just $ ugwpUIWithID brandingUgwp)
    -- brandingHash is used for browser cache busting

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

        instanceOverviewVars = Html.InstanceOverviewPageVars
          { commonVars     = commonVars
          , kontraApiUrl   = "/api/v2"
          , flowApiUrl     = "/" <> flowPath
          , flowInstanceId = instanceId'
          }

    mInstanceAuthConfig <- runMaybeT $ do
      uac <- MaybeT $ Model.selectUserAuthenticationConfiguration instanceId userName
      MaybeT . pure $ instanceAuthConfig fullInstance uac

    mIdentifyViewVars <- case mInstanceAuthConfig of
      Just (authKind, authConfig) -> do
        needsToIdentify <- participantNeedsToIdentifyToView (authKind, authConfig)
                                                            instanceId
                                                            userName
                                                            sessionId
        if needsToIdentify
          then do
            appConfig <- mkIdentifyViewAppConfig cdnBaseUrl'
                                                 logoUrl
                                                 (fullInstance ^. #flowInstance)
                                                 userName
                                                 authorUserId
                                                 (authKind, authConfig)
                                                 sessionId
            pure . Just $ Html.IdentifyViewVars commonVars appConfig
          else pure Nothing
      Nothing -> pure Nothing

    case mIdentifyViewVars of
      -- TODO: Temporary: Remove bypassIdentify when the Elm identify-view is usable.
      Just identifyViewVars | not bypassIdentify ->
        return $ Html.renderIdentifyView identifyViewVars
      _ -> return $ Html.renderInstanceOverview instanceOverviewVars

participantNeedsToIdentifyToView
  :: (MonadDB m, MonadThrow m, MonadMask m)
  => (AuthenticationKind, AuthenticationConfiguration)
  -> InstanceId
  -> UserName
  -> SessionID
  -> m Bool
participantNeedsToIdentifyToView (authKind, authenticationConfiguration) instanceId userName sid
  = do
    let authProvider = provider authenticationConfiguration
    mAuthInDb <- selectFlowEidAuthentication instanceId userName authKind sid
    case mAuthInDb of
      Nothing -> do
        return True
      Just authInDb -> return . not . authProviderMatchesAuth authProvider $ authInDb

  where
    authProviderMatchesAuth SmsPin SMSPinAuthentication_{} = True
    authProviderMatchesAuth SmsPin     _ = False
    authProviderMatchesAuth (Onfido _) EIDServiceOnfidoAuthentication_{} = True
    authProviderMatchesAuth (Onfido _) _ = False

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

mkIdentifyViewAppConfig
  :: (MonadDB m, MonadThrow m)
  => Text
  -> Text
  -> Instance
  -> UserName
  -> UserID
  -> (AuthenticationKind, AuthenticationConfiguration)
  -> SessionID
  -> m Html.IdentifyViewAppConfig
mkIdentifyViewAppConfig cdnBaseUrl logoUrl instance_ userName authorId (authKind, authenticationConfiguration) sessionId
  = do
    mAuthor       <- dbQuery $ GetUserByID authorId

    -- TODO: Find a nicer way of getting the signatory link
    signatoryInfo <- find (\(userName', _, _) -> userName' == userName)
      <$> Model.selectSignatoryInfo (instance_ ^. #id)
    signatoryLink <- case signatoryInfo of
      Just (_, slid, did) -> dbQuery $ GetSignatoryLinkByID did slid
      Nothing -> unexpectedError "mkIdentifyViewAppConfig: signatory info not found!"

    -- The error message is based on the assumption that we only use EID Hub
    mEidTransaction <- dbQuery $ GetEIDServiceTransactionGuardSessionID
      sessionId
      (instance_ ^. #id)
      userName
      (EIDServiceAuthToView authKind)
    let mAuthErrorMessage = mEidTransaction >>= \t -> if isFailedEidTransaction t
          then Just "Authentication failed, please try again."
          else Nothing

    let mGenericEidServiceStartUrl =
          (\eidProvider -> "/" <> T.intercalate
              "/"
              [ "eid-service-flow"
              , "start"
              , toRedirectURLName eidProvider
              , toUrlPiece (instance_ ^. #id)
              , toUrlPiece userName
              ]
            )
            <$> toEIDServiceTransactionProvider (authenticationConfiguration ^. #provider)

        -- TODO: Use localisation
        welcomeText = "To see the documents verify your identity"

    pure $ Html.IdentifyViewAppConfig
      { cdnBaseUrl                = cdnBaseUrl
      , logoUrl                   = logoUrl
      , welcomeText               = welcomeText
      , entityTypeLabel           = "Flow"
      , entityTitle               = fromMaybe "" (instance_ ^. #title)
      , authenticationMethod      = toAuthenticationToViewMethod
                                      (authenticationConfiguration ^. #provider)
      , authorName                = maybe "" getFullName mAuthor
      , participantEmail          = getEmail signatoryLink
      , participantMaskedMobile   = maskedMobile 3 (getMobile signatoryLink)
      , genericEidServiceStartUrl = mGenericEidServiceStartUrl
      , smsPinSendUrl             = "" -- TODO
      , smsPinVerifyUrl           = "" -- TODO
      , errorMessage              = mAuthErrorMessage
      }
  where
    maskedMobile showLastN mobile =
      T.map (\c -> if c /= ' ' then '*' else c) (T.dropEnd showLastN mobile)
        <> T.takeEnd showLastN mobile
    isFailedEidTransaction transaction =
      FEET.estStatus transaction
        `elem` [ EIDServiceTransactionStatusFailed
               , EIDServiceTransactionStatusCompleteAndFailed
               ]

    toEIDServiceTransactionProvider
      :: AuthenticationProvider -> Maybe EIDServiceTransactionProvider
    toEIDServiceTransactionProvider = \case
      (Onfido _) -> Just EIDServiceTransactionProviderOnfido
      SmsPin     -> Nothing

    toAuthenticationToViewMethod :: AuthenticationProvider -> AuthenticationToViewMethod
    toAuthenticationToViewMethod = \case
      (Onfido (AuthenticationProviderOnfidoData Document)) ->
        OnfidoDocumentCheckAuthenticationToView
      (Onfido (AuthenticationProviderOnfidoData DocumentAndPhoto)) ->
        OnfidoDocumentAndPhotoCheckAuthenticationToView
      SmsPin -> SMSPinAuthenticationToView

instanceAuthConfig
  :: FullInstance
  -> UserAuthenticationConfiguration
  -> Maybe (AuthenticationKind, AuthenticationConfiguration)
instanceAuthConfig fullInstance uac = if isComplete $ instanceToAggregator fullInstance
  then uac ^. (#configurationData % #authenticationToViewArchived) >>= \conf ->
    pure (AuthenticationToViewArchived, conf)
  else uac ^. (#configurationData % #authenticationToView) >>= \conf ->
    pure (AuthenticationToView, conf)
