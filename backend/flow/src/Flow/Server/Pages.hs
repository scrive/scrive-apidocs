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
import Doc.Types.SignatoryLink (AuthenticationKind)
import EID.Authentication.Model hiding (AuthenticationProvider)
import EID.EIDService.Types
import Flow.Aggregator (failureStageName)
import Flow.Core.Type.AuthenticationConfiguration
import Flow.EID.Authentication
import Flow.EID.EIDService.Model
import Flow.Error
import Flow.HighTongue
import Flow.Html (AuthenticationToViewFlowMethod(..))
import Flow.Id
import Flow.Model.Types
import Flow.OrphanInstances ()
import Flow.Routes.Pages
import Flow.Routes.Types
import Flow.Server.Cookies
import Flow.Server.Types
import Flow.Server.Utils
import Flow.Utils
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

  mSessionId <- getMSessionID

  -- If we don't have an existing Kontrakcja session - start a new one
  -- and add sessionId and xtoken cookies to the response.
  -- TODO: It would be better to let Kontrakcja handle creating the session
  (sessionId, maybeAddCookieHeaders) <- case mSessionId of
    Just sessionId -> pure (sessionId, noHeader . noHeader)
    Nothing        -> do
      (newSessionCookieInfo, newXToken) <- AuthModel.insertNewSession (cookieDomain mHost) Nothing
      pure
        ( cookieSessionID newSessionCookieInfo
        , addAuthCookieHeaders (isSecure == Secure) (newSessionCookieInfo, newXToken)
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
    getMSessionID = do
      let mSessionCookie = (fromCookies' <$> mCookies) >>= readCookie cookieNameSessionID
      case mSessionCookie of
        Just sessionCookie -> AuthModel.authenticateSession (sessionCookie, Nothing) (cookieDomain mHost)
        Nothing       -> pure Nothing
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
  :: InstanceUserHTML -> InstanceId -> UserName -> Maybe Host -> IsSecure -> AppM Html
instanceOverview (InstanceUserHTML InstanceUser {..}) instanceId' _ mHost isSecure = do
  FlowContext { mainDomainUrl, cdnBaseUrl, production } <- ask
  let baseUrl = mkBaseUrl mainDomainUrl (isSecure == Secure) mHost
  bd <- dbQuery $ GetBrandedDomainByURL baseUrl

  when (instanceId /= instanceId') $ throwAuthenticationErrorHTML bd AccessControlError

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

      instanceOverviewVars = Html.InstanceOverviewPageVars { commonVars     = commonVars
                                                           , kontraApiUrl   = "/api/v2"
                                                           , flowApiUrl = "/" <> flowPath
                                                           , flowInstanceId = instanceId'
                                                           }

  mUserAuthConfig <- Model.selectUserAuthenticationConfiguration instanceId userName
  let mAuthKindAndConfig =
        mUserAuthConfig >>= getAuthenticationKindAndConfiguration fullInstance

  mIdentifyViewVars <- case mAuthKindAndConfig of
    Just (authenticationKind, authenticationConfig) -> do
      needsToIdentify <- participantNeedsToIdentifyToView
        (authenticationKind, authenticationConfig)
        instanceId
        userName
        sessionId
      if needsToIdentify
        then do
          appConfig <- mkIdentifyViewAppConfig
            cdnBaseUrl'
            logoUrl
            (fullInstance ^. #flowInstance)
            userName
            authorUserId
            (authenticationKind, authenticationConfig)
            sessionId
          pure . Just $ Html.IdentifyViewVars commonVars appConfig
        else pure Nothing
    Nothing -> pure Nothing

  case mIdentifyViewVars of
    Just identifyViewVars -> return $ Html.renderIdentifyView identifyViewVars
    Nothing               -> return $ Html.renderInstanceOverview instanceOverviewVars

participantNeedsToIdentifyToView
  :: (MonadDB m, MonadThrow m, MonadMask m)
  => (AuthenticationKind, AuthenticationConfiguration)
  -> InstanceId
  -> UserName
  -> SessionID
  -> m Bool
participantNeedsToIdentifyToView (authenticationKind, authenticationConfig) instanceId userName sid
  = do
    let authProvider = provider authenticationConfig
    mAuthInDb <- selectFlowEidAuthentication instanceId userName authenticationKind sid
    case mAuthInDb of
      Nothing -> do
        return True
      Just authInDb -> return . not . authProviderMatchesAuth authProvider $ authInDb

  where
    authProviderMatchesAuth SmsOtp EIDServiceSmsOtpAuthentication_{} = True
    authProviderMatchesAuth SmsOtp     _ = False
    authProviderMatchesAuth (Onfido _) EIDServiceOnfidoAuthentication_{} = True
    authProviderMatchesAuth (Onfido _) _ = False

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
mkIdentifyViewAppConfig cdnBaseUrl logoUrl instance_ userName authorId (authenticationKind, authenticationConfig) sessionId
  = do
    mAuthor             <- dbQuery $ GetUserByID authorId

    (did, slid)         <- findFirstSignatoryLink (instance_ ^. #id) userName
    signatoryLink       <- dbQuery $ GetSignatoryLinkByID did slid

    maxFailuresExceeded <- checkAuthMaxFailuresExceeded
      (instance_ ^. #id)
      userName
      (authenticationKind, authenticationConfig)

    mEidTransaction <- dbQuery $ GetEIDServiceTransactionGuardSessionID
      sessionId
      (instance_ ^. #id)
      userName
      (EIDServiceAuthToView authenticationKind)

    let lastTransactionFailed = maybe False isFailedEidTransaction mEidTransaction

        -- The error messages are based on the assumption that we only use EID Hub
        mErrorMessage         = if
          | maxFailuresExceeded -> Just
            "Maximum number of failed authentications has been reached."
          | lastTransactionFailed -> Just "Authentication failed, please try again."
          | otherwise -> Nothing

        flowRejected = instance_ ^. #currentState == failureStageName

        -- TODO: Use localisation
        welcomeText  = if
          | maxFailuresExceeded -> "Authentication failed"
          | flowRejected        -> "Flow rejected"
          | otherwise           -> "To see the documents verify your identity"

        mGenericEidServiceStartUrl =
          (\eidProvider -> "/" <> T.intercalate
              "/"
              [ "eid-service-flow"
              , "start"
              , toRedirectURLName eidProvider
              , toUrlPiece (instance_ ^. #id)
              , toUrlPiece userName
              ]
            )
            <$> toEIDServiceTransactionProvider (authenticationConfig ^. #provider)

        rejectionRejectUrl = "/" <> T.intercalate
          "/"
          [flowPath, "instances", toUrlPiece (instance_ ^. #id), "reject"]

    pure $ Html.IdentifyViewAppConfig
      { cdnBaseUrl                = cdnBaseUrl
      , logoUrl                   = logoUrl
      , welcomeText               = welcomeText
      , entityTypeLabel           = "Flow"
      , entityTitle               = fromMaybe "" (instance_ ^. #title)
      , authenticationMethod      = toAuthenticationToViewMethod
                                      (authenticationConfig ^. #provider)
      , authorName                = maybe "" getFullName mAuthor
      , participantEmail          = getEmail signatoryLink
      , participantMaskedMobile   = maskedMobile 3 (getMobile signatoryLink)
      , genericEidServiceStartUrl = mGenericEidServiceStartUrl
      , smsPinSendUrl             = "" -- TODO
      , smsPinVerifyUrl           = "" -- TODO
      , rejectionRejectUrl        = rejectionRejectUrl
      , rejectionAlreadyRejected  = flowRejected
      , errorMessage              = mErrorMessage
      , maxFailuresExceeded       = maxFailuresExceeded
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
      SmsOtp     -> Just EIDServiceTransactionProviderSmsOtp

    toAuthenticationToViewMethod
      :: AuthenticationProvider -> AuthenticationToViewFlowMethod
    toAuthenticationToViewMethod = \case
      (Onfido (AuthenticationProviderOnfidoData Document _)) ->
        OnfidoDocumentCheckAuthenticationToView
      (Onfido (AuthenticationProviderOnfidoData DocumentAndPhoto _)) ->
        OnfidoDocumentAndPhotoCheckAuthenticationToView
      SmsOtp -> SmsOtpAuthenticationToView
