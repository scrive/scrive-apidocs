module Flow.Server.Pages where

import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Reader (ask)
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
import BrandedDomain.Model
import Branding.Adler32
import DB (dbQuery, dbUpdate)
import Doc.DocControl
  ( AddDocumentSessionError(..), checkBeforeAddingDocumentSession
  )
import Doc.Model.Query
import Doc.Tokens.Model
import Flow.Error
import Flow.HighTongue
import Flow.Id
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

  mSessionId <- case getAuthCookies of
    Just authCookies -> AuthModel.getSessionIDByCookies authCookies (cookieDomain mHost)
    Nothing          -> pure Nothing

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

  -- TODO: We should only add doc sessions for documents that the participant can act on at
  -- the current stage rather than all the documents that the participant is a signatory for.
  -- It should be ok for now since we start all documents at once.
  slids <- Model.selectSignatoryIdsByInstanceUser instanceId userName
  mapM_ (addDocumentSession bd sessionId) slids

  -- The Flow user's access token has been verified so insert an "instance session"
  -- which is used for cookie authentication in subsequent calls.
  Model.upsertInstanceSession sessionId instanceId userName

  pure . addHeader redirectUrl $ maybeAddCookieHeaders NoContent
  where
    getAuthCookies = do
      Cookies' cookies <- mCookies
      readAuthCookies cookies
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

  mFullInstance <- Model.selectFullInstance instanceId
  -- We know the instance exists because of the authenticated InstanceUser
  let authorUserId = fromJust mFullInstance ^. #template % #userId

  brandingDocId <- head <$> Model.selectDocumentIdsByInstanceId instanceId
  brandingUgwp  <- dbQuery $ UserGroupGetWithParentsByUserID authorUserId

  -- Used for browser cache busting
  brandingHash  <- brandingAdler32 bd Nothing (Just $ ugwpUIWithID brandingUgwp)

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

  return . Html.renderInstanceOverview $ Html.InstanceOverviewPageVars
    { commonVars     = Html.CommonPageVars
                         { cdnBaseUrl     = cdnBaseUrl'
                         , mainCssUrl     = mainCssUrl
                         , brandingCssUrl = brandingCssUrl
                         , logoUrl        = logoUrl
                         , versionCode    = versionCode
                         , browserTitle = brandedPageTitle bd (Just $ ugwpUI brandingUgwp)
                         }
    , kontraApiUrl   = "/api/v2"
    , flowApiUrl     = "/" <> flowPath
    , flowInstanceId = instanceId'
    }
