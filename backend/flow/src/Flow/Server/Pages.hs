{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Server.Pages where

import Control.Monad.Except
import Control.Monad.Extra (fromMaybeM)
import Log.Class
import Servant
import Web.Cookie
import qualified Data.Text as T

import Auth.MagicHash
import Auth.Session
import DB (dbQuery, dbUpdate)
import Doc.DocControl (checkBeforeAddingDocumentSession)
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
import qualified Auth.Model as AuthModel
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
  _ <-
    fromMaybeM (throwAuthenticationError InvalidInstanceAccessTokenError)
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
      pure ( cookieSessionID (authCookieSession newAuthCookies)
           , addAuthCookieHeaders (isSecure == Secure) newAuthCookies
           )

  -- TODO: We should only add doc sessions for documents that the participant can act on at
  -- the current stage rather than all the documents that the participant is a signatory for.
  -- It should be ok for now since we start all documents at once.
  slids <- Model.selectSignatoryIdsByInstanceUser instanceId userName
  mapM_ (addDocumentSession sessionId) slids

  -- The Flow user's access token has been verified so insert an "instance session"
  -- which is used for cookie authentication in subsequent calls.
  Model.upsertInstanceSession sessionId instanceId userName

  let response = addHeader redirectUrl $ maybeAddCookieHeaders NoContent
  pure response

  where
    getAuthCookies = do
      Cookies' cookies <- mCookies
      readAuthCookies cookies
    redirectUrl = "/"
      <> T.intercalate "/" [flowPath, "overview", toUrlPiece instanceId, toUrlPiece userName]
    addDocumentSession sid slid = do
      doc <- dbQuery $ GetDocumentBySignatoryLinkID slid -- Throws SomeDBExtraException
      case checkBeforeAddingDocumentSession doc slid of
        Just err -> do
          logInfo_ $ "Unable to add document session: " <> showt err
          throwError $ err500 { errBody = "Error: Unable to add a document session." }
        Nothing -> dbUpdate $ AddDocumentSession sid slid

-- Instance overview page
-- TODO: Implement the overview page
instanceOverview :: InstanceUser -> InstanceId -> UserName -> AppM Text
instanceOverview InstanceUser {..} instanceId' _ = do
  when (instanceId /= instanceId') $ throwAuthenticationError AccessControlError

  return "<html><body><h1> Flow overview page </h1></body></html>"
