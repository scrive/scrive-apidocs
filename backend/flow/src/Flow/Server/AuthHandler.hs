module Flow.Server.AuthHandler
  ( authHandlerAccount
  , authHandlerInstanceUser
  , authHandlerInstanceUserHTML
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int
import Data.Text.Encoding
import Database.PostgreSQL.PQTypes hiding (JSON(..))
import Log.Class
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie (parseCookies)
import qualified Data.Text as T

import AccessControl.Model (GetRolesIncludingInherited(..))
import Auth.Model
import Auth.OAuth
import Auth.Session
import BrandedDomain.Model
import DB hiding (JSON(..))
import Flow.Error
import Flow.OrphanInstances ()
import Flow.Routes.Types (Host)
import Flow.Server.Cookies
import Flow.Server.Types
import Flow.Server.Utils
import Folder.Model (FolderGet(..))
import Folder.Types (FolderID, unsafeFolderID)
import User.Model
import UserGroup.Model (UserGroupGet(..))
import UserGroup.Types (UserGroupID, unsafeUserGroupID)
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model

-- "Account" users, i.e. users who have a Scrive account can authenticate
-- using OAuth or session cookies
-- TODO: Handle decodeUtf8 exceptions
authHandlerAccount :: RunLogger -> FlowConfiguration -> AuthHandler Request Account
authHandlerAccount runLogger flowConfiguration = mkAuthHandler handler
  where
    handler :: Request -> Handler Account
    handler req =
      runLogger
        . runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings
        . flip runReaderT (context flowConfiguration)
        $ do
            FlowContext { mainDomainUrl } <- ask
            let baseUrl = mkBaseUrl mainDomainUrl (isSecure req) (mHost req)

            (userId, userGroupId, folderId) <-
              case lookup "authorization" $ requestHeaders req of
                -- OAuth
                Just oauthHeader -> do
                  oauthTokens <-
                    either (throwAuthError OAuthHeaderParseFailureError . Just . String)
                           pure
                      $ parseOAuthAuthorization oauthHeader
                  logInfo_ ("Authenticating Account using OAuth: " <> showt oauthTokens)
                  maybeIds <- fmap unsafeToIds <$> authenticateToken oauthTokens
                  maybe (throwAuthError InvalidTokenError Nothing) pure maybeIds
                -- Session
                Nothing -> do
                  authData <- either (`throwAuthError` Nothing)
                                     pure
                                     (getSessionAuthData req)
                  logInfo_ ("Authenticating Account using a session: " <> showt authData)
                  maybeIds <- runMaybeT $ do
                    sessionId <- MaybeT
                      $ authenticateSession authData (cookieDomain $ mHost req)
                    MaybeT $ Model.selectUserIdsBySessionId sessionId
                  maybe (throwAuthError SessionCookieOrXTokenInvalidError Nothing)
                        pure
                        maybeIds

            -- TODO: fromJust is verboten - Handle the Nothing case with an error
            user   <- fmap fromJust . dbQuery . GetUserByID $ userId
            ug     <- fmap fromJust . dbQuery . UserGroupGet $ userGroupId
            folder <- fmap fromJust . dbQuery . FolderGet $ folderId
            roles  <- dbQuery $ GetRolesIncludingInherited user ug

            pure $ Account { user, userGroup = ug, folder, roles, baseUrl }

    parseOAuthAuthorization :: ByteString -> Either Text OAuthAuthorization
    parseOAuthAuthorization = parseParams . splitAuthorization . decodeUtf8

    unsafeToIds :: (Int64, Int64, Int64) -> (UserID, UserGroupID, FolderID)
    unsafeToIds (uid, ugid, fid) =
      (unsafeUserID uid, unsafeUserGroupID ugid, unsafeFolderID fid)

    -- For convenience
    throwAuthError errorName mLogData = do
      throwAuthErrorJSON Nothing errorName mLogData

-- "Instance" users, i.e. users who don't have an account but participate
-- in a flow process can only authenticate using session cookies.
authHandlerInstanceUser
  :: RunLogger -> FlowConfiguration -> AuthHandler Request InstanceUser
authHandlerInstanceUser runLogger flowConfiguration = mkAuthHandler handler'
  where handler' = instanceUserHandler runLogger flowConfiguration throwAuthErrorJSON

authHandlerInstanceUserHTML
  :: RunLogger -> FlowConfiguration -> AuthHandler Request InstanceUserHTML
authHandlerInstanceUserHTML runLogger flowConfiguration = mkAuthHandler handler'
  where
    handler' =
      fmap InstanceUserHTML
        . instanceUserHandler runLogger flowConfiguration throwAuthErrorHTML

instanceUserHandler
  :: RunLogger -> FlowConfiguration -> ErrorThrower -> Request -> Handler InstanceUser
instanceUserHandler runLogger flowConfiguration errorThrower req =
  runLogger
    . runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings
    . flip runReaderT (context flowConfiguration)
    $ do
        FlowContext { mainDomainUrl } <- ask
        let baseUrl = mkBaseUrl mainDomainUrl (isSecure req) (mHost req)
        bd              <- dbQuery $ GetBrandedDomainByURL baseUrl

        instanceSession <- do
          authData <- either (\err -> errorThrower (Just bd) err Nothing)
                             pure
                             (getSessionAuthData req)
          logInfo_ ("Authenticating InstanceUser using a session: " <> showt authData)
          mInstanceSession <- runMaybeT $ do
            sessionId <- MaybeT $ authenticateSession authData (cookieDomain $ mHost req)
            MaybeT $ Model.selectInstanceSession sessionId
          maybe (errorThrower (Just bd) SessionCookieOrXTokenInvalidError Nothing)
                pure
                mInstanceSession
        pure $ InstanceUser (instanceSession ^. #userName)
                            (instanceSession ^. #instanceId)
                            (instanceSession ^. #sessionId)

type ErrorThrower
  =  forall b m
   . ( MonadLog m
     , MonadError ServerError m
     , MonadReader FlowContext m
     , MonadDB m
     , MonadThrow m
     )
  => Maybe BrandedDomain
  -> AuthError
  -> Maybe Value
  -> m b
throwAuthErrorJSON :: ErrorThrower
throwAuthErrorJSON _ errorName mLogData = do
  logAttention "throwAuthErrorJSON"
    $ object ["errorName" .= showt errorName, "details" .= mLogData]
  throwAuthenticationError errorName

throwAuthErrorHTML :: ErrorThrower
throwAuthErrorHTML mBrandedDomain errorName mLogData = do
  logAttention "throwAuthErrorHTML"
    $ object ["errorName" .= showt errorName, "details" .= mLogData]
  bd <- maybe (dbQuery GetMainBrandedDomain) pure mBrandedDomain
  throwAuthenticationErrorHTML bd errorName

mHost :: Request -> Maybe Host
mHost = fmap decodeUtf8 . requestHeaderHost

-- Maybe XToken in the return type is Just when an xtoken is required for
-- the requested http method and it has been provided in the request.
getSessionAuthData :: Request -> Either AuthError (SessionCookieInfo, Maybe XToken)
getSessionAuthData req =
  let headers        = requestHeaders req
      mCookies       = parseCookies <$> lookup "cookie" headers
      mSessionCookie = mCookies >>= readCookie cookieNameSessionID
      mXToken        = do
        headerVal <- lookup headerNameXToken headers
        maybeRead . unQuote . decodeUtf8 $ headerVal
      unQuote = T.dropWhile (== '"') . T.dropWhileEnd (== '"')
  in  case (requestMethod req, mSessionCookie, mXToken) of
      -- GET requests are "safe" in terms of CSRF and require only a session cookie
      -- Non-GET (state changing) requests additionally require an xtoken
        ("GET", Just sessionCookie, _          ) -> Right (sessionCookie, Nothing)
        (_    , Nothing           , _          ) -> Left SessionCookieMissingError
        (_    , Just sessionCookie, Just xtoken) -> Right (sessionCookie, Just xtoken)
        (_    , Just _            , Nothing    ) -> Left XTokenMissingError
