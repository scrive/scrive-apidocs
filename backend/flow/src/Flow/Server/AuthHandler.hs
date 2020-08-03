{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Flow.Server.AuthHandler
  ( authHandlerAccount
  , authHandlerInstanceUser
  , authHandlerInstanceUserHTML
  ) where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.Text.Encoding
import Database.PostgreSQL.PQTypes hiding (JSON(..))
import Log.Class
import Log.Monad
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie (parseCookies)

import AccessControl.Model (GetRolesIncludingInherited(..))
import Auth.Model
import Auth.OAuth
import Auth.Session
import DB hiding (JSON(..))
import Flow.Error
import Flow.OrphanInstances ()
import Flow.Routes.Types (Host)
import Flow.Server.Cookies
import Flow.Server.Types
import Flow.Server.Utils
import Folder.Model (FolderGet(..))
import Folder.Types (unsafeFolderID)
import User.Model
import UserGroup.Internal (unsafeUserGroupID)
import UserGroup.Model (UserGroupGet(..))
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
        $ do
            (userId, userGroupId, folderId) <-
              case lookup "authorization" $ requestHeaders req of
                -- OAuth
                Just oauthHeader -> do
                  oauthTokens <- either (throwAuthError OAuthHeaderParseFailureError) pure
                    $ parseOAuthAuthorization oauthHeader
                  logInfo_ ("Authenticating Account using OAuth: " <> showt oauthTokens)
                  maybeIds <- authenticateToken oauthTokens
                  maybe (throwAuthError InvalidTokenError (show InvalidTokenError))
                        pure
                        maybeIds
                -- Session cookies
                Nothing -> do
                  authCookies <- maybe
                    (throwAuthError AuthCookiesParseError (show AuthCookiesParseError))
                    pure
                    (getAuthCookies req)
                  logInfo_ ("Authenticating Account using cookies: " <> showt authCookies)
                  maybeIds <- authenticateSession authCookies (cookieDomain $ mHost req)
                  maybe
                    (throwAuthError InvalidAuthCookiesError (show InvalidAuthCookiesError)
                    )
                    pure
                    maybeIds

            -- TODO: fromJust is verboten - Handle the Nothing case with an error
            user   <- fmap fromJust . dbQuery . GetUserByID $ unsafeUserID userId
            ug <- fmap fromJust . dbQuery . UserGroupGet $ unsafeUserGroupID userGroupId
            folder <- fmap fromJust . dbQuery . FolderGet $ unsafeFolderID folderId
            roles  <- dbQuery $ GetRolesIncludingInherited user ug
            let domainUrl = mainDomainUrl (flowConfiguration :: FlowConfiguration)
            let baseUrl   = mkBaseUrl domainUrl (isSecure req) (mHost req)

            pure $ Account { user
                           , userGroup = ug
                           , folder
                           , roles
                           , headers   = requestHeaders req
                           , baseUrl
                           }

    parseOAuthAuthorization :: ByteString -> Either Text OAuthAuthorization
    parseOAuthAuthorization = parseParams . splitAuthorization . decodeUtf8

-- "Instance" users, i.e. users who don't have an account but participate
-- in a flow process can only authenticate using session cookies.
authHandlerInstanceUser
  :: RunLogger -> FlowConfiguration -> AuthHandler Request InstanceUser
authHandlerInstanceUser runLogger flowConfiguration = mkAuthHandler handler'
  where handler' = instanceUserHandler runLogger flowConfiguration throwAuthError

authHandlerInstanceUserHTML
  :: RunLogger -> FlowConfiguration -> AuthHandler Request InstanceUserHTML
authHandlerInstanceUserHTML runLogger flowConfiguration = mkAuthHandler handler'
  where
    handler' =
      fmap InstanceUserHTML
        . instanceUserHandler runLogger flowConfiguration throwAuthErrorHTML

instanceUserHandler
  :: RunLogger
  -> FlowConfiguration
  -> (forall a . AuthError -> AuthError -> DBT (LogT Handler) a)
  -> Request
  -> Handler InstanceUser
instanceUserHandler runLogger flowConfiguration errorThrower req =
  runLogger . runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings $ do
    instanceSession <- do
      authCookies <- maybe (errorThrower AuthCookiesParseError AuthCookiesParseError)
                           pure
                           (getAuthCookies req)
      logInfo_ ("Authenticating InstanceUser using cookies: " <> showt authCookies)
      mInstanceSession <- runMaybeT $ do
        sessionId <- MaybeT $ getSessionIDByCookies authCookies (cookieDomain $ mHost req)
        MaybeT $ Model.selectInstanceSession sessionId
      maybe (errorThrower InvalidAuthCookiesError InvalidAuthCookiesError)
            pure
            mInstanceSession
    pure $ InstanceUser (instanceSession ^. #userName) (instanceSession ^. #instanceId)

mHost :: Request -> Maybe Host
mHost req = decodeUtf8 <$> requestHeaderHost req

-- TODO handle the exception somehow
-- ... but don't put it into the response, it leaks internal information!
throwAuthError
  :: (MonadIO m, MonadLog m, Show a, MonadError ServerError m) => AuthError -> a -> m b
throwAuthError errorName e = do
  logAttention ("throwAuthError: " <> showt errorName) $ show e
  throwAuthenticationError errorName

-- TODO handle the exception somehow
-- ... but don't put it into the response, it leaks internal information!
throwAuthErrorHTML
  :: (MonadIO m, MonadLog m, Show a, MonadError ServerError m) => AuthError -> a -> m b
throwAuthErrorHTML errorName e = do
  logAttention ("throwAuthErrorHTML: " <> showt errorName) $ show e
  throwAuthenticationErrorHTML errorName

getAuthCookies :: Request -> Maybe AuthCookies
getAuthCookies req = do
  cookies <- parseCookies <$> lookup "cookie" (requestHeaders req)
  readAuthCookies cookies
