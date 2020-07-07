{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Flow.Server.AuthHandler where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.Text.Encoding
import Database.PostgreSQL.PQTypes hiding (JSON(..))
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
import Flow.Server.Cookies
import Flow.Server.Types
import Folder.Model (FolderGet(..))
import Folder.Types (unsafeFolderID)
import User.Model
import UserGroup.Internal (unsafeUserGroupID)
import UserGroup.Model (UserGroupGet(..))
import qualified Flow.Model.InstanceSession as Model

-- "Account" users, i.e. users who have a Scrive account can authenticate
-- using OAuth or session cookies
-- TODO: Handle decodeUtf8 exceptions
authHandlerAccount :: FlowConfiguration -> AuthHandler Request Account
authHandlerAccount flowConfiguration = mkAuthHandler handler
  where
    handler :: Request -> Handler Account
    handler req =
      runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings $ do
        (userId, userGroupId, folderId) <-
          case lookup "authorization" $ requestHeaders req of
            -- OAuth
            Just oauthHeader -> do
              oauthTokens <- either (throwAuthError OAuthHeaderParseFailureError) pure
                $ parseOAuthAuthorization oauthHeader
              -- TODO use MonadLog
              liftIO
                . putStrLn
                $ ("Authenticating Account using OAuth: " <> show oauthTokens)
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
              -- TODO use MonadLog
              liftIO
                . putStrLn
                $ ("Authenticating Account using cookies: " <> show authCookies)
              maybeIds <- authenticateSession authCookies
              maybe
                (throwAuthError InvalidAuthCookiesError (show InvalidAuthCookiesError))
                pure
                maybeIds

        -- TODO: fromJust is verboten - Handle the Nothing case with an error
        user   <- fmap fromJust . dbQuery . GetUserByID $ unsafeUserID userId
        ug     <- fmap fromJust . dbQuery . UserGroupGet $ unsafeUserGroupID userGroupId
        folder <- fmap fromJust . dbQuery . FolderGet $ unsafeFolderID folderId
        roles  <- dbQuery $ GetRolesIncludingInherited user ug


        pure $ Account { user      = user
                       , userGroup = ug
                       , folder    = folder
                       , roles     = roles
                       , headers   = requestHeaders req
                       , baseUrl   = mkBaseUrl (isSecure req) (requestHeaderHost req)
                       }

    parseOAuthAuthorization :: ByteString -> Either Text OAuthAuthorization
    parseOAuthAuthorization = parseParams . splitAuthorization . decodeUtf8

mkBaseUrl :: Bool -> Maybe ByteString -> Text
mkBaseUrl isSecure mHost = protocol <> domain
  where
    protocol = if isSecure then "https://" else "http://"
    domain   = maybe "scrive.com" decodeUtf8 mHost

-- "Instance" users, i.e. users who don't have an account but participate
-- in a flow process can only authenticate using session cookies.
authHandlerInstanceUser :: FlowConfiguration -> AuthHandler Request InstanceUser
authHandlerInstanceUser flowConfiguration = mkAuthHandler handler
  where
    handler :: Request -> Handler InstanceUser
    handler req =
      runDBT (dbConnectionPool flowConfiguration) defaultTransactionSettings $ do
        instanceSession <- do
          authCookies <- maybe
            (throwAuthError AuthCookiesParseError (show AuthCookiesParseError))
            pure
            (getAuthCookies req)
          -- TODO use MonadLog
          liftIO
            . putStrLn
            $ ("Authenticating InstanceUser using cookies: " <> show authCookies)
          mInstanceSession <- runMaybeT $ do
            sessionId <- MaybeT $ getSessionIDByCookies authCookies
            MaybeT $ Model.selectInstanceSession sessionId
          maybe (throwAuthError InvalidAuthCookiesError (show InvalidAuthCookiesError))
                pure
                mInstanceSession
        pure
          $ InstanceUser (instanceSession ^. #userName) (instanceSession ^. #instanceId)

-- TODO handle the exception somehow
-- ... but don't put it into the response, it leaks internal information!
throwAuthError :: (MonadIO m, Show a, MonadError ServerError m) => AuthError -> a -> m b
throwAuthError errorName e = do
  -- TODO use MonadLog
  liftIO $ print e
  throwAuthenticationError errorName

getAuthCookies :: Request -> Maybe AuthCookies
getAuthCookies req = do
  cookies <- parseCookies <$> lookup "cookie" (requestHeaders req)
  readAuthCookies cookies
