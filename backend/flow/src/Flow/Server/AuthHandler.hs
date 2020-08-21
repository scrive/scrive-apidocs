{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

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
import Data.Text.Encoding
import Database.PostgreSQL.PQTypes hiding (JSON(..))
import Log.Class
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie (parseCookies)

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
        . flip runReaderT (context flowConfiguration)
        $ do
            FlowContext { mainDomainUrl } <- ask
            let baseUrl = mkBaseUrl mainDomainUrl (isSecure req) (mHost req)

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

            pure $ Account { user, userGroup = ug, folder, roles, baseUrl }

    parseOAuthAuthorization :: ByteString -> Either Text OAuthAuthorization
    parseOAuthAuthorization = parseParams . splitAuthorization . decodeUtf8

    -- For convenience
    throwAuthError errorName e = do
      throwAuthErrorJSON Nothing errorName e

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
          authCookies <- maybe
            (errorThrower (Just bd) AuthCookiesParseError AuthCookiesParseError)
            pure
            (getAuthCookies req)
          logInfo_ ("Authenticating InstanceUser using cookies: " <> showt authCookies)
          mInstanceSession <- runMaybeT $ do
            sessionId <- MaybeT
              $ getSessionIDByCookies authCookies (cookieDomain $ mHost req)
            MaybeT $ Model.selectInstanceSession sessionId
          maybe (errorThrower (Just bd) InvalidAuthCookiesError InvalidAuthCookiesError)
                pure
                mInstanceSession
        pure
          $ InstanceUser (instanceSession ^. #userName) (instanceSession ^. #instanceId)

type ErrorThrower
  =  forall a b m
   . ( MonadLog m
     , MonadError ServerError m
     , MonadReader FlowContext m
     , MonadDB m
     , MonadThrow m
     , TextShow a
     )
  => Maybe BrandedDomain
  -> AuthError
  -> a
  -> m b

throwAuthErrorJSON :: ErrorThrower
throwAuthErrorJSON _ errorName e = do
  logAttention "throwAuthErrorJSON"
    $ object ["errorName" .= showt errorName, "e" .= showt e]
  throwAuthenticationError errorName

throwAuthErrorHTML :: ErrorThrower
throwAuthErrorHTML mBrandedDomain errorName e = do
  logAttention "throwAuthErrorHTML"
    $ object ["errorName" .= showt errorName, "e" .= showt e]
  bd <- maybe (dbQuery GetMainBrandedDomain) pure mBrandedDomain
  throwAuthenticationErrorHTML bd errorName

mHost :: Request -> Maybe Host
mHost = fmap decodeUtf8 . requestHeaderHost

getAuthCookies :: Request -> Maybe AuthCookies
getAuthCookies req = do
  cookies <- parseCookies <$> lookup "cookie" (requestHeaders req)
  readAuthCookies cookies
