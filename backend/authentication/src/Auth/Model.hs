module Auth.Model where

import Control.Monad.Catch
import Control.Monad.Time
import Crypto.RNG
import Data.Int
import Data.Time.Clock
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder

import Auth.MagicHash
import Auth.OAuth
import Auth.Session
import Auth.Session.Constant
import Auth.Session.SessionID

-- TODO use UserId and UserGroupId types, once they are available in some ID component
authenticateToken
  :: (MonadDB m, MonadThrow m) => OAuthAuthorization -> m (Maybe (Int64, Int64, Int64))
authenticateToken (OAuthAuthorization token secret atoken asecret) = do
  runQuery_ $ rawSQL
    (  "SELECT u.id, ug.id, u.home_folder_id "
    <> "FROM oauth_access_token a "
    <> "JOIN oauth_privilege p ON p.access_token_id = a.id "
    <> "JOIN oauth_api_token t ON a.api_token_id    = t.id "
    <> "JOIN users u           ON t.user_id         = u.id "
    <> "JOIN user_groups ug    ON u.user_group_id   = ug.id "
    <> "WHERE t.id = $1 AND t.api_token = $2 AND t.api_secret = $3 "
    <> "AND a.id = $4 AND a.access_token = $5 AND a.access_secret = $6 "
    <> "AND (p.privilege = $7 OR p.privilege = $8) LIMIT 1"
    )
    ( atID token
    , atToken token
    , secret
    , atID atoken
    , atToken atoken
    , asecret
    , APIPersonal
    , APIFullAccess
    )
  fetchMaybe identity

authenticateSession
  :: (MonadDB m, MonadThrow m, MonadTime m)
  => (SessionCookieInfo, Maybe XToken)
  -> Text
  -> m (Maybe SessionID)
authenticateSession (SessionCookieInfo {..}, mXToken) domain = do
  now <- currentTime
  runQuery_ . sqlSelect "sessions" $ do
    sqlResult "id"
    sqlWhereEq "id"    cookieSessionID
    sqlWhereEq "token" cookieSessionToken
    whenJust mXToken $ sqlWhereEq "csrf_token"
    sqlWhereEq "domain" domain
    sqlWhere $ "expires >=" <?> now
  fetchMaybe runIdentity

insertNewSession
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m)
  => Text
  -> Maybe Int64
  -> m (SessionCookieInfo, XToken)
insertNewSession domain mUserId = do
  sessionToken :: MagicHash <- random
  xToken :: MagicHash <- random
  now                 <- currentTime
  let expires = secondsAfter timeoutSecs now
  runQuery_ . sqlInsert "sessions" $ do
    mapM_ sqlResult ["id", "token", "csrf_token"]
    sqlSet "user_id"     mUserId
    sqlSet "pad_user_id" (Nothing :: Maybe Int64)
    sqlSet "token"       sessionToken
    sqlSet "csrf_token"  xToken
    sqlSet "domain"      domain
    sqlSet "expires"     expires
  fetchOne fetchCookieInfoAndXToken
  where
    timeoutSecs  = defaultSessionTimeoutSecs :: Int64
    secondsAfter = addUTCTime . fromIntegral
    fetchCookieInfoAndXToken (sessionID, sessionToken, xToken) =
      (SessionCookieInfo sessionID sessionToken, xToken)

