module Auth.Model where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import Database.PostgreSQL.PQTypes

import Auth.OAuth
import Auth.Session

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
    <> "WHERE t.id = $1 AND t.api_token = $2 AND t.api_secret = $3 AND a.id = $4 AND a.access_token = $5 AND a.access_secret = $6 AND p.privilege = $7 LIMIT 1"
    )
    (atID token, atToken token, secret, atID atoken, atToken atoken, asecret, apiPersonal)
  fetchMaybe identity
  where apiPersonal = 0 :: Int64 -- APIPersonal

-- TODO use UserId and UserGroupId types, once they are available in some ID component
authenticateSession
  :: (MonadDB m, MonadThrow m, MonadTime m)
  => (SessionCookieInfo, XToken)
  -> m (Maybe (Int64, Int64, Int64))
authenticateSession (SessionCookieInfo {..}, xtoken) = do
  now <- currentTime
  runQuery_ $ rawSQL
    (  "SELECT u.id, ug.id, u.home_folder_id "
    <> "FROM sessions s "
    <> "JOIN users u           ON s.user_id         = u.id "
    <> "JOIN user_groups ug    ON u.user_group_id   = ug.id "
    <> "WHERE s.id = $1 AND s.token = $2 AND s.csrf_token = $3 AND s.expires >= $4 LIMIT 1"
    )
    (cookieSessionID, cookieSessionToken, xtoken, now)
  fetchMaybe identity
