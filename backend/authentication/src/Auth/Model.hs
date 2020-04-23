module Auth.Model where

import Control.Monad.Catch
import Data.Int
import Data.Unjson
import qualified Data.Text as T
import Database.PostgreSQL.PQTypes
--import Database.PostgreSQL.PQTypes.Model
--import Database.PostgreSQL.PQTypes.SQL.Builder

import Auth.MagicHash

data APIToken = APIToken { atID    :: Int64     -- autoincrement for uniqueness
                         , atToken :: MagicHash -- random part for security
                         }
    deriving (Eq, Ord)

instance Show APIToken where
  showsPrec _ token = (++) $ show (atToken token) ++ "_" ++ show (atID token)

instance Read APIToken where
  readsPrec p s = case break (== '_') s of
    (ts, '_' : is) ->
      [ (APIToken { atID = i, atToken = read $ T.pack ts }, v)
      | (i, v) <- readsPrec p is
      ]
    _ -> []

unjsonAPIToken :: UnjsonDef APIToken
unjsonAPIToken = unjsonInvmapR
  (\s -> case reads s of
    [(apitoken, [])] -> pure apitoken
    _                -> fail "cannot parse APIToken"
  )
  show
  unjsonDef

data OAuthAuthorization = OAuthAuthorization
  { oaAPIToken     :: APIToken
  , oaAPISecret    :: MagicHash
  , oaAccessToken  :: APIToken
  , oaAccessSecret :: MagicHash
  } deriving (Show, Eq)

-- TODO use UserId and UserGroupId types, once they are available in some ID component
authenticateToken :: (MonadDB m, MonadThrow m) => OAuthAuthorization -> m (Maybe (Int64, Int64))
authenticateToken (OAuthAuthorization token secret atoken asecret) = do
  runQuery_ $ rawSQL
    (  "SELECT u.id, ug.id "
    <> "FROM oauth_access_token a "
    <> "JOIN oauth_privilege p ON p.access_token_id = a.id "
    <> "JOIN oauth_api_token t ON a.api_token_id    = t.id "
    <> "JOIN users u           ON t.user_id         = u.id "
    <> "JOIN user_groups ug    ON u.user_group_id   = ug.id "
    <> "WHERE t.id = $1 AND t.api_token = $2 AND t.api_secret = $3 AND a.id = $4 AND a.access_token = $5 AND a.access_secret = $6 AND p.privilege = $7 LIMIT 1"
    )
    ( atID token
    , atToken token
    , secret
    , atID atoken
    , atToken atoken
    , asecret
    , apiPersonal
    )
  fetchMaybe identity
  where
    apiPersonal = 0 :: Int64 -- APIPersonal
