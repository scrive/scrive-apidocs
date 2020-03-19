module OAuth.Model where

import Control.Monad.Catch
import Crypto.RNG
import Data.Aeson hiding ((<?>))
import Data.Int
import Data.Unjson
import Network.URI
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T

import DB
import Log.Identifier
import MagicHash
import MinutesTime
import User.Model

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

data APIPrivilege = APIDocCreate
                  | APIDocCheck
                  | APIDocSend
                  | APIPersonal  -- used only for personal access token
  deriving (Eq, Enum)

allPrivileges :: [APIPrivilege]
allPrivileges = [toEnum 0 ..]

instance Read APIPrivilege where
  readsPrec _ "DOC_CREATE" = [(APIDocCreate, "")]
  readsPrec _ "DOC_CHECK"  = [(APIDocCheck, "")]
  readsPrec _ "DOC_SEND"   = [(APIDocSend, "")]
  readsPrec _ _            = [] -- we should never read APIPersonal

instance Show APIPrivilege where
  showsPrec _ APIDocCreate = (++) "DOC_CREATE"
  showsPrec _ APIDocCheck  = (++) "DOC_CHECK"
  showsPrec _ APIDocSend   = (++) "DOC_SEND"
  showsPrec _ APIPersonal  = (++) "PERSONAL"

instance PQFormat APIPrivilege where
  pqFormat = pqFormat @Int16

instance FromSQL APIPrivilege where
  type PQBase APIPrivilege = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      0 -> return APIPersonal
      1 -> return APIDocCreate
      2 -> return APIDocCheck
      3 -> return APIDocSend
      _ -> E.throwIO $ RangeError { reRange = [(0, 3)], reValue = n }

instance ToSQL APIPrivilege where
  type PQDest APIPrivilege = PQDest Int16
  toSQL APIPersonal  = toSQL (0 :: Int16)
  toSQL APIDocCreate = toSQL (1 :: Int16)
  toSQL APIDocCheck  = toSQL (2 :: Int16)
  toSQL APIDocSend   = toSQL (3 :: Int16)

data OAuthTempCredRequest = OAuthTempCredRequest
  { tcCallback   :: URI
  , tcAPIToken   :: APIToken
  , tcAPISecret  :: MagicHash
  , tcPrivileges :: [APIPrivilege]
  } deriving Show

instance Loggable OAuthTempCredRequest where
  logValue OAuthTempCredRequest {..} = object
    [ "callback" .= show tcCallback
    , "api_token" .= show tcAPIToken
    , "privileges" .= map show tcPrivileges
    ]
  logDefaultLabel _ = "temp_cred_request"

data OAuthTokenRequest = OAuthTokenRequest
  { trAPIToken   :: APIToken
  , trAPISecret  :: MagicHash
  , trTempToken  :: APIToken
  , trTempSecret :: MagicHash
  , trVerifier   :: MagicHash
  } deriving Show

data OAuthAuthorization = OAuthAuthorization
  { oaAPIToken     :: APIToken
  , oaAPISecret    :: MagicHash
  , oaAccessToken  :: APIToken
  , oaAccessSecret :: MagicHash
  } deriving (Show, Eq)

data OAuthAuthorizationHideSecrets = OAuthAuthorizationHideSecrets
  { oahsAPIToken     :: APIToken
  , oahsAPISecret    :: Text
  , oahsAccessToken  :: APIToken
  , oahsAccessSecret :: Text
  , oahsHidden       :: Bool
  } deriving (Show, Eq)

toOAuthAuthorizationHideSecrets :: OAuthAuthorization -> OAuthAuthorizationHideSecrets
toOAuthAuthorizationHideSecrets oauth = OAuthAuthorizationHideSecrets
  { oahsAPIToken     = oaAPIToken oauth
  , oahsAPISecret    = T.pack $ (showOnlyLastFourCharacters . oaAPISecret) oauth
  , oahsAccessToken  = oaAccessToken oauth
  , oahsAccessSecret = T.pack $ (showOnlyLastFourCharacters . oaAccessSecret) oauth
  , oahsHidden       = True
  }

unjsonOAuthAuthorization :: UnjsonDef OAuthAuthorization
unjsonOAuthAuthorization =
  objectOf
    $   OAuthAuthorization
    <$> fieldBy "apitoken" oaAPIToken "OAuth API token" unjsonAPIToken
    <*> field "apisecret" oaAPISecret "OAuth API secret"
    <*> fieldBy "accesstoken" oaAccessToken "OAuth access token" unjsonAPIToken
    <*> field "accesssecret" oaAccessSecret "OAuth access secret"

unjsonOAuthAuthorizationHideSecrets :: UnjsonDef OAuthAuthorizationHideSecrets
unjsonOAuthAuthorizationHideSecrets =
  objectOf
    $   OAuthAuthorizationHideSecrets
    <$> fieldBy "apitoken" oahsAPIToken "OAuth API token" unjsonAPIToken
    <*> field "apisecret" oahsAPISecret "OAuth API secret"
    <*> fieldBy "accesstoken" oahsAccessToken "OAuth access token" unjsonAPIToken
    <*> field "accesssecret" oahsAccessSecret "OAuth access secret"
    <*> field "hidden"       oahsHidden       "OAuth api and access secrets are hidden"

-- APIToken Management

{- |
   Create a new API Token for a User. A User can have an unlimited number of API Tokens.
 -}
newtype CreateAPIToken = CreateAPIToken UserID
instance (MonadDB m, MonadThrow m, CryptoRNG m) => DBUpdate m CreateAPIToken Bool where
  update (CreateAPIToken userid) = do
    token :: MagicHash  <- random
    secret :: MagicHash <- random
    runQuery01 $ rawSQL
      (  "INSERT INTO oauth_api_token "
      <> "(api_token, api_secret, user_id) "
      <> "SELECT $1, $2, $3 "
      <> "WHERE $4 in (SELECT id from users)"
      ) -- UserID must point to real user
      (token, secret, userid, userid)

{- |
   Delete an API Token. We check the UserID for security purposes. A User may
   only delete their own tokens.
 -}
data DeleteAPIToken = DeleteAPIToken UserID APIToken
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteAPIToken Bool where
  update (DeleteAPIToken userid (APIToken i t)) = do
    runQuery01 $ rawSQL
      (  "DELETE FROM oauth_api_token "
      <> "WHERE user_id = $1 AND id = $2 AND api_token = $3 "
      )
      (userid, i, t)

{- |
   Get the API Tokens for a particular user, along with their API Secrets.
 -}
newtype GetAPITokensForUser = GetAPITokensForUser UserID
instance MonadDB m => DBQuery m GetAPITokensForUser [(APIToken, MagicHash)] where
  query (GetAPITokensForUser uid) = do
    runQuery_ $ rawSQL
      (  "SELECT id, api_token, api_secret "
      <> "FROM oauth_api_token "
      <> "WHERE user_id = $1 AND "
             -- exclude personal tokens
      <> "      id NOT IN (SELECT api_token_id FROM oauth_access_token "
      <> "                 JOIN oauth_privilege on oauth_privilege.access_token_id = oauth_access_token.id "
      <> "                 WHERE oauth_privilege.privilege = $2) "
      <> "ORDER BY id"
      )
      (uid, APIPersonal)
    fetchMany $ \(i, t, s) -> (APIToken i t, s)


-- Temporary Credentials Request (first part of OAuth flow)

{- |
   Record a request and return the Temporary API Token + Secret.

   Used in the first part of the OAuth flow.
 -}
data RequestTempCredentials = RequestTempCredentials OAuthTempCredRequest UTCTime
instance (CryptoRNG m, MonadDB m, MonadThrow m) => DBUpdate m RequestTempCredentials (Maybe (APIToken, MagicHash)) where
  update (RequestTempCredentials OAuthTempCredRequest { tcPrivileges = [] } _) =
    return Nothing
  update (RequestTempCredentials OAuthTempCredRequest { tcPrivileges } _)
    | APIPersonal `elem` tcPrivileges = return Nothing
  update (RequestTempCredentials OAuthTempCredRequest {..} time) = do
    temptoken :: MagicHash  <- random
    tempsecret :: MagicHash <- random
    verifier :: MagicHash   <- random
    runQuery_ $ rawSQL
      (  "INSERT INTO oauth_temp_credential ("
      <> "  temp_token"
      <> ", temp_secret"
      <> ", api_token_id"
      <> ", verifier"
      <> ", expires"
      <> ", callback"
      <> ") (SELECT $1, $2, $3, $4, $5, $6 WHERE EXISTS (SELECT 1 FROM oauth_api_token WHERE id = $7 AND api_token = $8 AND api_secret = $9))"
      <> " RETURNING id"
      )
      ( temptoken
      , tempsecret
      , atID tcAPIToken
      , verifier
      , 60 `minutesAfter` time
      , show tcCallback
      , atID tcAPIToken
      , atToken tcAPIToken
      , tcAPISecret
      )
    mid :: Maybe Int64 <- fetchMaybe runIdentity
    case mid of
      Nothing          -> return Nothing
      Just temptokenid -> do
        runQuery_
          $  "INSERT INTO oauth_temp_privileges ( "
          <> " temp_token_id "
          <> ",privilege "
          <> ") VALUES "
          <> sqlConcatComma
               (map (\p -> "(" <?> temptokenid <+> "," <?> p <+> ")") tcPrivileges)
        return $ Just (APIToken { atID = temptokenid, atToken = temptoken }, tempsecret)

-- second part of flow (user granting privileges)

{- |
   Associate a User with a temporary token. Returns the callback and verifier
 -}
data VerifyCredentials = VerifyCredentials APIToken UserID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m VerifyCredentials (Maybe (URI, MagicHash)) where
  update (VerifyCredentials token uid time) = do
    runQuery_ $ rawSQL
      "UPDATE oauth_temp_credential SET user_id = $1 WHERE user_id IS NULL AND EXISTS (SELECT 1 FROM users WHERE id = $2) AND id = $3 AND temp_token = $4 AND expires > $5 RETURNING callback, verifier"
      (uid, uid, atID token, atToken token, time)
    join <$> fetchMaybe (\(cb, vr) -> (, vr) <$> parseURI cb)

{- |
   If the User does not want to grant privileges, we delete the request and return the callback URL.
 -}
data DenyCredentials = DenyCredentials APIToken UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m DenyCredentials (Maybe URI) where
  update (DenyCredentials token _time) = do
    runQuery_ $ rawSQL
      "DELETE FROM oauth_temp_credential WHERE (id = $1 AND temp_token = $2) RETURNING callback"
      (atID token, atToken token)
    join <$> fetchMaybe (parseURI . runIdentity)

{- |
   For a given temp token, return the company name and list of requested privileges.
 -}
data GetRequestedPrivileges = GetRequestedPrivileges APIToken UTCTime
instance MonadDB m => DBQuery m GetRequestedPrivileges (Maybe (String, [APIPrivilege])) where
  query (GetRequestedPrivileges token time) = do
    runQuery_ $ rawSQL
      (  "SELECT ug.name, u.first_name, u.last_name, u.email, p.privilege "
      <> "FROM oauth_temp_privileges p "
      <> "JOIN oauth_temp_credential c  ON p.temp_token_id =   c.id "
      <> "JOIN oauth_api_token t        ON c.api_token_id  =   t.id "
      <> "JOIN users u                  ON t.user_id       =   u.id "
      <> "JOIN user_groups ug           ON u.user_group_id =  ug.id "
      <> "WHERE c.temp_token = $1 AND c.id = $2 AND expires > $3 AND c.user_id IS NULL"
      )
      (atToken token, atID token, time)
    -- get name of company from companies table, or if that does not exist, the users.company_name
    foldlDB f Nothing
    where
      f
        :: Maybe (String, [APIPrivilege])
        -> (String, String, String, String, APIPrivilege)
        -> m (Maybe (String, [APIPrivilege]))
      f Nothing         (c, fn, ln, e, pr) = return $ Just (getname c fn ln e, [pr])
      f (Just (n, acc)) (_, _ , _ , _, pr) = return $ Just (n, pr : acc)
      getname ""    "" "" email = email
      getname ""    fn ln _     = fn ++ " " ++ ln
      getname cname _  _  _     = cname

-- third part of flow

{- |
   After the User is registered, the client can request an access token.
 -}
data RequestAccessToken = RequestAccessToken OAuthTokenRequest UTCTime
instance (CryptoRNG m, MonadDB m, MonadThrow m) => DBUpdate m RequestAccessToken (Maybe (APIToken, MagicHash)) where
  update (RequestAccessToken OAuthTokenRequest {..} time) = do
    accesstoken :: MagicHash  <- random
    accesssecret :: MagicHash <- random
    runQuery_ $ rawSQL
      ("INSERT INTO oauth_access_token (access_token, access_secret, api_token_id, user_id, created) "
      <> "SELECT $1, $2, c.api_token_id, c.user_id, $3 "
      <> "FROM oauth_temp_credential c "
      <> "JOIN oauth_api_token a ON c.api_token_id = a.id "
      <> "WHERE a.id          = $4 AND a.api_token = $5 "
      <> "AND   a.api_secret  = $6 "
      <> "AND   c.temp_secret = $7 "
      <> "AND   c.id          = $8 AND c.temp_token = $9 "
      <> "AND   c.verifier    = $10 "
      <> "AND   c.expires     > $11 "
      <> "RETURNING id, access_token, access_secret"
      )
      ( accesstoken
      , accesssecret
      , time
      , atID trAPIToken
      , atToken trAPIToken
      , trAPISecret
      , trTempSecret
      , atID trTempToken
      , atToken trTempToken
      , trVerifier
      , time
      )
    mr <- fetchMaybe $ \(i, t, s) -> (APIToken { atID = i, atToken = t }, s)
    case mr of
      Just (tk, _) -> do
        runQuery_ $ rawSQL
          (  "INSERT INTO oauth_privilege (access_token_id, privilege) "
          <> "SELECT $1, privilege "
          <> "FROM oauth_temp_privileges p "
          <> "JOIN oauth_temp_credential c ON p.temp_token_id = c.id "
          <> "JOIN oauth_api_token a       ON c.api_token_id  = a.id "
          <> "WHERE a.id          = $2 AND a.api_token = $3 "
          <> "AND   a.api_secret  = $4 "
          <> "AND   c.temp_secret = $5 "
          <> "AND   c.id          = $6 AND c.temp_token = $7 "
          <> "AND   c.verifier    = $8 "
          <> "AND   c.expires     > $9 "
          )
          ( atID tk
          , atID trAPIToken
          , atToken trAPIToken
          , trAPISecret
          , trTempSecret
          , atID trTempToken
          , atToken trTempToken
          , trVerifier
          , time
          )
        return ()
      _ -> return ()
    -- delete the old stuff and the one we just requested; it's one-time-use
    runQuery_ $ rawSQL
      "DELETE FROM oauth_temp_credential WHERE (id = $1 AND temp_token = $2) OR expires <= $3"
      (atID trTempToken, atToken trTempToken, time)
    return mr

-- used for authorization

{- |
   Given an API Token, Secret, Access Token, Secret and a privilege, get the userid
   associated with this access token along with the company name of the
 -}
data GetUserIDForAPIWithPrivilege = GetUserIDForAPIWithPrivilege
                                    APIToken
                                    MagicHash
                                    APIToken
                                    MagicHash
                                    [APIPrivilege]
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserIDForAPIWithPrivilege (Maybe (UserID, String)) where
  query (GetUserIDForAPIWithPrivilege token secret atoken asecret priv) = do
    runQuery_ $ rawSQL
      (  "SELECT a.user_id, u.email, u.first_name, u.last_name,ug.name "
      <> "FROM oauth_access_token a "
      <> "JOIN oauth_privilege p      ON p.access_token_id = a.id "
      <> "JOIN oauth_api_token t      ON a.api_token_id    = t.id "
      <> "JOIN users u                ON t.user_id         = u.id "
      <> "JOIN user_groups ug         ON u.user_group_id   = ug.id "
      <> "WHERE t.id = $1 AND t.api_token = $2 AND t.api_secret = $3 AND a.id = $4 AND a.access_token = $5 AND a.access_secret = $6 AND (p.privilege = ANY($7) OR p.privilege = $8) LIMIT 1"
      )
      ( atID token
      , atToken token
      , secret
      , atID atoken
      , atToken atoken
      , asecret
      , Array1 priv
      , APIPersonal
      )
    fetchMaybe f
    where
      f (uid, e, "", "", "" ) = (uid, e)               -- just email
      f (uid, _, fn, ln, "" ) = (uid, fn ++ " " ++ ln) -- user's first + last
      f (uid, _, _ , _ , ugn) = (uid, ugn)             -- user's user_group name

-- stuff for the dashboard

{- |
   Show the privileges granted by a user.

   Returns the Access Token ID, the Company name, and the list of granted privileges
 -}
newtype GetGrantedPrivileges = GetGrantedPrivileges UserID
instance MonadDB m => DBQuery m GetGrantedPrivileges [(Int64, String, [APIPrivilege])] where
  query (GetGrantedPrivileges userid) = do
    runQuery_ $ rawSQL
      (  "SELECT a.id, u.email, u.first_name, u.last_name, ug.name, p.privilege "
      <> "FROM oauth_access_token a "
      <> "JOIN oauth_api_token t on a.api_token_id = t.id "
      <> "JOIN users u on u.id = t.user_id "
      <> "JOIN oauth_privilege p on p.access_token_id = a.id "
      <> "JOIN user_groups ug ON u.user_group_id = ug.id "
      <> "WHERE a.user_id = $1 AND "
             -- exclude personal tokens
      <> "      t.id NOT IN (SELECT api_token_id FROM oauth_access_token "
      <> "                   JOIN oauth_privilege on oauth_privilege.access_token_id = oauth_access_token.id "
      <> "                   WHERE oauth_privilege.privilege = $2) "
      <> "ORDER BY a.id "
      )
      (userid, APIPersonal)
    foldlDB f []
    where
      f ((tid, n, ps) : as) (tid', _, _, _, _, p) | tid == tid' =
        return $ (tid, n, p : ps) : as -- already have the id
      f acc (tid, e, "", "", "", p) = return $ (tid, e, [p]) : acc                     -- just email
      f acc (tid, _, fn, ln, "", p) = return $ (tid, fn ++ " " ++ ln, [p]) : acc        -- user's first + last
      f acc (tid, _, _ , _ , cn, p) = return $ (tid, cn, [p]) : acc                     -- user's company name company name
{- |
   Delete all privileges for a given token id.
   The user does not have to know the whole token.
 -}
data DeletePrivileges = DeletePrivileges UserID Int64
instance MonadDB m => DBUpdate m DeletePrivileges Bool where
  update (DeletePrivileges userid tokenid) = do
    r <- runQuery $ rawSQL
      ("DELETE FROM oauth_access_token " <> "WHERE user_id = $1 AND id = $2 ")
      (userid, tokenid)
    return $ r > 0

{- |
   Delete a single privilege for a given token id.
   The user does not have to know the whole token.
 -}
data DeletePrivilege = DeletePrivilege UserID Int64 APIPrivilege
instance MonadDB m => DBUpdate m DeletePrivilege Bool where
  update (DeletePrivilege userid tokenid privilege) = do
    r <- runQuery $ rawSQL
      (  "DELETE FROM oauth_privilege "
      <> "WHERE EXISTS (SELECT 1 FROM oauth_access_token "
      <> "                       WHERE $1 = oauth_access_token.id "
      <> "                         AND oauth_access_token.user_id = $2)"
      <> "  AND privilege = $3 AND access_token_id = $4"
      )
      (tokenid, userid, privilege, tokenid)
    -- get rid of oauth_access_tokens with not privileges
    runQuery_ $ rawSQL
      ("DELETE FROM oauth_access_token "
      <> "WHERE user_id = $1 AND id = $2 AND id NOT IN (SELECT access_token_id FROM oauth_privilege)"
      )
      (userid, tokenid)
    return $ r == 0

-- Personal Tokens

{- |
   Each user has a single personal token used to access the api for their own account.
 -}
newtype GetPersonalToken = GetPersonalToken UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetPersonalToken (Maybe OAuthAuthorization) where
  query (GetPersonalToken userid) = do
    runQuery_ $ rawSQL
      (  "SELECT t.id, t.api_token, t.api_secret, a.id, a.access_token, a.access_secret "
      <> "FROM oauth_access_token a "
      <> "JOIN oauth_api_token t on a.api_token_id = t.id "
      <> "WHERE a.user_id = $1 AND t.user_id = $2 AND a.id IN (SELECT access_token_id FROM oauth_privilege WHERE access_token_id = a.id AND privilege = $3)"
      )
      (userid, userid, APIPersonal)
    fetchMaybe $ \(ti, tt, ts, i, t, s) -> OAuthAuthorization
      { oaAPIToken     = APIToken ti tt
      , oaAPISecret    = ts
      , oaAccessToken  = APIToken i t
      , oaAccessSecret = s
      }

{- |
   Get personal token if it was newly created
 -}
data GetRecentPersonalToken = GetRecentPersonalToken UserID Int
instance (MonadDB m, MonadTime m, MonadThrow m) => DBQuery m GetRecentPersonalToken (Maybe OAuthAuthorization) where
  query (GetRecentPersonalToken userid recencyMinutes) = do
    now <- currentTime
    runQuery_ $ rawSQL
      (  "SELECT t.id, t.api_token, t.api_secret, a.id, a.access_token, a.access_secret "
      <> "FROM oauth_access_token a "
      <> "JOIN oauth_api_token t on a.api_token_id = t.id "
      <> "WHERE a.user_id = $1 AND t.user_id = $2 "
      <> "AND a.id IN (SELECT access_token_id FROM oauth_privilege WHERE access_token_id = a.id AND privilege = $3) "
      <> "AND a.created > $4 - INTERVAL '$5 minute'"
      )
      (userid, userid, APIPersonal, now, recencyMinutes)
    fetchMaybe $ \(ti, tt, ts, i, t, s) -> OAuthAuthorization
      { oaAPIToken     = APIToken ti tt
      , oaAPISecret    = ts
      , oaAccessToken  = APIToken i t
      , oaAccessSecret = s
      }

{- |
   Create a personal token. Each User can have only one, so we should fail
   if we already have one
 -}
newtype CreatePersonalToken = CreatePersonalToken UserID
instance (MonadDB m, MonadTime m, MonadThrow m, CryptoRNG m) => DBUpdate m CreatePersonalToken Bool where
  update (CreatePersonalToken userid) = do
    m <- DB.query $ GetPersonalToken userid
    if isJust m
      then return False
      else do
        token :: MagicHash  <- random
        secret :: MagicHash <- random
        runQuery_ $ rawSQL
          (  "INSERT INTO oauth_api_token "
          <> "(api_token, api_secret, user_id) "
          <> "SELECT $1, $2, $3 "
          <> "WHERE $4 in (SELECT id from users) "
          <> "RETURNING id"
          )
          (token, secret, userid, userid)
        apiid :: Int64       <- fetchOne runIdentity
        -- now create the access token
        atoken :: MagicHash  <- random
        asecret :: MagicHash <- random
        now                  <- currentTime
        runQuery_ $ rawSQL
          (  "INSERT INTO oauth_access_token "
          <> "(access_token, access_secret, api_token_id, user_id, created) "
          <> "VALUES ($1, $2, $3, $4, $5) "
          <> "RETURNING id"
          )
          (atoken, asecret, apiid, userid, now)
        accessid :: Int64 <- fetchOne runIdentity
        -- need to add all privileges here
        r                 <- runQuery $ rawSQL
          (  "INSERT INTO oauth_privilege "
          <> "(access_token_id, privilege) "
          <> "VALUES ($1, $2) "
          )
          (accessid, APIPersonal)
        return $ r > 0

{- |
   Delete the personal token for a user.
 -}
newtype DeletePersonalToken = DeletePersonalToken UserID
instance MonadDB m => DBUpdate m DeletePersonalToken Bool where
  update (DeletePersonalToken userid) = do
    r <- runQuery $ rawSQL
      (  "DELETE FROM oauth_api_token "
      <> "WHERE user_id = $1 AND id IN "
      <> "(SELECT api_token_id FROM oauth_access_token "
      <> " JOIN oauth_privilege on oauth_privilege.access_token_id = oauth_access_token.id "
      <> " WHERE oauth_privilege.privilege = $2)"
      )
      (userid, APIPersonal)
    return $ r > 0
