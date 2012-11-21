module OAuth.Model where

import DB.Derive
import MinutesTime
import DB
import User.Model
import MagicHash

import Crypto.RNG
import Data.Int
import Data.List
import Control.Monad.Trans
import Network.URI
import Data.Maybe

data APIToken = APIToken { atID    :: Int64     -- autoincrement for uniqueness
                         , atToken :: MagicHash -- random part for security
                         }
    deriving (Eq, Ord)

instance Show APIToken where
    showsPrec _ token = (++) $ show (atToken token) ++ "_" ++ show (atID token)

instance Read APIToken where
    readsPrec p s = case break (== '_') s of
      (ts, '_':is) -> [(APIToken { atID = i, atToken = read ts }, v) 
                      | (i, v) <- readsPrec p is]
      _ -> []

data APIPrivilege = APIDocCreate
                  | APIDocCheck
                  | APIDocSend
                  | APIPersonal  -- used only for personal access token
  deriving (Eq)

instance Read APIPrivilege where
  readsPrec _ "DOC_CREATE" = [(APIDocCreate, "")]
  readsPrec _ "DOC_CHECK"  = [(APIDocCheck, "")]
  readsPrec _ "DOC_SEND"   = [(APIDocSend, "")] 
  readsPrec _ _ = [] -- we should never read APIPersonal

instance Show APIPrivilege where
  showsPrec _ APIDocCreate = (++) "DOC_CREATE"
  showsPrec _ APIDocCheck  = (++) "DOC_CHECK"
  showsPrec _ APIDocSend   = (++) "DOC_SEND"
  showsPrec _ APIPersonal  = (++) "PERSONAL"

instance Convertible APIPrivilege Int where
  safeConvert APIPersonal  = return 0
  safeConvert APIDocCreate = return 1
  safeConvert APIDocCheck  = return 2
  safeConvert APIDocSend   = return 3
  
instance Convertible Int APIPrivilege where
  safeConvert 0 = return APIPersonal
  safeConvert 1 = return APIDocCreate
  safeConvert 2 = return APIDocCheck
  safeConvert 3 = return APIDocSend

  safeConvert s = Left ConvertError { convSourceValue  = show s
                                    , convSourceType   = "Int"
                                    , convDestType     = "APIPrivilege"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }

instance Convertible APIPrivilege SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue APIPrivilege where
  safeConvert s = safeConvert (fromSql s :: Int)
  
data OAuthTempCredRequest = OAuthTempCredRequest { tcCallback   :: URI
                                                 , tcAPIToken   :: APIToken
                                                 , tcAPISecret  :: MagicHash
                                                 , tcPrivileges :: [APIPrivilege]
                                                 }
                          deriving (Show)

data OAuthTokenRequest = OAuthTokenRequest { trAPIToken   :: APIToken
                                           , trAPISecret  :: MagicHash
                                           , trTempToken  :: APIToken
                                           , trTempSecret :: MagicHash
                                           , trVerifier   :: MagicHash
                                           }
                          deriving (Show)

data OAuthAuthorization = OAuthAuthorization { oaAPIToken     :: APIToken
                                             , oaAPISecret    :: MagicHash
                                             , oaAccessToken  :: APIToken
                                             , oaAccessSecret :: MagicHash
                                             }
                          deriving (Show)                                               
                                               
-- APIToken Management
  
{- |
   Create a new API Token for a User. A User can have an unlimited number of API Tokens.
 -}
data CreateAPIToken = CreateAPIToken UserID
instance (MonadDB m, CryptoRNG m) => DBUpdate m CreateAPIToken Bool where
  update (CreateAPIToken userid) = do
    token  :: MagicHash <- lift random
    secret :: MagicHash <- lift random
    kPrepare (  "INSERT INTO oauth_api_token "
             <> "(api_token, api_secret, user_id) "   
             <> "SELECT ?, ?, ? "
             <> "WHERE ? in (SELECT id from users)") -- UserID must point to real user
    r <- kExecute [toSql token,
                   toSql secret,
                   toSql userid, 
                   toSql userid]
    return $ r == 1

{- |
   Delete an API Token. We check the UserID for security purposes. A User may
   only delete their own tokens.
 -}
data DeleteAPIToken = DeleteAPIToken UserID APIToken
instance (MonadDB m, CryptoRNG m) => DBUpdate m DeleteAPIToken Bool where
  update (DeleteAPIToken userid (APIToken i t)) = do
    kPrepare (  "DELETE FROM oauth_api_token "
             <> "WHERE user_id = ? AND id = ? AND api_token = ? ")
    r <- kExecute [toSql userid,
                   toSql i,
                   toSql t]
    return $ r == 1

{- |
   Get the API Tokens for a particular user, along with their API Secrets.
 -}
data GetAPITokensForUser = GetAPITokensForUser UserID
instance MonadDB m => DBQuery m GetAPITokensForUser [(APIToken, MagicHash)] where
  query (GetAPITokensForUser uid) = do
    kPrepare (  "SELECT id, api_token, api_secret "
             <> "FROM oauth_api_token "
             <> "WHERE user_id = ? AND "
             -- exclude personal tokens
             <> "      id NOT IN (SELECT api_token_id FROM oauth_access_token "
             <> "                 JOIN oauth_privilege on oauth_privilege.access_token_id = oauth_access_token.id "
             <> "                 WHERE oauth_privilege.privilege = ?) " 
             <> "ORDER BY id DESC")
    _ <- kExecute [toSql uid, toSql APIPersonal]
    foldDB (\acc i t s -> (APIToken i t, s):acc) []


-- Temporary Credentials Request (first part of OAuth flow)

{- |
   Record a request and return the Temporary API Token + Secret.

   Used in the first part of the OAuth flow.
 -}
data RequestTempCredentials = RequestTempCredentials OAuthTempCredRequest MinutesTime
instance (CryptoRNG m, MonadDB m) => DBUpdate m RequestTempCredentials (Maybe (APIToken, MagicHash)) where
  update (RequestTempCredentials (OAuthTempCredRequest {tcPrivileges = []}) _) = return Nothing
  update (RequestTempCredentials (OAuthTempCredRequest {tcPrivileges}) _) | APIPersonal `elem` tcPrivileges = return Nothing
  update (RequestTempCredentials (OAuthTempCredRequest {..}) time) = do
    temptoken  :: MagicHash <- lift random
    tempsecret :: MagicHash <- lift random
    verifier   :: MagicHash <- lift random
    mid ::    Maybe Int64 <- getOne $ SQL ("INSERT INTO oauth_temp_credential ("
                           <> "  temp_token"
                           <> ", temp_secret"
                           <> ", api_token_id"
                           <> ", verifier"
                           <> ", expires"
                           <> ", callback"
                           <> ") (SELECT ?, ?, ?, ?, ?, ? WHERE EXISTS (SELECT 1 FROM oauth_api_token WHERE id = ? AND api_token = ? AND api_secret = ?))"
                           <> " RETURNING id")
                           [ toSql temptoken
                           , toSql tempsecret
                           , toSql $ atID tcAPIToken
                           , toSql verifier
                           , toSql $ 60 `minutesAfter` time
                           , toSql $ show tcCallback
                           , toSql $ atID tcAPIToken
                           , toSql $ atToken tcAPIToken
                           , toSql tcAPISecret
                           ]
    case mid of
      Nothing          -> return Nothing
      Just temptokenid -> do
        kRun_ $ "INSERT INTO oauth_temp_privileges ( "
                  <> " temp_token_id "
                  <> ",privilege "
                  <> ") VALUES " 
                  <> sqlConcatComma (map (\p -> "(" <?> temptokenid <+> "," <?> p <+> ")") tcPrivileges)
        return $ Just (APIToken { atID = temptokenid, atToken = temptoken}, tempsecret)

-- second part of flow (user granting privileges)

{- |
   Associate a User with a temporary token. Returns the callback and verifier
 -}
data VerifyCredentials = VerifyCredentials APIToken UserID MinutesTime
instance MonadDB m => DBUpdate m VerifyCredentials (Maybe (URI, MagicHash)) where
  update (VerifyCredentials token uid time) = do
    kPrepare ("UPDATE oauth_temp_credential SET user_id = ? WHERE user_id IS NULL AND EXISTS (SELECT 1 FROM users WHERE id = ?) AND id = ? AND temp_token = ? AND expires > ? RETURNING callback, verifier")
    _ <- kExecute [ toSql uid, toSql uid, toSql $ atID token, toSql $ atToken token, toSql time ]
    mr <- foldDB (\acc cb vr -> maybe acc (\uri->(uri,vr):acc) $ parseURI cb) []
    oneObjectReturnedGuard mr

{- |
   If the User does not want to grant privileges, we delete the request and return the callback URL.
 -}
data DenyCredentials = DenyCredentials APIToken MinutesTime
instance MonadDB m => DBUpdate m DenyCredentials (Maybe URI) where
  update (DenyCredentials token _time) = do
    _ <- kRun $ SQL "DELETE FROM oauth_temp_credential WHERE (id = ? AND temp_token = ?) RETURNING callback" 
                    [ toSql $ atID token, toSql $ atToken token ]
    mr <- foldDB (\acc cb -> maybe acc (:acc) $ parseURI cb) []
    oneObjectReturnedGuard mr

{- |
   For a given temp token, return the company name and list of requested privileges.
 -}
data GetRequestedPrivileges = GetRequestedPrivileges APIToken MinutesTime
instance MonadDB m => DBQuery m GetRequestedPrivileges (Maybe (String, [APIPrivilege])) where
  query (GetRequestedPrivileges token time) = do
    kPrepare (   "SELECT com.name, u.company_name, u.first_name, u.last_name, u.email, p.privilege "
              <> "FROM oauth_temp_privileges p " 
              <> "JOIN oauth_temp_credential c  ON p.temp_token_id =   c.id "
              <> "JOIN oauth_api_token t        ON c.api_token_id  =   t.id "
              <> "JOIN users u                  ON t.user_id       =   u.id "
              <> "LEFT OUTER JOIN companies com ON u.company_id    = com.id " -- 0..1 relationship
              <> "WHERE c.temp_token = ? AND c.id = ? AND expires > ? AND c.user_id IS NULL")
    _ <- kExecute [ toSql $ atToken token, toSql $ atID token, toSql time ]
    foldDB f Nothing
    -- get name of company from companies table, or if that does not exist, the users.company_name
    where f :: Maybe (String, [APIPrivilege]) -> Maybe String -> String -> String -> String -> String -> APIPrivilege -> Maybe (String, [APIPrivilege])
          f Nothing c n fn ln e pr       = Just (getname c n fn ln e, [pr])
          f (Just (n, acc)) _ _ _ _ _ pr = Just (n, pr:acc)
          getname (Just cname) _ _ _ _   = cname
          getname Nothing "" "" "" email = email
          getname Nothing "" fn ln _     = intercalate " " [fn, ln]
          getname Nothing cname _ _ _    = cname

-- third part of flow

{- |
   After the User is registered, the client can request an access token.
 -}
data RequestAccessToken = RequestAccessToken OAuthTokenRequest MinutesTime
instance (CryptoRNG m, MonadDB m) => DBUpdate m RequestAccessToken (Maybe (APIToken, MagicHash)) where
  update (RequestAccessToken (OAuthTokenRequest {..}) time) = do
    accesstoken  :: MagicHash <- lift random
    accesssecret :: MagicHash <- lift random
    kPrepare (   "INSERT INTO oauth_access_token (access_token, access_secret, api_token_id, user_id, created) "
              <> "SELECT ?, ?, c.api_token_id, c.user_id, ? "
              <> "FROM oauth_temp_credential c "
              <> "JOIN oauth_api_token a ON c.api_token_id = a.id "
              <> "WHERE a.id          = ? AND a.api_token = ? "
              <> "AND   a.api_secret  = ? "
              <> "AND   c.temp_secret = ? "
              <> "AND   c.id          = ? AND c.temp_token = ? "
              <> "AND   c.verifier    = ? "
              <> "AND   c.expires     > ? "
              <> "RETURNING id, access_token, access_secret")
    _ <- kExecute [ toSql accesstoken
                  , toSql accesssecret
                  , toSql time
                  , toSql $ atID trAPIToken
                  , toSql $ atToken trAPIToken
                  , toSql trAPISecret
                  , toSql trTempSecret
                  , toSql $ atID trTempToken
                  , toSql $ atToken trTempToken
                  , toSql trVerifier
                  , toSql time
                  ]
    mr <- foldDB (\acc i t s -> (APIToken { atID = i, atToken = t }, s):acc) []
    case mr of
      [(tk, _)] -> do
        _ <- kRun $ SQL (   "INSERT INTO oauth_privilege (access_token_id, privilege) "
                         <> "SELECT ?, privilege "
                         <> "FROM oauth_temp_privileges p "
                         <> "JOIN oauth_temp_credential c ON p.temp_token_id = c.id "
                         <> "JOIN oauth_api_token a       ON c.api_token_id  = a.id "
                         <> "WHERE a.id          = ? AND a.api_token = ? "
                         <> "AND   a.api_secret  = ? "
                         <> "AND   c.temp_secret = ? "
                         <> "AND   c.id          = ? AND c.temp_token = ? "
                         <> "AND   c.verifier    = ? "
                         <> "AND   c.expires     > ? ")
                         [ toSql $ atID tk
                         , toSql $ atID trAPIToken
                         , toSql $ atToken trAPIToken
                         , toSql trAPISecret
                         , toSql trTempSecret
                         , toSql $ atID trTempToken
                         , toSql $ atToken trTempToken
                         , toSql trVerifier
                         , toSql time
                         ]
        return ()
      _ -> return ()
    -- delete the old stuff and the one we just requested; it's one-time-use
    _ <- kRun $ SQL "DELETE FROM oauth_temp_credential WHERE (id = ? AND temp_token = ?) OR expires <= ?" 
                    [ toSql $ atID trTempToken, toSql $ atToken trTempToken, toSql time ]
    oneObjectReturnedGuard mr

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
                                    APIPrivilege
instance MonadDB m => DBQuery m GetUserIDForAPIWithPrivilege (Maybe (UserID, String)) where
  query (GetUserIDForAPIWithPrivilege token secret atoken asecret priv) = do
    kPrepare (  "SELECT a.user_id, u.email, u.first_name, u.last_name, u.company_name, c.name "
             <> "FROM oauth_access_token a "
             <> "JOIN oauth_privilege p      ON p.access_token_id = a.id "
             <> "JOIN oauth_api_token t      ON a.api_token_id    = t.id "
             <> "JOIN users u                ON t.user_id         = u.id "
             <> "LEFT OUTER JOIN companies c ON u.company_id      = c.id " -- 0..1 relationship
             <> "WHERE t.id = ? AND t.api_token = ? AND t.api_secret = ? AND a.id = ? AND a.access_token = ? AND a.access_secret = ? AND (p.privilege = ? OR p.privilege = ?)")
    _ <- kExecute [ toSql $ atID token
                  , toSql $ atToken token
                  , toSql secret
                  , toSql $ atID atoken
                  , toSql $ atToken atoken
                  , toSql asecret
                  , toSql priv
                  , toSql APIPersonal]
    mr <- foldDB f []
    oneObjectReturnedGuard mr
      where f acc uid _ _ _ _ (Just s) = (uid, s):acc               -- company name from company table
            f acc uid e "" "" "" _     = (uid, e):acc               -- just email
            f acc uid _ fn ln "" _     = (uid, fn ++ " " ++ ln):acc -- user's first + last
            f acc uid _ _  _  cn _     = (uid, cn):acc              -- user's company name
            
-- stuff for the dashboard
            
{- |
   Show the privileges granted by a user.

   Returns the Access Token ID, the Company name, and the list of granted privileges
 -}
data GetGrantedPrivileges = GetGrantedPrivileges UserID                           
instance MonadDB m => DBQuery m GetGrantedPrivileges [(Int64, String, [APIPrivilege])] where
  query (GetGrantedPrivileges userid) = do
    kPrepare (  "SELECT a.id, u.email, u.first_name, u.last_name, u.company_name, c.name, p.privilege "
             <> "FROM oauth_access_token a "
             <> "JOIN oauth_api_token t on a.api_token_id = t.id "
             <> "JOIN users u on u.id = t.user_id "
             <> "JOIN oauth_privilege p on p.access_token_id = a.id "
             <> "LEFT OUTER JOIN companies c ON u.company_id = c.id " -- 0..1 relationship
             <> "WHERE a.user_id = ? AND " 
             -- exclude personal tokens
             <> "      t.id NOT IN (SELECT api_token_id FROM oauth_access_token "
             <> "                   JOIN oauth_privilege on oauth_privilege.access_token_id = oauth_access_token.id "
             <> "                   WHERE oauth_privilege.privilege = ?) " 
             <> "ORDER BY a.id ")
    _ <- kExecute [toSql userid, toSql APIPersonal]
    foldDB f []
      where f ((tid,n,ps):as) tid' _ _ _ _ _ p | tid == tid' = (tid, n, p:ps): as -- already have the id
            f acc tid _ _ _ _ (Just s) p = (tid, s, [p]):acc                      -- company name
            f acc tid e "" "" "" _     p = (tid, e, [p]):acc                      -- just email
            f acc tid _ fn ln "" _     p = (tid, fn ++ " " ++ ln, [p]):acc        -- user's first + last
            f acc tid _ _  _  cn _     p = (tid, cn, [p]):acc                     -- user's company name
{- |
   Delete all privileges for a given token id.
   The user does not have to know the whole token.
 -}
data DeletePrivileges = DeletePrivileges UserID Int64
instance MonadDB m => DBUpdate m DeletePrivileges Bool where
  update (DeletePrivileges userid tokenid) = do
    kPrepare (  "DELETE FROM oauth_access_token "
             <> "WHERE user_id = ? AND id = ? ")
    r <- kExecute [toSql userid, toSql tokenid]
    return $ r > 0

{- |
   Delete a single privilege for a given token id.
   The user does not have to know the whole token.
 -}
data DeletePrivilege = DeletePrivilege UserID Int64 APIPrivilege
instance MonadDB m => DBUpdate m DeletePrivilege Bool where
  update (DeletePrivilege userid tokenid privilege) = do
    kPrepare (  "DELETE FROM oauth_privilege "
             <> "WHERE EXISTS (SELECT 1 FROM oauth_access_token "
             <> "                       WHERE ? = oauth_access_token.id "
             <> "                         AND oauth_access_token.user_id = ?)"
             <> "  AND privilege = ? AND access_token_id = ?")
    r <- kExecute [toSql tokenid, toSql userid, toSql privilege, toSql tokenid]
    -- get rid of oauth_access_tokens with not privileges
    kPrepare (  "DELETE FROM oauth_access_token "
             <> "WHERE user_id = ? AND id = ? AND id NOT IN (SELECT access_token_id FROM oauth_privilege)") 
    _ <- kExecute [toSql userid, toSql tokenid]
    return $ r == 0

-- Personal Tokens
    
{- |
   Each user has a single personal token used to access the api for their own account.
 -}
data GetPersonalToken = GetPersonalToken UserID
instance MonadDB m => DBQuery m GetPersonalToken (Maybe (APIToken, MagicHash, APIToken, MagicHash)) where
  query (GetPersonalToken userid) = do
    kPrepare (  "SELECT t.id, t.api_token, t.api_secret, a.id, a.access_token, a.access_secret "
             <> "FROM oauth_access_token a "
             <> "JOIN oauth_api_token t on a.api_token_id = t.id "
             <> "WHERE a.user_id = ? AND t.user_id = ? AND a.id IN (SELECT access_token_id FROM oauth_privilege WHERE access_token_id = a.id AND privilege = ?)")
    _ <- kExecute [toSql userid, toSql userid, toSql APIPersonal]
    mr <- foldDB (\acc ti tt ts i t s -> (APIToken ti tt, ts, APIToken i t, s):acc) []
    oneObjectReturnedGuard mr
    
{- |
   Create a personal token. Each User can have only one, so we should fail
   if we already have one
 -}
data CreatePersonalToken = CreatePersonalToken UserID
instance (MonadDB m, CryptoRNG m) => DBUpdate m CreatePersonalToken Bool where
  update (CreatePersonalToken userid) = do
    m <- DB.query $ GetPersonalToken userid
    if isJust m
      then return False
      else do
        token  :: MagicHash <- lift random
        secret :: MagicHash <- lift random
        kPrepare (  "INSERT INTO oauth_api_token "
                 <> "(api_token, api_secret, user_id) "   
                 <> "SELECT ?, ?, ? "
                 <> "WHERE ? in (SELECT id from users) "
                 <> "RETURNING id")
        _ <- kExecute [toSql token,
                       toSql secret,
                       toSql userid, 
                       toSql userid]
        [apiid] <- foldDB (\acc (i :: Int64) -> i:acc) []    
        -- now create the access token
        atoken  :: MagicHash <- lift random
        asecret :: MagicHash <- lift random
        now <- getMinutesTime
        kPrepare (  "INSERT INTO oauth_access_token "
                 <> "(access_token, access_secret, api_token_id, user_id, created) "   
                 <> "VALUES (?, ?, ?, ?, ?) "
                 <> "RETURNING id")
        _ <- kExecute [toSql atoken,
                       toSql asecret,
                       toSql apiid,
                       toSql userid,
                       toSql now]
        [accessid] <- foldDB (\acc (i :: Int64) -> i:acc) [] 
        -- need to add all privileges here
        kPrepare (  "INSERT INTO oauth_privilege "
                 <> "(access_token_id, privilege) "
                 <> "VALUES (?,?) ")
        r <- kExecute [toSql accessid, toSql APIPersonal]
        return $ r > 0

{- |
   Delete the personal token for a user.
 -}
data DeletePersonalToken = DeletePersonalToken UserID
instance MonadDB m => DBUpdate m DeletePersonalToken Bool where
  update (DeletePersonalToken userid) = do
    kPrepare (  "DELETE FROM oauth_api_token "
             <> "WHERE user_id = ? AND id IN "
             <> "(SELECT api_token_id FROM oauth_access_token "
             <> " JOIN oauth_privilege on oauth_privilege.access_token_id = oauth_access_token.id "
             <> " WHERE oauth_privilege.privilege = ?)")
    r <- kExecute [toSql userid, toSql APIPersonal]
    return $ r > 0
    
