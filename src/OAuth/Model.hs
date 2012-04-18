module OAuth.Model where

import DB.Derive
import MinutesTime
import DB
import qualified Log
import User.Model
import MagicHash

import Crypto.RNG
import Data.Int
import Data.Data (Data)
import Happstack.Data
import Data.List
import Control.Monad.Trans

data APIToken = APIToken { atID :: Int64
                         , atToken :: MagicHash
                         }
    deriving (Eq, Ord, Typeable, Data)

instance Show APIToken where
    showsPrec _ token = (++) $ show (atToken token) ++ "_" ++ show (atID token)

instance Read APIToken where
    readsPrec p s = case break (== '_') s of
      (ts, '_':is) -> [(APIToken { atID = i, atToken = read ts }, v) 
                      | (i, v) <- readsPrec p is]
      _ -> []

data APIPrivilege = APIDocCreate
  deriving (Eq)

instance Read APIPrivilege where
  readsPrec _ "DOC_CREATE" = [(APIDocCreate, "")]
  readsPrec _ _ = []

instance Show APIPrivilege where
  showsPrec _ APIDocCreate = (++) "DOC_CREATE"

instance Convertible APIPrivilege Int where
  safeConvert APIDocCreate = return 1

instance Convertible Int APIPrivilege where
  safeConvert 1 = return APIDocCreate
  safeConvert s = Left ConvertError { convSourceValue  = show s
                                    , convSourceType   = "Int"
                                    , convDestType     = "APIPrivilege"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }

instance Convertible APIPrivilege SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue APIPrivilege where
  safeConvert s = safeConvert (fromSql s :: Int)
                  
data RequestTempCredentials = RequestTempCredentials
                              APIToken 
                              MagicHash
                              [APIPrivilege]
                              String -- callback URL
                              MinutesTime
instance (CryptoRNG m, MonadDB m) => DBUpdate m RequestTempCredentials (Maybe (APIToken, MagicHash)) where
  update (RequestTempCredentials _     _      []    _        _) = return Nothing
  update (RequestTempCredentials token secret privs callback time) = do
    temptoken  :: MagicHash <- lift random
    tempsecret :: MagicHash <- lift random
    verifier   :: MagicHash <- lift random
    mid ::    Maybe Int64 <- getOne $ SQL ("INSERT INTO oauth_temp_credential ("
                           ++ "  temp_token"
                           ++ ", temp_secret"
                           ++ ", api_token_id"
                           ++ ", verifier"
                           ++ ", expires"
                           ++ ", callback"
                           ++ ") (SELECT ?, ?, ?, ?, ?, ? WHERE EXISTS (SELECT 1 FROM oauth_api_token WHERE id = ? AND api_token = ? AND api_secret = ?))"
                           ++ " RETURNING id")
                           [ toSql temptoken
                           , toSql tempsecret
                           , toSql $ atID token
                           , toSql verifier
                           , toSql $ 10 `minutesAfter` time
                           , toSql callback
                           , toSql $ atID token
                           , toSql $ atToken token
                           , toSql secret
                           ]
    case mid of
      Nothing   -> do
        Log.debug $ show [ toSql temptoken
                   , toSql tempsecret
                   , toSql $ atID token
                   , toSql verifier
                   , toSql $ 10 `minutesAfter` time
                   , toSql callback
                   , toSql $ atID token
                   , toSql $ atToken token
                   , toSql secret
                   ]
        return Nothing
      Just temptokenid -> do
        kPrepare ("INSERT INTO oauth_temp_privileges ( "
                  ++ " temp_token_id "
                  ++ ",privilege "
                  ++ ") VALUES " 
                  ++ (intercalate ", " $ map (const "(?,?)") privs))
        _ <- kExecute $ concatMap (\p-> [toSql temptokenid, toSql p]) privs
        return $ Just (APIToken { atID = temptokenid, atToken = temptoken}, tempsecret)

data VerifyCredentials = VerifyCredentials
                              APIToken 
                              UserID
                              MinutesTime
instance MonadDB m => DBUpdate m VerifyCredentials (Maybe (String, MagicHash)) where
  update (VerifyCredentials token uid time) = do
    kPrepare ("UPDATE oauth_temp_credential SET user_id = ? WHERE id = ? AND temp_token = ? AND expires > ? RETURNING callback, verifier")
    _ <- kExecute [ toSql uid, toSql $ atID token, toSql $ atToken token, toSql time ]
    mr <- foldDB (\acc cb vr -> (cb, vr):acc) []
    oneObjectReturnedGuard mr

data GetRequestedPrivileges = GetRequestedPrivileges
                              APIToken 
                              MinutesTime
instance MonadDB m => DBQuery m GetRequestedPrivileges (Maybe (String, [APIPrivilege])) where
  query (GetRequestedPrivileges token time) = do
    kPrepare (   "SELECT com.name, p.privilege FROM oauth_temp_privileges p " 
              ++ "JOIN oauth_temp_credential c ON p.temp_token_id =   c.id "
              ++ "JOIN oauth_api_token t       ON c.api_token_id  =   t.id "
              ++ "JOIN users u                 ON t.user_id       =   u.id "
              ++ "JOIN companies com           ON u.company_id    = com.id "
              ++ "WHERE c.temp_token = ? AND c.id = ? AND expires > ? AND c.user_id IS NULL")
    _ <- kExecute [ toSql $ atToken token, toSql $ atID token, toSql time ]
    foldDB f Nothing
    where f :: Maybe (String, [APIPrivilege]) -> String -> APIPrivilege -> Maybe (String, [APIPrivilege])
          f Nothing         name pr = Just (name, [pr])
          f (Just (_, acc)) name pr = Just (name, pr:acc)

data RequestAccessToken = RequestAccessToken
                          APIToken  -- API Token
                          MagicHash -- API Token Secret
                          APIToken  -- Temporary Token
                          MagicHash -- Temporary Token secret
                          MagicHash -- Verifier code
                          MinutesTime
instance (CryptoRNG m, MonadDB m) => DBUpdate m RequestAccessToken (Maybe (APIToken, MagicHash)) where
  update (RequestAccessToken tok sec temptok tempsec ver time) = do
    accesstoken  :: MagicHash <- lift random
    accesssecret :: MagicHash <- lift random
    kPrepare (   "INSERT INTO oauth_access_token (access_token, access_secret, api_token_id, user_id, created) "
              ++ "SELECT ?, ?, c.api_token_id, c.user_id, ? FROM oauth_temp_credential c "
              ++ "JOIN oauth_api_token a ON c.api_token_id = a.id "
              ++ "WHERE a.id = ? AND a.api_token = ? "
              ++ "AND   a.api_secret = ? "
              ++ "AND   c.temp_secret = ? "
              ++ "AND   c.id = ? AND c.temp_token = ? "
              ++ "AND   c.verifier = ? "
              ++ "AND   c.expires > ? "
              ++ "RETURNING id, access_token, access_secret")
    _ <- kExecute [ toSql accesstoken
                  , toSql accesssecret
                  , toSql time
                  , toSql $ atID tok
                  , toSql $ atToken tok
                  , toSql sec
                  , toSql tempsec
                  , toSql $ atID temptok
                  , toSql $ atToken temptok
                  , toSql ver
                  , toSql time
                  ]
    Log.debug $ show [ toSql accesstoken
                  , toSql accesssecret
                  , toSql time
                  , toSql $ atID tok
                  , toSql $ atToken tok
                  , toSql sec
                  , toSql tempsec
                  , toSql $ atID temptok
                  , toSql $ atToken temptok
                  , toSql ver
                  , toSql time
                  ]
    mr <- foldDB (\acc i t s -> (APIToken { atID = i, atToken = t }, s):acc) []
    Log.debug $ "inserted: " ++ show mr
    case mr of
      [(tk, _)] -> do
        _ <- kRun $ SQL (   "INSERT INTO oauth_privilege (access_token_id, privilege) "
                         ++ "SELECT ?, privilege FROM oauth_temp_privileges p "
                         ++ "JOIN oauth_temp_credential c ON p.temp_token_id = c.id "
                         ++ "JOIN oauth_api_token a ON c.api_token_id = a.id "
                         ++ "WHERE a.id = ? AND a.api_token = ? "
                         ++ "AND   a.api_secret = ? "
                         ++ "AND   c.temp_secret = ? "
                         ++ "AND   c.id = ? AND c.temp_token = ? "
                         ++ "AND   c.verifier = ? "
                         ++ "AND   c.expires > ? ")
                         [ toSql $ atID tk
                         , toSql $ atID tok
                         , toSql $ atToken tok
                         , toSql sec
                         , toSql tempsec
                         , toSql $ atID temptok
                         , toSql $ atToken temptok
                         , toSql ver
                         , toSql time
                         ]
        Log.debug $ show [ toSql $ atID tk
                         , toSql $ atID tok
                         , toSql $ atToken tok
                         , toSql sec
                         , toSql tempsec
                         , toSql $ atID temptok
                         , toSql $ atToken temptok
                         , toSql ver
                         , toSql time
                         ]
        return ()
      _ -> return ()
    -- delete the old stuff and the one we just requested; it's one-time-use
    _ <- kRun $ SQL "DELETE FROM oauth_temp_credential WHERE (id = ? AND temp_token = ?) OR expires <= ?" 
                    [ toSql $ atID temptok, toSql $ atToken temptok, toSql time ]
    oneObjectReturnedGuard mr


data DenyCredentials = DenyCredentials
                       APIToken
                       MinutesTime
instance MonadDB m => DBUpdate m DenyCredentials () where
  update (DenyCredentials token time) = do
    _ <- kRun $ SQL "DELETE FROM oauth_temp_credential WHERE (id = ? AND temp_token = ?) OR expires <= ?" 
                    [ toSql $ atID token, toSql $ atToken token, toSql time ]
    return ()

data GetUserIDForAPIWithPrivilege = GetUserIDForAPIWithPrivilege
                                    APIToken
                                    MagicHash
                                    APIToken
                                    MagicHash
                                    APIPrivilege
instance MonadDB m => DBQuery m GetUserIDForAPIWithPrivilege (Maybe (UserID, String)) where
  query (GetUserIDForAPIWithPrivilege token secret atoken asecret priv) = do
    kPrepare (  "SELECT a.user_id, c.name "
             ++ "FROM oauth_access_token a "
             ++ "JOIN oauth_privilege p ON p.access_token_id = a.id "
             ++ "JOIN oauth_api_token t ON a.api_token_id = t.id "
             ++ "JOIN users u ON t.user_id = u.id "
             ++ "JOIN companies c ON u.company_id = c.id "
             ++ "WHERE t.id = ? AND t.api_token = ? AND t.api_secret = ? AND a.id = ? AND a.access_token = ? AND a.access_secret = ? AND p.privilege = ?")
    _ <- kExecute [ toSql $ atID token
                  , toSql $ atToken token
                  , toSql secret
                  , toSql $ atID atoken
                  , toSql $ atToken atoken
                  , toSql asecret
                  , toSql priv ]
    mr <- foldDB (\acc u s -> (u, s):acc) []
    oneObjectReturnedGuard mr
