module OAuth.Model where

import DB.Derive
import MinutesTime
import DB.Classes
import DB.Utils
import DB.Fetcher2
--import Misc
import qualified Log
import User.Model
import MagicHash

import Crypto.RNG (random)
import Data.Int
import Database.HDBC
import Data.Data (Data)
import Happstack.Data
import Data.List

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
                              String -- email address
                              [APIPrivilege]
                              String -- callback URL
                              MinutesTime
instance DBUpdate RequestTempCredentials (Maybe (APIToken, MagicHash)) where
  dbUpdate (RequestTempCredentials _     _      _     []    _        _) = return Nothing
  dbUpdate (RequestTempCredentials token secret email privs callback time) = do
    temptoken  :: MagicHash <- random
    tempsecret :: MagicHash <- random
    verifier   :: MagicHash <- random
    mid ::    Maybe Int64 <- getOne $ SQL ("INSERT INTO oauth_temp_credential ("
                           ++ "  temp_token"
                           ++ ", temp_secret"
                           ++ ", api_token_id"
                           ++ ", verifier"
                           ++ ", expires"
                           ++ ", email"
                           ++ ", callback"
                           ++ ") (SELECT ?, ?, ?, ?, ?, ?, ? WHERE EXISTS (SELECT 1 FROM oauth_api_token WHERE id = ? AND api_token = ? AND api_secret = ?))"
                           ++ " RETURNING id")
                           [ toSql temptoken
                           , toSql tempsecret
                           , toSql $ atID token
                           , toSql verifier
                           , toSql $ 10 `minutesAfter` time
                           , toSql email
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
                   , toSql email
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
                              String -- email address
                              MinutesTime
instance DBUpdate VerifyCredentials (Maybe (String, MagicHash)) where
  dbUpdate (VerifyCredentials token email time) = do
    kPrepare ("SELECT callback, verifier FROM oauth_temp_credential WHERE email = ? AND id = ? AND temp_token = ? AND expires > ?")
    _ <- kExecute [ toSql email, toSql $ atID token, toSql $ atToken token, toSql time ]
    mr <- foldDB (\acc cb vr -> (cb, vr):acc) []
    oneObjectReturnedGuard mr

data GetRequestedPrivileges = GetRequestedPrivileges
                              APIToken 
                              String -- email address
                              MinutesTime
instance DBQuery GetRequestedPrivileges (Maybe (String, [APIPrivilege])) where
  dbQuery (GetRequestedPrivileges token email time) = do
    kPrepare (   "SELECT com.name, p.privilege FROM oauth_temp_privileges p " 
              ++ "JOIN oauth_temp_credential c ON p.temp_token_id =   c.id "
              ++ "JOIN oauth_api_token t       ON c.api_token_id  =   t.id "
              ++ "JOIN users u                 ON t.user_id       =   u.id "
              ++ "JOIN companies com           ON u.company_id    = com.id "
              ++ "WHERE c.temp_token = ? AND c.id = ? AND c.email = ? AND expires > ?")
    _ <- kExecute [ toSql $ atToken token, toSql $ atID token, toSql email, toSql time ]
    foldDB f Nothing
    where f :: Maybe (String, [APIPrivilege]) -> String -> APIPrivilege -> Maybe (String, [APIPrivilege])
          f Nothing         name pr = Just (name, [pr])
          f (Just (_, acc)) name pr = Just (name, pr:acc)

-- do we need to check the email address?
data RequestAccessToken = RequestAccessToken
                          APIToken  -- API Token
                          MagicHash -- API Token Secret
                          APIToken  -- Temporary Token
                          MagicHash -- Temporary Token secret
                          MagicHash -- Verifier code
                          MinutesTime
instance DBUpdate RequestAccessToken (Maybe (APIToken, MagicHash)) where
  dbUpdate (RequestAccessToken tok sec temptok tempsec ver time) = do
    accesstoken  :: MagicHash <- random
    accesssecret :: MagicHash <- random
    kPrepare (   "INSERT INTO oauth_access_token (access_token, access_secret, api_token_id, user_id, created) "
              ++ "SELECT ?, ?, c.api_token_id, u.id, ? FROM oauth_temp_credential c "
              ++ "JOIN oauth_api_token a ON c.api_token_id = a.id "
              ++ "JOIN users u ON c.email = u.email "
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
                         ++ "JOIN users u ON c.email = u.email "
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
                       String -- email address
                       MinutesTime
instance DBUpdate DenyCredentials () where
  dbUpdate (DenyCredentials token email time) = do
    _ <- kRun $ SQL "DELETE FROM oauth_temp_credential WHERE (id = ? AND temp_token = ? AND email = ?) OR expires <= ?" 
                    [ toSql $ atID token, toSql $ atToken token, toSql email, toSql time ]
    return ()

data GetUserIDForAPIWithPrivilege = GetUserIDForAPIWithPrivilege
                                    APIToken
                                    MagicHash
                                    APIToken
                                    MagicHash
                                    APIPrivilege
instance DBQuery GetUserIDForAPIWithPrivilege (Maybe (UserID, String)) where
  dbQuery (GetUserIDForAPIWithPrivilege token secret atoken asecret priv) = do
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
