module OAuth.Model where

import DB.Derive
import MinutesTime
import DB.Classes
import DB.Utils
import DB.Fetcher2
--import Misc
import qualified Log

import Crypto.RNG (random)
import Control.Applicative
import Data.Int
import Database.HDBC
import Data.Data (Data)
import Happstack.Data
import Data.List

data APIToken = APIToken { atID :: Int64
                         , atToken :: Int64
                         }
    deriving (Eq, Ord, Typeable, Data)

instance Show APIToken where
    showsPrec _ token = (++) $ show (atToken token) ++ "_" ++ show (atID token)

instance Read APIToken where
    readsPrec p s = case break (== '_') s of
      (ts, '_':is) -> [(APIToken { atID = i, atToken = read ts }, v) 
                      | (i, v) <- readsPrec p is]
      _ -> []

newtype APISecret = APISecret { unAPISecret :: Int64 }
    deriving (Eq, Ord, Typeable, Data)
$(newtypeDeriveUnderlyingReadShow ''APISecret)
$(newtypeDeriveConvertible ''APISecret)

data APITokenStatus = APIEnabled
                    | APIDisabled

instance Convertible APITokenStatus Int where
  safeConvert APIEnabled = return 1
  safeConvert APIDisabled = return 100

instance Convertible Int APITokenStatus where
  safeConvert 1 = return APIEnabled
  safeConvert 100 = return APIDisabled
  safeConvert s = Left ConvertError { convSourceValue = show s
                                    , convSourceType = "Int"
                                    , convDestType = "APITokenStatus"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }

instance Convertible APITokenStatus SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue APITokenStatus where
  safeConvert s = safeConvert (fromSql s :: Int)

data APIPrivilege = APIUserCreate
                  | APIDocCreate
  deriving (Eq)

instance Read APIPrivilege where
  readsPrec _ "DOC_CREATE" = [(APIDocCreate, "")]
  readsPrec _ "USER_CREATE" = [(APIUserCreate, "")]
  readsPrec _ _ = []

instance Show APIPrivilege where
  showsPrec _ APIDocCreate = (++) "DOC_CREATE"
  showsPrec _ APIUserCreate = (++) "USER_CREATE"

instance Convertible APIPrivilege Int where
  safeConvert APIUserCreate = return 1
  safeConvert APIDocCreate = return 2

instance Convertible Int APIPrivilege where
  safeConvert 1 = return APIUserCreate
  safeConvert 2 = return APIDocCreate
  safeConvert s = Left ConvertError { convSourceValue = show s
                                    , convSourceType = "Int"
                                    , convDestType = "APIPrivilege"
                                    , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                    }

instance Convertible APIPrivilege SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue APIPrivilege where
  safeConvert s = safeConvert (fromSql s :: Int)
                  
data RequestTempCredentials = RequestTempCredentials
                              APIToken 
                              APISecret
                              String -- email address
                              [APIPrivilege]
                              String -- callback URL
                              MinutesTime
instance DBUpdate RequestTempCredentials (Maybe (APIToken, APISecret)) where
  dbUpdate (RequestTempCredentials _     _      _     []    _        _) = return Nothing
  dbUpdate (RequestTempCredentials token secret email privs callback time) = do
    temptoken :: Int64 <- random
    tempsecret <- APISecret <$> random
    verifier   <- APISecret <$> random
    mid :: Maybe Int64 <- getOne  ("INSERT INTO oauth_temp_credential ("
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
    Log.debug $ "#### Got here"
    --mid :: Maybe Int64 <- foldDB id Nothing
    case mid of
      Nothing   -> return Nothing
      Just temptokenid -> do
        kPrepare ("INSERT INTO oauth_temp_privileges ("
                  ++ " temp_token_id"
                  ++ ",privilege"
                  ++ ") VALUES " 
                  ++ (intercalate ", " $ map (const "(?,?)") privs))
        _ <- kExecute $ concatMap (\p-> [toSql temptokenid, toSql p]) privs
        return $ Just (APIToken { atID = temptokenid, atToken = temptoken}, tempsecret)

data VerifyCredentials = VerifyCredentials
                              APIToken 
                              String -- email address
                              MinutesTime
instance DBUpdate VerifyCredentials (Maybe (String, APISecret)) where
  dbUpdate (VerifyCredentials token email time) = do
    kPrepare ("SELECT callback, verifier FROM oauth_temp_credential WHERE email = ? AND id = ? AND temp_token = ? AND expires > ?")
    _ <- kExecute [ toSql email, toSql $ atID token, toSql $ atToken token, toSql time ]
    mr <- foldDB (\acc cb vr -> (cb, vr):acc) []
    oneObjectReturnedGuard mr

data GetRequestedPrivileges = GetRequestedPrivileges
                              APIToken 
                              String -- email address
                              MinutesTime
instance DBQuery GetRequestedPrivileges [APIPrivilege] where
  dbQuery (GetRequestedPrivileges token email time) = do
    kPrepare (   "SELECT privilege FROM oauth_temp_privileges p " 
              ++ "JOIN oauth_temp_credential c ON p.temp_token_id = c.id "
              ++ " WHERE c.temp_token = ? AND c.id = ? AND c.email = ? AND expires > ?")
    _ <- kExecute [ toSql $ atToken token, toSql $ atID token, toSql email, toSql time ]
    foldDB (\acc pr -> pr:acc) []

-- do we need to check the email address?
data RequestAccessToken = RequestAccessToken
                          APIToken  -- API Token
                          APISecret -- API Token Secret
                          APIToken  -- Temporary Token
                          APISecret -- Temporary Token secret
                          APISecret -- Verifier code
                          MinutesTime
instance DBUpdate RequestAccessToken (Maybe (APIToken, APISecret)) where
  dbUpdate (RequestAccessToken tok sec temptok tempsec ver time) = do
    accesstoken :: Int64 <- random
    accesssecret <- APISecret <$> random
    kPrepare (   "INSERT INTO oauth_access_token (access_token, access_secret, api_token_id, user_id, created, status) "
              ++ "SELECT ?, ?, c.api_token_id, u.id, ?, ? FROM oauth_temp_credential c "
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
                  , toSql APIEnabled
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
