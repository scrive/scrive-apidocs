module OAuth.Model where

import DB.Derive
import MinutesTime
import DB.Classes
import DB.Utils
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
    showsPrec _ token = (++) $ show (atToken token) ++ "-" ++ show (atID token)

instance Read APIToken where
    readsPrec p s = case break (== '-') s of
      (ts, '-':is) -> [(APIToken { atID = i, atToken = read ts }, v) 
                      | (i, v) <- readsPrec p is]
      _ -> []

newtype APISecret = APISecret { unAPISecret :: Int64 }
    deriving (Eq, Ord, Typeable, Data)
$(newtypeDeriveUnderlyingReadShow ''APISecret)
$(newtypeDeriveConvertible ''APISecret)

data APITokenStatus = APIEnabled
                    | APIDisabled

data APIPrivilege = APIUserCreate
                  | APIDocCreate

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
    verifier :: Int64 <- random
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

