module ScriveByMail.Model 
    (
     MailAPIInfo(..),
     DeleteMailAPIDelays(..),
     GetCompanyMailAPI(..),
     SetCompanyMailAPI(..),
     AddMailAPIDelay(..),
     GetMailAPIDelay(..)
    )  
    where

import MagicHash
import DB.Classes
import DB.Derive
import DB.Fetcher2
import DB.Types
import DB.Utils
import MinutesTime
import qualified Data.ByteString as BS
import Company.Model

import Database.HDBC
import Data.Int
import Crypto.RNG(random)

data MailAPIInfo = MailAPIInfo {
    umapiKey          :: MagicHash
  , umapiDailyLimit   :: Int32
  , umapiSentToday    :: Int32
  } deriving (Eq, Ord, Show)

data GetCompanyMailAPI = GetCompanyMailAPI CompanyID
instance DBQuery GetCompanyMailAPI (Maybe MailAPIInfo) where
  dbQuery (GetCompanyMailAPI cid) = do
    kPrepare "SELECT key, daily_limit, (CASE WHEN last_sent_date = now()::DATE THEN sent_today ELSE 0 END) FROM company_mail_apis WHERE company_id = ?"
    _ <- kExecute [toSql cid]
    foldDB fetchMailAPIs [] >>= oneObjectReturnedGuard
    where
      fetchMailAPIs acc key daily_limit sent_today = MailAPIInfo {
          umapiKey        = key
        , umapiDailyLimit = daily_limit
        , umapiSentToday  = sent_today
        } : acc

-- todo: make this one query delete/insert
data SetCompanyMailAPI = SetCompanyMailAPI CompanyID (Maybe MailAPIInfo)
instance DBUpdate SetCompanyMailAPI Bool where
  dbUpdate (SetCompanyMailAPI cid mmailapi) =
    case mmailapi of
      Just mailapi -> do
        kPrepare "DELETE FROM company_mail_apis WHERE company_id = ?"
        _ <- kExecute01 [toSql cid]
        kPrepare $ "INSERT INTO user_mail_apis ("
                ++ "  company_id"
                ++ ", key"
                ++ ", daily_limit"
                ++ ", sent_today"
                ++ ", last_sent_date"
                ++ ") "
                ++ "VALUES (?, ?, ?, ?, now()) "
        kExecute01 [
                     toSql cid
                   , toSql $ umapiKey mailapi
                   , toSql $ umapiDailyLimit mailapi
                   , toSql $ umapiSentToday mailapi
                   ]
      Nothing -> do
        kPrepare "DELETE FROM company_mail_apis WHERE company_id = ?"
        kExecute01 [toSql cid]

data AddMailAPIDelay = AddMailAPIDelay String BS.ByteString MinutesTime
instance DBUpdate AddMailAPIDelay (Int64, MagicHash) where
  dbUpdate (AddMailAPIDelay email text now) = do
    key :: MagicHash <- random
    kPrepare $ "INSERT INTO mail_api_delay ("
            ++ " key"
            ++ ",email"
            ++ ",email_text"
            ++ ",time"
            ++ ",expires"
            ++ ") VALUES (?,?,?,?) "
            ++ "RETURNING id, key"
    _ <- kExecute [toSql key, toSql email, toSql text, toSql now, toSql $ 3 `daysAfter` now]
    foldDB f [] >>= (return . head)
    where f acc id key = (id, key):acc
          
data GetMailAPIDelay = GetMailAPIDelay Int64 MagicHash MinutesTime
instance DBQuery GetMailAPIDelay (Maybe (String, BS.ByteString)) where
  dbQuery (GetMailAPIDelay id key now) = do
    kPrepare $ "SELECT email, email_text FROM mail_api_delay "
            ++ "WHERE id = ? AND key = ? AND expires <= ?"
    _ <- kExecute [toSql id, toSql key, toSql now]
    foldDB f [] >>= oneObjectReturnedGuard
    where f acc email text = (email, text):acc

data DeleteMailAPIDelays = DeleteMailAPIDelays Int64 MagicHash MinutesTime
instance DBUpdate DeleteMailAPIDelays () where
  dbUpdate (DeleteMailAPIDelays id key now) = do
    kPrepare $ "DELETE FROM mail_api_delay "
            ++ "WHERE (id = ? AND key = ?)"
            ++ "   OR expires > ?"
    _ <- kExecute [toSql id, toSql key, toSql now]
    return ()