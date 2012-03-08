module ScriveByMail.Model 
    (
     MailAPIInfo(..),
     DeleteMailAPIDelays(..),
     GetCompanyMailAPI(..),
     SetCompanyMailAPI(..),
     AddMailAPIDelay(..),
     GetMailAPIDelay(..),
     GetUserMailAPI(..),
     SetUserMailAPI(..)
    )  
    where

import MagicHash
import DB.Classes
import DB.Fetcher2
import DB.Utils
import MinutesTime
import qualified Data.ByteString as BS
import Company.Model
import User.Model

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
        -- Update handles case where there's already a row (we only want one row per company)
        -- Insert handles case where there's no row yet (and checks that the company_id exists)
        -- This avoids a table lock
        kPrepare $ "UPDATE company_mail_apis SET "
                ++ " key = ? "
                ++ ",daily_limit = ? "
                ++ ",sent_today = ? "
                ++ ",last_sent_date = now() "
                ++ "WHERE company_id = ? ; "
                ++ "INSERT INTO company_mail_apis ("
                ++ "  company_id"
                ++ ", key"
                ++ ", daily_limit"
                ++ ", sent_today"
                ++ ", last_sent_date"
                ++ ") "
                ++ "SELECT ?, ?, ?, ?, now() "
                ++ "WHERE NOT EXISTS (SELECT 1 FROM company_mail_apis WHERE company_id = ?) "
                ++ "AND EXISTS (SELECT 1 FROM companies WHERE id = ?)"
        kExecute01 [
                     toSql $ umapiKey mailapi
                   , toSql $ umapiDailyLimit mailapi
                   , toSql $ umapiSentToday mailapi
                   , toSql cid
                   , toSql cid
                   , toSql $ umapiKey mailapi
                   , toSql $ umapiDailyLimit mailapi
                   , toSql $ umapiSentToday mailapi
                   , toSql cid
                   , toSql cid
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
    where f acc delayid key = (delayid, key):acc
          
data GetMailAPIDelay = GetMailAPIDelay Int64 MagicHash MinutesTime
instance DBQuery GetMailAPIDelay (Maybe (String, BS.ByteString)) where
  dbQuery (GetMailAPIDelay delayid key now) = do
    kPrepare $ "SELECT email, email_text FROM mail_api_delay "
            ++ "WHERE id = ? AND key = ? AND expires <= ?"
    _ <- kExecute [toSql delayid, toSql key, toSql now]
    foldDB f [] >>= oneObjectReturnedGuard
    where f acc email text = (email, text):acc

data DeleteMailAPIDelays = DeleteMailAPIDelays Int64 MagicHash MinutesTime
instance DBUpdate DeleteMailAPIDelays () where
  dbUpdate (DeleteMailAPIDelays delayid key now) = do
    kPrepare $ "DELETE FROM mail_api_delay "
            ++ "WHERE (id = ? AND key = ?)"
            ++ "   OR expires > ?"
    _ <- kExecute [toSql delayid, toSql key, toSql now]
    return ()

data GetUserMailAPI = GetUserMailAPI UserID
instance DBQuery GetUserMailAPI (Maybe MailAPIInfo) where
  dbQuery (GetUserMailAPI uid) = do
    kPrepare "SELECT key, daily_limit, (CASE WHEN last_sent_date = now()::DATE THEN sent_today ELSE 0 END) FROM user_mail_apis WHERE user_id = ?"
    _ <- kExecute [toSql uid]
    foldDB fetchUserMailAPIs [] >>= oneObjectReturnedGuard
    where
      fetchUserMailAPIs acc key daily_limit sent_today = MailAPIInfo {
          umapiKey = key
        , umapiDailyLimit = daily_limit
        , umapiSentToday = sent_today
        } : acc

--- these need to be changed; the semantics of it is incrementing for today;
-- this has the possibility of a race condition with mails sent and changing your settings
-- we need to fix this using an if statement to see if it's today
-- also take a now time, don't user now()
data SetUserMailAPI = SetUserMailAPI UserID (Maybe MailAPIInfo)
instance DBUpdate SetUserMailAPI Bool where
  dbUpdate (SetUserMailAPI uid mmailapi) =
    case mmailapi of
      Just mailapi -> do
        -- Update handles case where there's already a row (we only want one row per company)
        -- Insert handles case where there's no row yet (and checks that the company_id exists)
        -- This avoids a table lock and there are no problems if there is race condition
        -- possible orders of execution
        -- u1,i1,u2,i2 => no problem, will be value of 2
        -- u2,i2,u1,i1 => no problem, will be value of 2
        -- u1,u2,i1,i2 => no problem, will be value of 2
        -- u2,u1,i2,i1 => will be value of 2
        -- 
        kPrepare $ "UPDATE user_mail_apis SET "
                ++ " key = ? "
                ++ ",daily_limit = ? "
                ++ ",sent_today = ? "
                ++ ",last_sent_date = now() "
                ++ "WHERE user_id = ?"
        r <- kExecute [ toSql $ umapiKey mailapi
                      , toSql $ umapiDailyLimit mailapi
                      , toSql $ umapiSentToday mailapi
                      , toSql uid
                      ]
        if r == 1
         then return True
         else do
          kPrepare $ "INSERT INTO user_mail_apis ("
                  ++ "  user_id"
                  ++ ", key"
                  ++ ", daily_limit"
                  ++ ", sent_today"
                  ++ ", last_sent_date"
                  ++ ") "
                  ++ "SELECT ?, ?, ?, ?, now() "
                  ++ "WHERE NOT EXISTS (SELECT 1 FROM user_mail_apis WHERE user_id = ?) "
                  ++ "AND EXISTS (SELECT 1 FROM users WHERE id = ?)"
          kExecute01 [ toSql uid
                     , toSql $ umapiKey mailapi
                     , toSql $ umapiDailyLimit mailapi
                     , toSql $ umapiSentToday mailapi
                     , toSql uid
                     , toSql uid
                     ]
      Nothing -> do
        kPrepare "DELETE FROM user_mail_apis WHERE user_id = ?"
        kExecute01 [toSql uid]