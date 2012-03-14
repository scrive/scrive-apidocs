module ScriveByMail.Model 
    (
     MailAPIInfo(..),
     DeleteMailAPIDelays(..),
     AddMailAPIDelay(..),
     GetMailAPIUserRequest(..),
     GetMailAPIDelaysForEmail(..),
     ConfirmBossDelay(..),
     DelayStatus(..),

     GetUserMailAPI(..),
     SetUserMailAPIKey(..),
     IncrementUserMailAPI(..),
     RemoveUserMailAPI(..),
     ResetUserMailAPI(..),

     GetCompanyMailAPI(..),
     SetCompanyMailAPIKey(..),
     IncrementCompanyMailAPI(..),
     RemoveCompanyMailAPI(..),
     ResetCompanyMailAPI(..)

    )  
    where

import MagicHash
import DB.Classes
import DB.Derive
import DB.Fetcher2
import DB.Utils
import MinutesTime
import qualified Data.ByteString as BS
import Company.Model
import User.Model
import qualified Log

import Data.Maybe
import Database.HDBC
import Data.Int
import Crypto.RNG(random)
import Control.Applicative

data MailAPIInfo = MailAPIInfo {
    umapiKey          :: MagicHash
  , umapiDailyLimit   :: Int32
  , umapiSentToday    :: Int32
  } deriving (Eq, Ord, Show)

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

data ResetUserMailAPI = ResetUserMailAPI UserID
instance DBUpdate ResetUserMailAPI Bool where
  dbUpdate (ResetUserMailAPI uid) =
    (== 1) <$> kRun (SQL "UPDATE user_mail_apis SET sent_today = 0 WHERE user_id = ?" [toSql uid])

data IncrementUserMailAPI = IncrementUserMailAPI UserID
instance DBUpdate IncrementUserMailAPI (Maybe Int) where
  dbUpdate (IncrementUserMailAPI uid) = 
    getOne $ SQL ("UPDATE user_mail_apis SET "
               ++ " sent_today = (CASE WHEN last_sent_date = now()::DATE THEN sent_today + 1 ELSE 1 END) "
               ++ ",last_sent_date = now() "
               ++ "WHERE user_id = ? "
               ++ "RETURNING sent_today")
                 [toSql uid]

data SetUserMailAPIKey = SetUserMailAPIKey UserID MagicHash Int32
instance DBUpdate SetUserMailAPIKey Bool where
  dbUpdate (SetUserMailAPIKey uid key limit) = do
        kPrepare $ "UPDATE user_mail_apis SET "
                ++ " key = ? "
                ++ ",daily_limit = ? "
                ++ "WHERE user_id = ?"
        r <- kExecute [ toSql key
                      , toSql limit
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
                  ++ "SELECT ?, ?, ?, 0, now() "
                  ++ "WHERE NOT EXISTS (SELECT 1 FROM user_mail_apis WHERE user_id = ?) "
                  ++ "AND EXISTS (SELECT 1 FROM users WHERE id = ?)"
          kExecute01 [ toSql uid
                     , toSql key
                     , toSql limit
                     , toSql uid
                     , toSql uid
                     ]

data RemoveUserMailAPI = RemoveUserMailAPI UserID
instance DBUpdate RemoveUserMailAPI Bool where
  dbUpdate (RemoveUserMailAPI uid) =
    (== 1) <$> kRun (SQL "DELETE FROM user_mail_apis WHERE user_id = ?" [toSql uid])



data GetCompanyMailAPI = GetCompanyMailAPI CompanyID
instance DBQuery GetCompanyMailAPI (Maybe MailAPIInfo) where
  dbQuery (GetCompanyMailAPI cid) = do
    kPrepare "SELECT key, daily_limit, (CASE WHEN last_sent_date = now()::DATE THEN sent_today ELSE 0 END) FROM company_mail_apis WHERE company_id = ?"
    _ <- kExecute [toSql cid]
    foldDB fetchCompanyMailAPIs [] >>= oneObjectReturnedGuard
    where
      fetchCompanyMailAPIs acc key daily_limit sent_today = MailAPIInfo {
          umapiKey = key
        , umapiDailyLimit = daily_limit
        , umapiSentToday = sent_today
        } : acc

data ResetCompanyMailAPI = ResetCompanyMailAPI CompanyID
instance DBUpdate ResetCompanyMailAPI Bool where
  dbUpdate (ResetCompanyMailAPI cid) =
    (== 1) <$> kRun (SQL "UPDATE company_mail_apis SET sent_today = 0 WHERE company_id = ?" [toSql cid])

data IncrementCompanyMailAPI = IncrementCompanyMailAPI CompanyID
instance DBUpdate IncrementCompanyMailAPI (Maybe Int) where
  dbUpdate (IncrementCompanyMailAPI cid) = 
    getOne $ SQL ("UPDATE company_mail_apis SET "
               ++ " sent_today = (CASE WHEN last_sent_date = now()::DATE THEN sent_today + 1 ELSE 1 END) "
               ++ ",last_sent_date = now() "
               ++ "WHERE company_id = ? "
               ++ "RETURNING sent_today")
                 [toSql cid]

data SetCompanyMailAPIKey = SetCompanyMailAPIKey CompanyID MagicHash Int32
instance DBUpdate SetCompanyMailAPIKey Bool where
  dbUpdate (SetCompanyMailAPIKey cid key limit) = do
        kPrepare $ "UPDATE company_mail_apis SET "
                ++ " key = ? "
                ++ ",daily_limit = ? "
                ++ "WHERE company_id = ?"
        r <- kExecute [ toSql key
                      , toSql limit
                      , toSql cid
                      ]
        if r == 1
         then return True
         else do
          kPrepare $ "INSERT INTO company_mail_apis ("
                  ++ "  company_id"
                  ++ ", key"
                  ++ ", daily_limit"
                  ++ ", sent_today"
                  ++ ", last_sent_date"
                  ++ ") "
                  ++ "SELECT ?, ?, ?, 0, now() "
                  ++ "WHERE NOT EXISTS (SELECT 1 FROM company_mail_apis WHERE company_id = ?) "
                  ++ "AND EXISTS (SELECT 1 FROM companies WHERE id = ?)"
          kExecute01 [ toSql cid
                     , toSql key
                     , toSql limit
                     , toSql cid
                     , toSql cid
                     ]

data RemoveCompanyMailAPI = RemoveCompanyMailAPI CompanyID
instance DBUpdate RemoveCompanyMailAPI Bool where
  dbUpdate (RemoveCompanyMailAPI cid) =
    (== 1) <$> kRun (SQL "DELETE FROM company_mail_apis WHERE company_id = ?" [toSql cid])




data DelayStatus = DelayWaitAdmin
                 | DelayWaitUser
$(enumDeriveConvertible ''DelayStatus)

{- |
   Create a new Delay to request confirmation from the BOSS.
-}
data AddMailAPIDelay = AddMailAPIDelay String BS.ByteString CompanyID MinutesTime
instance DBUpdate AddMailAPIDelay (Maybe (Int64, MagicHash, Bool)) where
  dbUpdate (AddMailAPIDelay email text cid now) = do
    key :: MagicHash <- random
    kPrepare $ "INSERT INTO mail_api_user_request ("
                 ++ " key"
                 ++ ",email"
                 ++ ",time"
                 ++ ",expires"
                 ++ ",status"
                 ++ ",company_id"
                 ++ ") SELECT ?,?,?,?,?,? "
                 ++ "WHERE NOT EXISTS (SELECT 1 FROM mail_api_user_request WHERE email = ? AND company_id = ?) "
    r <- kExecute [ toSql key
                  , toSql email
                  , toSql now
                  , toSql $ 3 `daysAfter` now
                  , toSql DelayWaitAdmin
                  , toSql cid
                  , toSql email
                  , toSql cid]
    Log.debug $ "number of rows AddMailAPIDelay: " ++ show r
    _ <- kRun $ SQL "SELECT id, key FROM mail_api_user_request WHERE email = ? AND company_id = ?"
                      [toSql email, toSql cid]
    a <- listToMaybe <$> foldDB f []
    case a of
      Nothing -> return Nothing
      Just (requestid, key') -> do
        kPrepare $ "INSERT INTO mail_api_delay ("
                     ++ " email_text"
                     ++ ",user_request_id"
                     ++ ") VALUES (?,?) "
        _ <- kExecute [ toSql text
                      , toSql requestid]
        return $ Just (requestid, key', r == 1) -- r is 1 when user_request is new
    where f acc delayid key = (delayid, key):acc
          
data GetMailAPIUserRequest = GetMailAPIUserRequest Int64 MagicHash MinutesTime
instance DBQuery GetMailAPIUserRequest (Maybe (String, CompanyID)) where
  dbQuery (GetMailAPIUserRequest delayid key now) = do
    kPrepare $ "SELECT email, company_id FROM mail_api_user_request "
            ++ "WHERE id = ? AND key = ? AND expires >= ? AND status = ?"
    _ <- kExecute [toSql delayid, toSql key, toSql now, toSql DelayWaitAdmin]
    foldDB f [] >>= oneObjectReturnedGuard
    where f acc email cid = (email, cid):acc

data DeleteMailAPIDelays = DeleteMailAPIDelays Int64 MinutesTime
instance DBUpdate DeleteMailAPIDelays () where
  dbUpdate (DeleteMailAPIDelays delayid now) = do
    kPrepare $ "DELETE FROM mail_api_user_request "
            ++ "WHERE id = ? "
            ++ "   OR expires < ?"
    _ <- kExecute [toSql delayid, toSql now]
    return ()

data GetMailAPIDelaysForEmail = GetMailAPIDelaysForEmail String MinutesTime
instance DBQuery GetMailAPIDelaysForEmail (Maybe (Int64, [BS.ByteString])) where
  dbQuery (GetMailAPIDelaysForEmail email now) = do
    kPrepare $ "SELECT mail_api_user_request.id, mail_api_delay.email_text FROM mail_api_user_request "
            ++ "JOIN mail_api_delay ON mail_api_delay.user_request_id = mail_api_user_request.id "
            ++ "WHERE email = ? AND expires >= ? AND status = ?"
    _ <- kExecute [toSql email, toSql now, toSql DelayWaitUser]
    foldDB f Nothing
    where f Nothing i e = Just (i, [e])
          f (Just (i, acc)) _ e = Just (i, e:acc)

data ConfirmBossDelay = ConfirmBossDelay Int64 MinutesTime
instance DBUpdate ConfirmBossDelay Bool where
  dbUpdate (ConfirmBossDelay delayid now) = do
    (==1) <$> (kRun $ SQL "UPDATE mail_api_user_request SET status = ?, expires = ? WHERE id = ? and expires >= ?" 
                        [toSql DelayWaitUser, toSql $ 30 `daysAfter` now, toSql delayid, toSql now])