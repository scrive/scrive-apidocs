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

import Crypto.RNG
import DB.Classes
import DB.Derive
import DB.Fetcher2
import DB.Utils
import MagicHash
import MinutesTime
import qualified Data.ByteString as BS
import Company.Model
import User.Model
import qualified Log

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Database.HDBC
import Data.Int

data MailAPIInfo = MailAPIInfo {
    umapiKey          :: MagicHash
  , umapiDailyLimit   :: Int32
  , umapiSentToday    :: Int32
  } deriving (Eq, Ord, Show)

data GetUserMailAPI = GetUserMailAPI UserID
instance MonadDB m => DBQuery m GetUserMailAPI (Maybe MailAPIInfo) where
  query (GetUserMailAPI uid) = do
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
instance MonadDB m => DBUpdate m ResetUserMailAPI Bool where
  update (ResetUserMailAPI uid) =
    (== 1) `liftM` kRun (SQL "UPDATE user_mail_apis SET sent_today = 0 WHERE user_id = ?" [toSql uid])

data IncrementUserMailAPI = IncrementUserMailAPI UserID
instance MonadDB m => DBUpdate m IncrementUserMailAPI (Maybe Int) where
  update (IncrementUserMailAPI uid) =
    getOne $ SQL ("UPDATE user_mail_apis SET "
               ++ " sent_today = (CASE WHEN last_sent_date = now()::DATE THEN sent_today + 1 ELSE 1 END) "
               ++ ",last_sent_date = now() "
               ++ "WHERE user_id = ? "
               ++ "RETURNING sent_today")
                 [toSql uid]

data SetUserMailAPIKey = SetUserMailAPIKey UserID MagicHash Int32
instance MonadDB m => DBUpdate m SetUserMailAPIKey Bool where
  update (SetUserMailAPIKey uid key limit) = do
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
instance MonadDB m => DBUpdate m RemoveUserMailAPI Bool where
  update (RemoveUserMailAPI uid) =
    (== 1) `liftM` kRun (SQL "DELETE FROM user_mail_apis WHERE user_id = ?" [toSql uid])

data GetCompanyMailAPI = GetCompanyMailAPI CompanyID
instance MonadDB m => DBQuery m GetCompanyMailAPI (Maybe MailAPIInfo) where
  query (GetCompanyMailAPI cid) = do
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
instance MonadDB m => DBUpdate m ResetCompanyMailAPI Bool where
  update (ResetCompanyMailAPI cid) =
    (== 1) `liftM` kRun (SQL "UPDATE company_mail_apis SET sent_today = 0 WHERE company_id = ?" [toSql cid])

data IncrementCompanyMailAPI = IncrementCompanyMailAPI CompanyID
instance MonadDB m => DBUpdate m IncrementCompanyMailAPI (Maybe Int) where
  update (IncrementCompanyMailAPI cid) =
    getOne $ SQL ("UPDATE company_mail_apis SET "
               ++ " sent_today = (CASE WHEN last_sent_date = now()::DATE THEN sent_today + 1 ELSE 1 END) "
               ++ ",last_sent_date = now() "
               ++ "WHERE company_id = ? "
               ++ "RETURNING sent_today")
                 [toSql cid]

data SetCompanyMailAPIKey = SetCompanyMailAPIKey CompanyID MagicHash Int32
instance MonadDB m => DBUpdate m SetCompanyMailAPIKey Bool where
  update (SetCompanyMailAPIKey cid key limit) = do
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
instance MonadDB m => DBUpdate m RemoveCompanyMailAPI Bool where
  update (RemoveCompanyMailAPI cid) =
    (== 1) `liftM` kRun (SQL "DELETE FROM company_mail_apis WHERE company_id = ?" [toSql cid])

data DelayStatus = DelayWaitAdmin
                 | DelayWaitUser
$(enumDeriveConvertible ''DelayStatus)

{- |
   Create a new Delay to request confirmation from the BOSS.
-}
data AddMailAPIDelay = AddMailAPIDelay String BS.ByteString CompanyID MinutesTime
instance (CryptoRNG m, MonadDB m) => DBUpdate m AddMailAPIDelay (Maybe (Int64, MagicHash, Bool)) where
  update (AddMailAPIDelay email text cid now) = do
    key :: MagicHash <- lift random
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
    a <- listToMaybe `liftM` foldDB f []
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
instance MonadDB m => DBQuery m GetMailAPIUserRequest (Maybe (String, CompanyID)) where
  query (GetMailAPIUserRequest delayid key now) = do
    kPrepare $ "SELECT email, company_id FROM mail_api_user_request "
            ++ "WHERE id = ? AND key = ? AND expires >= ? AND status = ?"
    _ <- kExecute [toSql delayid, toSql key, toSql now, toSql DelayWaitAdmin]
    foldDB f [] >>= oneObjectReturnedGuard
    where f acc email cid = (email, cid):acc

data DeleteMailAPIDelays = DeleteMailAPIDelays Int64 MinutesTime
instance MonadDB m => DBUpdate m DeleteMailAPIDelays () where
  update (DeleteMailAPIDelays delayid now) = do
    kPrepare $ "DELETE FROM mail_api_user_request "
            ++ "WHERE id = ? "
            ++ "   OR expires < ?"
    _ <- kExecute [toSql delayid, toSql now]
    return ()

data GetMailAPIDelaysForEmail = GetMailAPIDelaysForEmail String MinutesTime
instance MonadDB m => DBQuery m GetMailAPIDelaysForEmail (Maybe (Int64, [BS.ByteString])) where
  query (GetMailAPIDelaysForEmail email now) = do
    kPrepare $ "SELECT mail_api_user_request.id, mail_api_delay.email_text FROM mail_api_user_request "
            ++ "JOIN mail_api_delay ON mail_api_delay.user_request_id = mail_api_user_request.id "
            ++ "WHERE email = ? AND expires >= ? AND status = ?"
    _ <- kExecute [toSql email, toSql now, toSql DelayWaitUser]
    foldDB f Nothing
    where f Nothing i e = Just (i, [e])
          f (Just (i, acc)) _ e = Just (i, e:acc)

data ConfirmBossDelay = ConfirmBossDelay Int64 MinutesTime
instance MonadDB m => DBUpdate m ConfirmBossDelay Bool where
  update (ConfirmBossDelay delayid now) = do
    (==1) `liftM` (kRun $ SQL "UPDATE mail_api_user_request SET status = ?, expires = ? WHERE id = ? and expires >= ?"
                        [toSql DelayWaitUser, toSql $ 30 `daysAfter` now, toSql delayid, toSql now])