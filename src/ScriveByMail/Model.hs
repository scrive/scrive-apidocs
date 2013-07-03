module ScriveByMail.Model
    (
     MailAPIInfo(..),

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

import DB
import MagicHash
import Company.Model
import User.Model
import Control.Monad
import Data.Int

data MailAPIInfo = MailAPIInfo {
    umapiKey          :: MagicHash
  , umapiDailyLimit   :: Int32
  , umapiSentToday    :: Int32
  } deriving (Eq, Ord, Show)

data GetUserMailAPI = GetUserMailAPI UserID
instance MonadDB m => DBQuery m GetUserMailAPI (Maybe MailAPIInfo) where
  query (GetUserMailAPI uid) = do
    kRun_ $ SQL "SELECT key, daily_limit, (CASE WHEN last_sent_date = now()::DATE THEN sent_today ELSE 0 END) FROM user_mail_apis WHERE user_id = ?"
           [toSql uid]
    kFold fetchUserMailAPIs [] >>= oneObjectReturnedGuard
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
               <> " sent_today = (CASE WHEN last_sent_date = now()::DATE THEN sent_today + 1 ELSE 1 END) "
               <> ",last_sent_date = now() "
               <> "WHERE user_id = ? "
               <> "RETURNING sent_today")
                 [toSql uid]

data SetUserMailAPIKey = SetUserMailAPIKey UserID MagicHash Int32
instance MonadDB m => DBUpdate m SetUserMailAPIKey Bool where
  update (SetUserMailAPIKey uid key limit) = do
        r <- kRun $ SQL ("UPDATE user_mail_apis SET "
                         <> " key = ? "
                         <> ",daily_limit = ? "
                         <> "WHERE user_id = ?")
                      [ toSql key
                      , toSql limit
                      , toSql uid
                      ]
        if r == 1
         then return True
         else do
          kRun01 $ SQL ("INSERT INTO user_mail_apis ("
                        <> "  user_id"
                        <> ", key"
                        <> ", daily_limit"
                        <> ", sent_today"
                        <> ", last_sent_date"
                        <> ") "
                        <> "SELECT ?, ?, ?, 0, now() "
                        <> "WHERE NOT EXISTS (SELECT 1 FROM user_mail_apis WHERE user_id = ?) "
                        <> "AND EXISTS (SELECT 1 FROM users WHERE id = ?)")
                   [ toSql uid
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
    kRun_ $ SQL "SELECT key, daily_limit, (CASE WHEN last_sent_date = now()::DATE THEN sent_today ELSE 0 END) FROM company_mail_apis WHERE company_id = ?"
            [toSql cid]
    kFold fetchCompanyMailAPIs [] >>= oneObjectReturnedGuard
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
               <> " sent_today = (CASE WHEN last_sent_date = now()::DATE THEN sent_today + 1 ELSE 1 END) "
               <> ",last_sent_date = now() "
               <> "WHERE company_id = ? "
               <> "RETURNING sent_today")
                 [toSql cid]

data SetCompanyMailAPIKey = SetCompanyMailAPIKey CompanyID MagicHash Int32
instance MonadDB m => DBUpdate m SetCompanyMailAPIKey Bool where
  update (SetCompanyMailAPIKey cid key limit) = do
        r <- kRun $ SQL ("UPDATE company_mail_apis SET "
                         <> " key = ? "
                         <> ",daily_limit = ? "
                         <> "WHERE company_id = ?")
                      [ toSql key
                      , toSql limit
                      , toSql cid
                      ]
        if r == 1
         then return True
         else do
          kRun01 $ SQL( "INSERT INTO company_mail_apis ("
                        <> "  company_id"
                        <> ", key"
                        <> ", daily_limit"
                        <> ", sent_today"
                        <> ", last_sent_date"
                        <> ") "
                        <> "SELECT ?, ?, ?, 0, now() "
                        <> "WHERE NOT EXISTS (SELECT 1 FROM company_mail_apis WHERE company_id = ?) "
                        <> "AND EXISTS (SELECT 1 FROM companies WHERE id = ?)")
                   [ toSql cid
                   , toSql key
                   , toSql limit
                   , toSql cid
                   , toSql cid
                   ]

data RemoveCompanyMailAPI = RemoveCompanyMailAPI CompanyID
instance MonadDB m => DBUpdate m RemoveCompanyMailAPI Bool where
  update (RemoveCompanyMailAPI cid) =
    (== 1) `liftM` kRun (SQL "DELETE FROM company_mail_apis WHERE company_id = ?" [toSql cid])
