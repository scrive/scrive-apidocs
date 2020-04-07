module User.PasswordReminder (
    PasswordReminder(..)
  , getPasswordReminderUser
  , newPasswordReminder
  , newPasswordReminderLink
  , GetPasswordReminder(..)
  , DeletePasswordReminder(..)
  , UpdatePasswordReminder(..)
  , DeleteExpiredPasswordReminders(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Crypto.RNG
import Data.Int
import Data.Typeable

import DB
import KontraLink
import MagicHash
import MinutesTime
import User.Model

data PasswordReminder = PasswordReminder
  { prUserID :: UserID
  , prExpires :: UTCTime
  , prRemainedEmails :: Int32
  , prToken :: MagicHash
  } deriving (Show, Typeable)

getPasswordReminderUser
  :: (MonadDB m, MonadThrow m, MonadTime m) => UserID -> MagicHash -> m (Maybe User)
getPasswordReminderUser uid token = runMaybeT $ do
  Just pr <- dbQuery $ GetPasswordReminder uid
  guard $ prToken pr == token
  Just user <- dbQuery . GetUserByID $ prUserID pr
  return user

newPasswordReminder
  :: (MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m) => UserID -> m PasswordReminder
newPasswordReminder uid = do
  token   <- random
  expires <- minutesAfter (12 * 60) <$> currentTime
  dbUpdate . CreatePasswordReminder $ PasswordReminder uid expires 9 token

newPasswordReminderLink
  :: (MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m) => UserID -> m KontraLink
newPasswordReminderLink uid = do
  pr <- newPasswordReminder uid
  return $ LinkPasswordReminder (prUserID pr) (prToken pr)

selectPasswordReminderSelectorsList :: [SQL]
selectPasswordReminderSelectorsList = ["user_id", "expires", "remained_emails", "token"]

data DeleteExpiredPasswordReminders = DeleteExpiredPasswordReminders
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m DeleteExpiredPasswordReminders () where
  dbUpdate DeleteExpiredPasswordReminders = do
    now <- currentTime
    runQuery_ . sqlDelete "password_reminders" $ sqlWhere ("expires <" <?> now)

newtype GetPasswordReminder = GetPasswordReminder UserID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetPasswordReminder (Maybe PasswordReminder) where
  dbQuery (GetPasswordReminder user_id) = do
    now <- currentTime
    runQuery_ . sqlSelect "password_reminders" $ do
      mapM_ sqlResult selectPasswordReminderSelectorsList
      sqlWhereEq "user_id" user_id
      sqlWhere $ "expires >=" <?> now
    fetchMaybe fetchPasswordReminder

newtype CreatePasswordReminder = CreatePasswordReminder PasswordReminder
instance (MonadDB m, MonadThrow m) => DBUpdate m CreatePasswordReminder PasswordReminder where
  dbUpdate (CreatePasswordReminder PasswordReminder {..}) = do
    runQuery_ . sqlInsert "password_reminders" $ do
      sqlSet "user_id"         prUserID
      sqlSet "expires"         prExpires
      sqlSet "remained_emails" prRemainedEmails
      sqlSet "token"           prToken
      mapM_ sqlResult selectPasswordReminderSelectorsList
    fetchOne fetchPasswordReminder

newtype UpdatePasswordReminder = UpdatePasswordReminder PasswordReminder
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdatePasswordReminder Bool where
  dbUpdate (UpdatePasswordReminder PasswordReminder {..}) = do
    runQuery01 . sqlUpdate "password_reminders" $ do
      sqlSet "expires"         prExpires
      sqlSet "remained_emails" prRemainedEmails
      sqlSet "token"           prToken
      sqlWhereEq "user_id" prUserID

newtype DeletePasswordReminder = DeletePasswordReminder UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeletePasswordReminder Bool where
  dbUpdate (DeletePasswordReminder user_id) = do
    runQuery01 . sqlDelete "password_reminders" $ do
      sqlWhereEq "user_id" user_id

fetchPasswordReminder :: (UserID, UTCTime, Int32, MagicHash) -> PasswordReminder
fetchPasswordReminder (user_id, expires, remained_emails, token) = PasswordReminder
  { prUserID         = user_id
  , prExpires        = expires
  , prRemainedEmails = remained_emails
  , prToken          = token
  }
