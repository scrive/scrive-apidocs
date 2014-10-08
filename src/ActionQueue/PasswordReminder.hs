module ActionQueue.PasswordReminder (
    PasswordReminder(..)
  , passwordReminder
  , getPasswordReminderUser
  , newPasswordReminder
  , newPasswordReminderLink
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Data.Int
import Data.Typeable

import ActionQueue.Core
import ActionQueue.Scheduler
import ActionQueue.Tables
import Crypto.RNG
import DB
import KontraLink
import MagicHash
import MinutesTime
import User.Model

data PasswordReminder = PasswordReminder {
    prUserID :: UserID
  , prExpires :: UTCTime
  , prRemainedEmails :: Int32
  , prToken :: MagicHash
  } deriving (Show, Typeable)

passwordReminder :: Action UserID PasswordReminder (UserID, Int32, MagicHash) Scheduler
passwordReminder = Action {
    qaTable = tablePasswordReminders
  , qaSetFields = \(uid, remained_emails, token) -> do
      sqlSet "user_id" uid
      sqlSet "remained_emails" remained_emails
      sqlSet "token" token
  , qaSelectFields = ["user_id", "expires", "remained_emails", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = \(user_id, expires, remained_emails, token) -> PasswordReminder {
      prUserID = user_id
    , prExpires = expires
    , prRemainedEmails = remained_emails
    , prToken = token
    }
  , qaUpdateSQL = \PasswordReminder{..} -> toSQLCommand $ sqlUpdate "password_reminders" $ do
      sqlSet "expires" prExpires
      sqlSet "remained_emails" prRemainedEmails
      sqlSet "token" prToken
      sqlWhereEq (qaIndexField passwordReminder) prUserID
  , qaEvaluateExpired = \PasswordReminder{prUserID} -> do
    _ <- dbUpdate $ DeleteAction passwordReminder prUserID
    return ()
  }

getPasswordReminderUser :: (MonadDB m, MonadThrow m) => UserID -> MagicHash -> m (Maybe User)
getPasswordReminderUser uid token = runMaybeT $ do
  Just pr <- dbQuery $ GetAction passwordReminder uid
  guard $ prToken pr == token
  Just user <- dbQuery $ GetUserByID $ prUserID pr
  return user

newPasswordReminder :: (MonadDB m, MonadThrow m, CryptoRNG m) => UserID -> m PasswordReminder
newPasswordReminder uid = do
  token <- random
  expires <- minutesAfter (12*60) `liftM` currentTime
  dbUpdate $ NewAction passwordReminder expires (uid, 9, token)

newPasswordReminderLink :: (MonadDB m, MonadThrow m, CryptoRNG m) => UserID -> m KontraLink
newPasswordReminderLink uid = do
  pr <- newPasswordReminder uid
  return $ LinkPasswordReminder (prUserID pr) (prToken pr)
