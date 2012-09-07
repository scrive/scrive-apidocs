module ActionQueue.PasswordReminder (
    PasswordReminder(..)
  , passwordReminder
  , getPasswordReminderUser
  , newPasswordReminder
  , newPasswordReminderLink
  ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Int
import Data.Monoid
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
  , prExpires :: MinutesTime
  , prRemainedEmails :: Int32
  , prToken :: MagicHash
  } deriving (Show, Typeable)

passwordReminder :: Action UserID PasswordReminder (UserID, Int32, MagicHash) Scheduler
passwordReminder = Action {
    qaTable = tablePasswordReminders
  , qaFields = \(uid, remained_emails, token) -> [
      sql "user_id" uid
    , sql "remained_emails" remained_emails
    , sql "token" token
    ]
  , qaSelectFields = ["user_id", "expires", "remained_emails", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = foldDB decoder []
  , qaUpdateSQL = \PasswordReminder{..} -> mkSQL UPDATE tablePasswordReminders [
      sql "expires" prExpires
    , sql "remained_emails" prRemainedEmails
    , sql "token" prToken
    ] <> SQL ("WHERE " ++ qaIndexField passwordReminder ++ " = ?") [toSql prUserID]
  , qaEvaluateExpired = \PasswordReminder{prUserID} -> do
    _ <- dbUpdate $ DeleteAction passwordReminder prUserID
    return ()
  }
  where
    decoder acc user_id expires remained_emails token = PasswordReminder {
        prUserID = user_id
      , prExpires = expires
      , prRemainedEmails = remained_emails
      , prToken = token
      } : acc

getPasswordReminderUser :: MonadDB m => UserID -> MagicHash -> m (Maybe User)
getPasswordReminderUser uid token = runMaybeT $ do
  Just pr <- dbQuery $ GetAction passwordReminder uid
  guard $ prToken pr == token
  Just user <- dbQuery $ GetUserByID $ prUserID pr
  return user

newPasswordReminder :: (MonadDB m, CryptoRNG m) => UserID -> m PasswordReminder
newPasswordReminder uid = do
  token <- random
  expires <- minutesAfter (12*60) `liftM` getMinutesTime
  dbUpdate $ NewAction passwordReminder expires (uid, 9, token)

newPasswordReminderLink :: (MonadDB m, CryptoRNG m) => UserID -> m KontraLink
newPasswordReminderLink uid = do
  pr <- newPasswordReminder uid
  return $ LinkPasswordReminder (prUserID pr) (prToken pr)
