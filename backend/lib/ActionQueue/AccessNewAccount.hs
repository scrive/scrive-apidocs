module ActionQueue.AccessNewAccount (
    AccessNewAccount
  , accessNewAccount
  , getAccessNewAccountUser
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Data.Typeable

import ActionQueue.Core
import ActionQueue.Scheduler
import ActionQueue.Tables
import DB
import KontraPrelude
import MagicHash
import MinutesTime
import User.Model

data AccessNewAccount = AccessNewAccount {
    aUserID :: UserID
  , aExpires :: UTCTime
  , aToken :: MagicHash
  } deriving (Show, Typeable)

accessNewAccount :: Action UserID AccessNewAccount (UserID, MagicHash) Scheduler
accessNewAccount = Action {
    qaTable = tableAccessNewAccounts
  , qaSetFields = \(uid, token) -> do
      sqlSet "user_id" uid
      sqlSet "token" token
  , qaSelectFields = ["user_id", "expires", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = \(user_id, expires, token) -> AccessNewAccount {
        aUserID = user_id
      , aExpires = expires
      , aToken = token
      }
  , qaUpdateSQL = \AccessNewAccount{..} -> toSQLCommand $ sqlUpdate "access_new_accounts" $ do
      sqlSet "expires" aExpires
      sqlSet "token" aToken
      sqlWhereEq (qaIndexField accessNewAccount) aUserID
  , qaEvaluateExpired = \AccessNewAccount{aUserID} -> do
    _ <- dbUpdate $ DeleteAction accessNewAccount aUserID
    return ()
  }

getAccessNewAccountUser :: (MonadDB m, MonadThrow m, MonadTime m) => UserID -> MagicHash -> m (Maybe User)
getAccessNewAccountUser uid token = runMaybeT $ do
  Just a <- dbQuery $ GetAction accessNewAccount uid
  guard $ aToken a == token
  Just user <- dbQuery $ GetUserByID $ aUserID a
  return user
