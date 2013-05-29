module ActionQueue.AccessNewAccount (
    AccessNewAccount
  , accessNewAccount
  , getAccessNewAccountUser
  , newAccessNewAccount
  , newAccessNewAccountLink
  ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Typeable

import ActionQueue.Core
import ActionQueue.Scheduler
import ActionQueue.Tables
import Crypto.RNG
import DB
import DB.SQL2
import KontraLink
import MagicHash
import MinutesTime
import User.Model

data AccessNewAccount = AccessNewAccount {
    aUserID :: UserID
  , aExpires :: MinutesTime
  , aToken :: MagicHash
  } deriving (Show, Typeable)

accessNewAccount :: Action UserID AccessNewAccount (UserID, MagicHash) Scheduler
accessNewAccount = Action {
    qaTable = tableAccessNewAccounts
  , qaFields = \(uid, token) -> [
      sql "user_id" uid
    , sql "token" token
    ]
  , qaSelectFields = ["user_id", "expires", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = kFold decoder []
  , qaUpdateSQL = \AccessNewAccount{..} -> toSQLCommand $ sqlUpdate "access_new_accounts" $ do
      sqlSet "expires" aExpires
      sqlSet "token" aToken
      sqlWhereEq (qaIndexField accessNewAccount) aUserID
  , qaEvaluateExpired = \AccessNewAccount{aUserID} -> do
    _ <- dbUpdate $ DeleteAction accessNewAccount aUserID
    return ()
  }
  where
    decoder acc user_id expires token = AccessNewAccount {
        aUserID = user_id
      , aExpires = expires
      , aToken = token
      } : acc

getAccessNewAccountUser :: MonadDB m => UserID -> MagicHash -> m (Maybe User)
getAccessNewAccountUser uid token = runMaybeT $ do
  Just a <- dbQuery $ GetAction accessNewAccount uid
  guard $ aToken a == token
  Just user <- dbQuery $ GetUserByID $ aUserID a
  return user

newAccessNewAccount :: (MonadDB m, CryptoRNG m) => UserID -> m AccessNewAccount
newAccessNewAccount uid = do
  token <- random
  expires <- (14 `daysAfter`) `liftM` getMinutesTime
  dbUpdate $ NewAction accessNewAccount expires (uid, token)

newAccessNewAccountLink :: (MonadDB m, CryptoRNG m) => UserID -> m KontraLink
newAccessNewAccountLink uid = do
  a <- newAccessNewAccount uid
  return $ LinkAccessNewAccount (aUserID a) (aToken a)
