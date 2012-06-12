module ActionQueue.UserAccountRequest where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Typeable

import ActionQueue.Core
import ActionQueue.Scheduler
import ActionQueue.Tables
import Crypto.RNG
import DB
import KontraLink
import MagicHash
import MinutesTime
import Misc
import User.Model

data UserAccountRequest = UserAccountRequest {
    uarUserID :: UserID
  , uarExpires :: MinutesTime
  , uarToken :: MagicHash
  } deriving (Show, Typeable)

userAccountRequest :: Action UserID UserAccountRequest (UserID, MagicHash) Scheduler
userAccountRequest = Action {
    qaTable = tableUserAccountRequests
  , qaFields = \(user_id, token) -> [
      sql "user_id" user_id
    , sql "token" token
    ]
  , qaSelectFields = ["user_id", "expires", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = foldDB decoder []
  , qaUpdateSQL = \UserAccountRequest{..} -> mkSQL UPDATE tableUserAccountRequests [
      sql "expires" uarExpires
    , sql "token" uarToken
    ] <++> SQL ("WHERE " ++ qaIndexField userAccountRequest ++ " = ?") [toSql uarUserID]
  , qaEvaluateExpired = \UserAccountRequest{uarUserID} -> do
    _ <- dbUpdate $ DeleteAction userAccountRequest uarUserID
    return ()
  }
  where
    decoder acc user_id expires token = UserAccountRequest {
        uarUserID = user_id
      , uarExpires = expires
      , uarToken = token
      } : acc

getUserAccountRequestUser :: MonadDB m => UserID -> MagicHash -> m (Maybe User)
getUserAccountRequestUser uid token = runMaybeT $ do
  Just UserAccountRequest{..} <- dbQuery $ GetAction userAccountRequest uid
  guard $ uarToken == token
  Just user <- dbQuery $ GetUserByID uarUserID
  return user

newUserAccountRequest :: (MonadDB m, CryptoRNG m) => UserID -> m UserAccountRequest
newUserAccountRequest uid = do
  token <- random
  expires <- minutesAfter (14*24*60) `liftM` getMinutesTime
  -- FIXME: highly unlikely, but possible race condition
  _ <- dbUpdate $ DeleteAction userAccountRequest uid
  dbUpdate $ NewAction userAccountRequest expires (uid, token)

newUserAccountRequestLink :: (MonadDB m, CryptoRNG m) => UserID -> m KontraLink
newUserAccountRequestLink uid = do
  uar <- newUserAccountRequest uid
  return $ LinkAccountCreated (uarUserID uar) (uarToken uar)
