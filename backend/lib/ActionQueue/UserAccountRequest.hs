module ActionQueue.UserAccountRequest (
    UserAccountRequest(..)
  , userAccountRequest
  , getUserAccountRequestUser
  , newUserAccountRequest
  , newUserAccountRequestLink
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Data.Typeable
import Log

import ActionQueue.Core
import ActionQueue.Scheduler
import ActionQueue.Tables
import Crypto.RNG
import DB
import KontraLink
import KontraPrelude
import Log.Identifier
import MagicHash
import MinutesTime
import User.Model

data UserAccountRequest = UserAccountRequest {
    uarUserID :: UserID
  , uarExpires :: UTCTime
  , uarToken :: MagicHash
  } deriving (Show, Typeable)

userAccountRequest :: Action UserID UserAccountRequest (UserID, MagicHash) Scheduler
userAccountRequest = Action {
    qaTable = tableUserAccountRequests
  , qaSetFields = \(user_id, token) -> do
      sqlSet "user_id" user_id
      sqlSet "token" token
  , qaSelectFields = ["user_id", "expires", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = \(user_id, expires, token) -> UserAccountRequest {
      uarUserID = user_id
    , uarExpires = expires
    , uarToken = token
    }
  , qaUpdateSQL = \UserAccountRequest{..} -> toSQLCommand $ sqlUpdate "user_account_requests" $ do
      sqlSet "expires" uarExpires
      sqlSet "token" uarToken
      sqlWhereEq (qaIndexField userAccountRequest) uarUserID
  , qaEvaluateExpired = \UserAccountRequest{uarUserID} -> localData [identifier_ uarUserID] $ do
      _ <- dbUpdate $ DeleteAction userAccountRequest uarUserID
      musertos <- (fmap userhasacceptedtermsofservice) <$> dbQuery (GetUserByIDIncludeDeleted uarUserID)
      case musertos of
        Just Nothing -> do
          success <- dbUpdate $ RemoveInactiveUser uarUserID
          when success $
            logInfo_ "Inactive user (no plan) successfully removed from database"
        _ -> return ()
  }

getUserAccountRequestUser :: (MonadDB m, MonadThrow m, MonadTime m) => UserID -> MagicHash -> m (Maybe User)
getUserAccountRequestUser uid token = runMaybeT $ do
  Just UserAccountRequest{..} <- dbQuery $ GetAction userAccountRequest uid
  guard $ uarToken == token
  Just user <- dbQuery $ GetUserByID uarUserID
  return user

newUserAccountRequest :: (MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m) => UserID -> m UserAccountRequest
newUserAccountRequest uid = do
  token <- random
  let midnightInAMonth = (1 `secondsBefore`) . nextDayMidnight . (28 `daysAfter`)
  expires <- midnightInAMonth <$> currentTime
  -- FIXME: highly unlikely, but possible race condition
  ma <- dbQuery $ GetAction userAccountRequest uid
  case ma of
       Just a -> do
         let a' = a { uarExpires = expires }
         _ <- dbUpdate $ UpdateAction userAccountRequest a'
         return a'
       Nothing -> do
          _ <- dbUpdate $ DeleteAction userAccountRequest uid
          dbUpdate $ NewAction userAccountRequest expires (uid, token)

newUserAccountRequestLink :: (MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m) => Lang -> UserID -> SignupMethod -> m KontraLink
newUserAccountRequestLink lang uid sm = do
  uar <- newUserAccountRequest uid
  return $ LinkAccountCreated lang (uarUserID uar) (uarToken uar) sm
