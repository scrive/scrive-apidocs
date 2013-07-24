module ActionQueue.UserAccountRequest (
    UserAccountRequest(..)
  , userAccountRequest
  , getUserAccountRequestUser
  , newUserAccountRequest
  , newUserAccountRequestLink
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
import qualified Log


data UserAccountRequest = UserAccountRequest {
    uarUserID :: UserID
  , uarExpires :: MinutesTime
  , uarToken :: MagicHash
  } deriving (Show, Typeable)

userAccountRequest :: Action UserID UserAccountRequest (UserID, MagicHash) Scheduler
userAccountRequest = Action {
    qaTable = tableUserAccountRequests
  , qaFields = \(user_id, token) -> [
      ("user_id", toSql user_id)
    , ("token", toSql token)
    ]
  , qaSelectFields = ["user_id", "expires", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = kFold decoder []
  , qaUpdateSQL = \UserAccountRequest{..} -> toSQLCommand $ sqlUpdate "user_account_requests" $ do
      sqlSet "expires" uarExpires
      sqlSet "token" uarToken
      sqlWhereEq (qaIndexField userAccountRequest) uarUserID
  , qaEvaluateExpired = \UserAccountRequest{uarUserID} -> do
      _ <- dbUpdate $ DeleteAction userAccountRequest uarUserID
      success <- dbUpdate $ RemoveInactiveUser uarUserID
      when success $
            Log.debug $ "Inactive user (no plan) with id = " ++ show uarUserID ++ " successfully removed from database"

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
  expires <- (14 `daysAfter`) `liftM` getMinutesTime
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

newUserAccountRequestLink :: (MonadDB m, CryptoRNG m) => Lang -> UserID -> SignupMethod -> m KontraLink
newUserAccountRequestLink lang uid sm = do
  uar <- newUserAccountRequest uid
  return $ LinkAccountCreated lang (uarUserID uar) (uarToken uar) sm
