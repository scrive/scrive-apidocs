module ActionQueue.UserAccountRequest (
    UserAccountRequest(..)
  , userAccountRequest
  , getUserAccountRequestUser
  , newUserAccountRequest
  , newUserAccountRequestLink
  ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Monoid
import Data.Typeable
import Control.Applicative
import Control.Monad.Reader

import ActionQueue.Core
import ActionQueue.Scheduler
import ActionQueue.Tables
import AppConf
import Crypto.RNG
import DB
import KontraLink
import MagicHash
import MinutesTime
import Stats.Model
import User.Model
import qualified Log
import Recurly
import Payments.Model
import Payments.Config

import Utils.IO

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
    ] <> SQL ("WHERE " ++ qaIndexField userAccountRequest ++ " = ?") [toSql uarUserID]
  , qaEvaluateExpired = \UserAccountRequest{uarUserID} -> do
    mplan <- dbQuery $ GetPaymentPlanInactiveUser uarUserID
    case mplan of
      Nothing -> do
        _ <- dbUpdate $ DeleteAction userAccountRequest uarUserID
        _ <- dbUpdate $ RemoveInactiveUserLoginEvents uarUserID
        success <- dbUpdate $ RemoveInactiveUser uarUserID
        when success $
          Log.debug $ "Inactive user (no plan) with id = " ++ show uarUserID ++ " successfully removed from database"
      Just plan | ppPaymentPlanProvider plan == RecurlyProvider -> do
        rc <- recurlyConfig <$> sdAppConf <$> ask
        esub <- liftIO $ getSubscriptionsForAccount curl_exe (recurlyAPIKey rc) (show $ ppAccountCode plan)
        case esub of
          Left msg -> Log.debug $ "Could not get subscription from Recurly with account code = " ++ (show $ ppAccountCode plan) ++ "; failed with message: " ++ msg
          Right [sub] -> do
            eres <- liftIO $ terminateSubscription curl_exe (recurlyAPIKey rc) (subID sub) FullRefund
            case eres of
              Left msg2 -> Log.debug $ "Cound not terminate subscription from Recurly with account code = " ++ (show $ ppAccountCode plan) ++ "; failed with message: " ++ msg2
              Right _ -> do -- afraid to delete user request until this point! -- Eric
                Log.debug $ "Successfully terminated subscription from recurly with account code = " ++ (show $ ppAccountCode plan)
                _ <- dbUpdate $ DeletePaymentPlan (Left uarUserID)
                _ <- dbUpdate $ DeleteAction userAccountRequest uarUserID
                _ <- dbUpdate $ RemoveInactiveUserLoginEvents uarUserID
                success <- dbUpdate $ RemoveInactiveUser uarUserID
                when success $
                  Log.debug $ "Inactive user (Recurly plan) with id = " ++ show uarUserID ++ " successfully removed from database"
          Right _ -> do
            Log.debug $ "Multiple subscriptions found from Recurly with account code = " ++ (show $ ppAccountCode plan) ++ "; failing."
      Just _ -> do
        -- No need to mess with recurly! -- Eric
        _ <- dbUpdate $ DeletePaymentPlan (Left uarUserID)
        _ <- dbUpdate $ DeleteAction userAccountRequest uarUserID
        _ <- dbUpdate $ RemoveInactiveUserLoginEvents uarUserID
        success <- dbUpdate $ RemoveInactiveUser uarUserID
        when success $
          Log.debug $ "Inactive user (no provider plan) with id = " ++ show uarUserID ++ " successfully removed from database"
       
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
  _ <- dbUpdate $ DeleteAction userAccountRequest uid
  dbUpdate $ NewAction userAccountRequest expires (uid, token)

newUserAccountRequestLink :: (MonadDB m, CryptoRNG m) => UserID -> m KontraLink
newUserAccountRequestLink uid = do
  uar <- newUserAccountRequest uid
  return $ LinkAccountCreated (uarUserID uar) (uarToken uar)
