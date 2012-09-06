module ActionQueue.EmailChangeRequest (
    EmailChangeRequest(..)
  , emailChangeRequest
  , getEmailChangeRequestNewEmail
  , newEmailChangeRequest
  , newEmailChangeRequestLink
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Typeable

import ActionQueue.Core
import ActionQueue.Scheduler
import ActionQueue.Tables
import Context
import Crypto.RNG
import DB
import KontraMonad
import KontraLink
import MagicHash
import MinutesTime
import Utils.Monoid
import User.Model

data EmailChangeRequest = EmailChangeRequest {
    ecrUserID :: UserID
  , ecrExpires :: MinutesTime
  , ecrNewEmail :: Email
  , ecrToken :: MagicHash
  } deriving (Show, Typeable)

emailChangeRequest :: Action UserID EmailChangeRequest (UserID, Email, MagicHash) Scheduler
emailChangeRequest = Action {
    qaTable = tableEmailChangeRequests
  , qaFields = \(uid, new_email, token) -> [
      sql "user_id" uid
    , sql "new_email" new_email
    , sql "token" token
    ]
  , qaSelectFields = ["user_id", "expires", "new_email", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = foldDB decoder []
  , qaUpdateSQL = \EmailChangeRequest{..} -> mkSQL UPDATE tableEmailChangeRequests [
      sql "expires" ecrExpires
    , sql "new_email" ecrNewEmail
    , sql "token" ecrToken
    ] <++> SQL ("WHERE " ++ qaIndexField emailChangeRequest ++ " = ?") [toSql ecrUserID]
  , qaEvaluateExpired = \EmailChangeRequest{ecrUserID} -> do
    _ <- dbUpdate $ DeleteAction emailChangeRequest ecrUserID
    return ()
  }
  where
    decoder acc user_id expires new_email token = EmailChangeRequest {
        ecrUserID = user_id
      , ecrExpires = expires
      , ecrNewEmail = new_email
      , ecrToken = token
      } : acc

getEmailChangeRequestNewEmail :: (MonadDB m, KontraMonad m) => UserID -> MagicHash -> m (Maybe Email)
getEmailChangeRequestNewEmail uid token = runMaybeT $ do
  Just EmailChangeRequest{..} <- dbQuery $ GetAction emailChangeRequest uid
  guard $ ecrToken == token
  Context{ctxmaybeuser = Just loggeduser} <- lift getContext
  guard $ ecrUserID == userid loggeduser
  -- check if a user didn't create an account with this email
  -- between requesting email change and confirming the change
  Nothing <- dbQuery $ GetUserByEmail Nothing ecrNewEmail
  return ecrNewEmail

newEmailChangeRequest :: (MonadDB m, CryptoRNG m) => UserID -> Email -> m EmailChangeRequest
newEmailChangeRequest uid new_email = do
  token <- random
  expires <- minutesAfter (24*60) `liftM` getMinutesTime
  -- only one email change request can be active at a time, so we want
  -- to remove old one before we insert new one. this could potentially
  -- lead to race condition (when we introduce possibility of one user
  -- being logged in on more than one machine), but the possibility that
  -- two users logged to the same account will invoke this function at
  -- the same time is basically 0.
  _ <- dbUpdate $ DeleteAction emailChangeRequest uid
  dbUpdate $ NewAction emailChangeRequest expires (uid, new_email, token)

newEmailChangeRequestLink :: (MonadDB m, CryptoRNG m) => UserID -> Email -> m KontraLink
newEmailChangeRequestLink uid new_email = do
  ecr <- newEmailChangeRequest uid new_email
  return $ LinkChangeUserEmail (ecrUserID ecr) (ecrToken ecr)
