module ActionQueue.EmailChangeRequest (
    EmailChangeRequest(..)
  , emailChangeRequest
  , getEmailChangeRequestNewEmail
  , newEmailChangeRequest
  , newEmailChangeRequestLink
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Typeable

import ActionQueue.Core
import ActionQueue.Scheduler
import ActionQueue.Tables
import Context
import Crypto.RNG
import DB
import KontraLink
import KontraMonad
import MagicHash
import MinutesTime
import User.Email
import User.Model

data EmailChangeRequest = EmailChangeRequest {
    ecrUserID :: UserID
  , ecrExpires :: UTCTime
  , ecrNewEmail :: Email
  , ecrToken :: MagicHash
  } deriving (Show, Typeable)

emailChangeRequest :: Action UserID EmailChangeRequest (UserID, Email, MagicHash) Scheduler
emailChangeRequest = Action {
    qaTable = tableEmailChangeRequests
  , qaSetFields = \(uid, new_email, token) -> do
      sqlSet "user_id" uid
      sqlSet "new_email" new_email
      sqlSet "token" token
  , qaSelectFields = ["user_id", "expires", "new_email", "token"]
  , qaIndexField = "user_id"
  , qaExpirationDelay = "1 hour"
  , qaDecode = \(user_id, expires, new_email, token) -> EmailChangeRequest {
      ecrUserID = user_id
    , ecrExpires = expires
    , ecrNewEmail = new_email
    , ecrToken = token
    }
  , qaUpdateSQL = \EmailChangeRequest{..} -> toSQLCommand $ sqlUpdate "email_change_requests" $ do
      sqlSet "expires" ecrExpires
      sqlSet "new_email" ecrNewEmail
      sqlSet "token" ecrToken
      sqlWhereEq (qaIndexField emailChangeRequest) ecrUserID
  , qaEvaluateExpired = \EmailChangeRequest{ecrUserID} -> do
    _ <- dbUpdate $ DeleteAction emailChangeRequest ecrUserID
    return ()
  }

getEmailChangeRequestNewEmail :: (MonadDB m, MonadThrow m, KontraMonad m) => UserID -> MagicHash -> m (Maybe Email)
getEmailChangeRequestNewEmail uid token = runMaybeT $ do
  Just EmailChangeRequest{..} <- dbQuery $ GetAction emailChangeRequest uid
  guard $ ecrToken == token
  Context{ctxmaybeuser = Just loggeduser} <- lift getContext
  guard $ ecrUserID == userid loggeduser
  -- check if a user didn't create an account with this email
  -- between requesting email change and confirming the change
  Nothing <- dbQuery $ GetUserByEmail ecrNewEmail
  return ecrNewEmail

newEmailChangeRequest :: (MonadDB m, MonadThrow m, CryptoRNG m) => UserID -> Email -> m EmailChangeRequest
newEmailChangeRequest uid new_email = do
  token <- random
  expires <- (1 `daysAfter`) `liftM` currentTime
  -- only one email change request can be active at a time, so we want
  -- to remove old one before we insert new one. this could potentially
  -- lead to race condition (when we introduce possibility of one user
  -- being logged in on more than one machine), but the possibility that
  -- two users logged to the same account will invoke this function at
  -- the same time is basically 0.
  _ <- dbUpdate $ DeleteAction emailChangeRequest uid
  dbUpdate $ NewAction emailChangeRequest expires (uid, new_email, token)

newEmailChangeRequestLink :: (MonadDB m, MonadThrow m, CryptoRNG m) => UserID -> Email -> m KontraLink
newEmailChangeRequestLink uid new_email = do
  ecr <- newEmailChangeRequest uid new_email
  return $ LinkChangeUserEmail (ecrUserID ecr) (ecrToken ecr)
