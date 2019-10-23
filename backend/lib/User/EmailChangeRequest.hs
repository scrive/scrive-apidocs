module User.EmailChangeRequest (
    EmailChangeRequest(..)
  , getEmailChangeRequestNewEmail
  , newEmailChangeRequest
  , newEmailChangeRequestLink
  , DeleteEmailChangeRequest(..)
  , DeleteExpiredEmailChangeRequests(..)

  -- only for testing
  , GetExpiredEmailChangeRequestsForTesting(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Crypto.RNG
import Data.Typeable

import Context
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

getEmailChangeRequestNewEmail
  :: (MonadDB m, MonadThrow m, MonadTime m, KontraMonad m)
  => UserID
  -> MagicHash
  -> m (Maybe Email)
getEmailChangeRequestNewEmail uid token = runMaybeT $ do
  Just EmailChangeRequest {..} <- dbQuery $ GetEmailChangeRequest uid
  guard $ ecrToken == token
  Just loggeduser <- get ctxmaybeuser <$> lift getContext
  guard $ ecrUserID == userid loggeduser
  -- check if a user didn't create an account with this email
  -- between requesting email change and confirming the change
  Nothing <- dbQuery $ GetUserByEmail ecrNewEmail
  return ecrNewEmail

newEmailChangeRequest
  :: (MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m)
  => UserID
  -> Email
  -> m EmailChangeRequest
newEmailChangeRequest uid new_email = do
  token   <- random
  expires <- (1 `daysAfter`) <$> currentTime
  -- only one email change request can be active at a time, so we want
  -- to remove old one before we insert new one. this could potentially
  -- lead to race condition (when we introduce possibility of one user
  -- being logged in on more than one machine), but the possibility that
  -- two users logged to the same account will invoke this function at
  -- the same time is basically 0.
  void $ dbUpdate $ DeleteEmailChangeRequest uid
  dbUpdate . CreateEmailChangeRequest $ EmailChangeRequest uid expires new_email token

newEmailChangeRequestLink
  :: (MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m)
  => UserID
  -> Email
  -> m KontraLink
newEmailChangeRequestLink uid new_email = do
  ecr <- newEmailChangeRequest uid new_email
  return $ LinkChangeUserEmail (ecrUserID ecr) (ecrToken ecr)

selectEmailChangeRequestSelectorsList :: [SQL]
selectEmailChangeRequestSelectorsList = ["user_id", "expires", "new_email", "token"]

data DeleteExpiredEmailChangeRequests = DeleteExpiredEmailChangeRequests
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m DeleteExpiredEmailChangeRequests () where
  update DeleteExpiredEmailChangeRequests = do
    now <- currentTime
    runQuery_ . sqlDelete "email_change_requests" $ sqlWhere $ "expires <" <?> now

data GetExpiredEmailChangeRequestsForTesting = GetExpiredEmailChangeRequestsForTesting
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetExpiredEmailChangeRequestsForTesting [EmailChangeRequest] where
  query GetExpiredEmailChangeRequestsForTesting = do
    now <- currentTime
    runQuery_ $ sqlSelect "email_change_requests" $ do
      mapM_ sqlResult selectEmailChangeRequestSelectorsList
      sqlWhere $ "expires <" <?> now
    fetchMany fetchEmailChangeRequest

data GetEmailChangeRequest = GetEmailChangeRequest UserID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetEmailChangeRequest (Maybe EmailChangeRequest) where
  query (GetEmailChangeRequest user_id) = do
    now <- currentTime
    runQuery_ $ sqlSelect "email_change_requests" $ do
      mapM_ sqlResult selectEmailChangeRequestSelectorsList
      sqlWhereEq "user_id" user_id
      sqlWhere $ "expires >=" <?> now
    fetchMaybe fetchEmailChangeRequest

data CreateEmailChangeRequest = CreateEmailChangeRequest EmailChangeRequest
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateEmailChangeRequest EmailChangeRequest where
  update (CreateEmailChangeRequest EmailChangeRequest {..}) = do
    runQuery_ $ sqlInsert "email_change_requests" $ do
      sqlSet "user_id"   ecrUserID
      sqlSet "new_email" ecrNewEmail
      sqlSet "token"     ecrToken
      sqlSet "expires"   ecrExpires
      mapM_ sqlResult selectEmailChangeRequestSelectorsList
    fetchOne fetchEmailChangeRequest

data DeleteEmailChangeRequest = DeleteEmailChangeRequest UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteEmailChangeRequest Bool where
  update (DeleteEmailChangeRequest user_id) = do
    runQuery01 . sqlDelete "email_change_requests" $ do
      sqlWhereEq "user_id" user_id

fetchEmailChangeRequest :: (UserID, UTCTime, Email, MagicHash) -> EmailChangeRequest
fetchEmailChangeRequest (user_id, expires, new_email, token) = EmailChangeRequest
  { ecrUserID   = user_id
  , ecrExpires  = expires
  , ecrNewEmail = new_email
  , ecrToken    = token
  }
