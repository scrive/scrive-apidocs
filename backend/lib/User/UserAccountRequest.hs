module User.UserAccountRequest (
    UserAccountRequest(..)
  , getUserAccountRequestUser
  , newUserAccountRequest
  , newUserAccountRequestLink
  , expireUserAccountRequests
  , DeleteUserAccountRequest(..)

  -- only for testing
  , GetExpiredUserAccountRequestsForTesting(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Crypto.RNG
import Data.Typeable
import Log
import qualified Control.Exception.Lifted as E

import DB
import KontraLink
import Log.Identifier
import MagicHash
import MinutesTime
import User.Model

data UserAccountRequest = UserAccountRequest {
    uarUserID :: UserID
  , uarExpires :: UTCTime
  , uarToken :: MagicHash
  } deriving (Show, Typeable)

getUserAccountRequestUser :: (MonadDB m, MonadThrow m, MonadTime m) => UserID -> MagicHash -> m (Maybe User)
getUserAccountRequestUser uid token = runMaybeT $ do
  Just UserAccountRequest{..} <- dbQuery $ GetUserAccountRequest uid
  guard $ uarToken == token
  Just user <- dbQuery $ GetUserByID uarUserID
  return user

newUserAccountRequest :: (MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m) => UserID -> m UserAccountRequest
newUserAccountRequest uid = do
  token <- random
  let midnightInAMonth = (1 `secondsBefore`) . nextDayMidnight . (28 `daysAfter`)
  expires <- midnightInAMonth <$> currentTime
  -- FIXME: highly unlikely, but possible race condition
  ma <- dbQuery $ GetUserAccountRequest uid
  case ma of
    Just a -> do
      let a' = a { uarExpires = expires }
      _ <- dbUpdate $ UpdateUserAccountRequest a'
      return a'
    Nothing -> do
      _ <- dbUpdate $ DeleteUserAccountRequest uid
      dbUpdate . CreateUserAccountRequest $ UserAccountRequest uid expires token

newUserAccountRequestLink :: (MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m) => Lang -> UserID -> SignupMethod -> m KontraLink
newUserAccountRequestLink lang uid sm = do
  uar <- newUserAccountRequest uid
  return $ LinkAccountCreated lang (uarUserID uar) (uarToken uar) sm

selectUserAccountRequestSelectorsList :: [SQL]
selectUserAccountRequestSelectorsList = ["user_id", "expires", "token"]

expireUserAccountRequests
  :: (MonadDB m, MonadTime m, MonadThrow m, MonadCatch m, MonadLog m)
  => m ()
expireUserAccountRequests = do
  prs <- dbQuery GetExpiredUserAccountRequests
  forM_ prs $ \UserAccountRequest{uarUserID} -> do
    res <- try . localData [identifier_ uarUserID] $ do
      _ <- dbUpdate $ DeleteUserAccountRequest uarUserID
      musertos <- (fmap userhasacceptedtermsofservice) <$> dbQuery (GetUserByIDIncludeDeleted uarUserID)
      case musertos of
        Just Nothing -> do
          success <- dbUpdate $ RemoveInactiveUser uarUserID
          when success $
            logInfo_ "Inactive user (no plan) successfully removed from database"
        _ -> return ()
    case res of
      Right () -> commit
      Left (e::E.SomeException) -> do
        logAttention "UserAccountRequest expiration failed" $ object [
            "exception" .= show e
          ]
        rollback

data GetExpiredUserAccountRequests = GetExpiredUserAccountRequests
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetExpiredUserAccountRequests [UserAccountRequest] where
  query GetExpiredUserAccountRequests = do
    now <- currentTime
    runQuery_ $ sqlSelect "user_account_requests" $ do
      mapM_ sqlResult selectUserAccountRequestSelectorsList
      sqlWhere $ "expires < " <?> now
    fetchMany fetchUserAccountRequest

data GetExpiredUserAccountRequestsForTesting = GetExpiredUserAccountRequestsForTesting UTCTime
instance (MonadDB m, MonadThrow m) => DBQuery m GetExpiredUserAccountRequestsForTesting [UserAccountRequest] where
  query (GetExpiredUserAccountRequestsForTesting now) = do
    runQuery_ $ sqlSelect "user_account_requests" $ do
      mapM_ sqlResult selectUserAccountRequestSelectorsList
      sqlWhere $ "expires < " <?> now
    fetchMany fetchUserAccountRequest

data GetUserAccountRequest = GetUserAccountRequest UserID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetUserAccountRequest (Maybe UserAccountRequest) where
  query (GetUserAccountRequest user_id) = do
    now <- currentTime
    runQuery_ $ sqlSelect "user_account_requests" $ do
      mapM_ sqlResult selectUserAccountRequestSelectorsList
      sqlWhereEq "user_id" user_id
      sqlWhere $ "expires >= " <?> now
    fetchMaybe fetchUserAccountRequest

data CreateUserAccountRequest = CreateUserAccountRequest UserAccountRequest
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateUserAccountRequest UserAccountRequest where
  update (CreateUserAccountRequest UserAccountRequest{..}) = do
    runQuery_ $ sqlInsert "user_account_requests" $ do
      sqlSet "user_id" uarUserID
      sqlSet "expires" uarExpires
      sqlSet "token" uarToken
      mapM_ sqlResult selectUserAccountRequestSelectorsList
    fetchOne fetchUserAccountRequest

data UpdateUserAccountRequest = UpdateUserAccountRequest UserAccountRequest
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateUserAccountRequest Bool where
  update (UpdateUserAccountRequest UserAccountRequest{..}) = do
    runQuery01 $ sqlUpdate "user_account_requests" $ do
      sqlSet "expires" uarExpires
      sqlSet "token" uarToken
      sqlWhereEq "user_id" uarUserID

data DeleteUserAccountRequest = DeleteUserAccountRequest UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteUserAccountRequest Bool where
  update (DeleteUserAccountRequest user_id) = do
    runQuery01 . sqlDelete  "user_account_requests" $ do
      sqlWhereEq "user_id" user_id

fetchUserAccountRequest :: (UserID, UTCTime, MagicHash) -> UserAccountRequest
fetchUserAccountRequest (user_id, expires, token) =
  UserAccountRequest
    { uarUserID = user_id
    , uarExpires = expires
    , uarToken = token
    }
