module UserGroup.DeletionRequest.Model where

import Control.Monad.Catch
import Data.Time (Day, UTCTime(..))
import Log

import DB
import User.UserID (UserID)
import UserGroup.DeletionRequest.Types
import UserGroup.Types (UserGroupID)

selectUserGroupDeletionRequest :: [SQL]
selectUserGroupDeletionRequest =
  [ "for_user_group_id"
  , "requested_by_user_id"
  , "requested_deletion_date"
  , "signed_off_by_user_id"
  , "expires"
  ]

fetchUserGroupDeletionRequest
  :: (UserGroupID, UserID, Day, Maybe UserID, Maybe UTCTime) -> UserGroupDeletionRequest
fetchUserGroupDeletionRequest (requestedFor, requestedBy, requestedDeletionDate, signedOffBy, expires)
  = UserGroupDeletionRequest { .. }

data UserGroupSignOffDeletion = UserGroupSignOffDeletion UserGroupID UserID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m UserGroupSignOffDeletion (Maybe UserGroupDeletionRequest) where
  dbUpdate (UserGroupSignOffDeletion requestedFor signedOffBy) = do
    now <- currentTime
    runQuery_ . sqlUpdate "user_group_deletion_requests" $ do
      sqlWhereEq "for_user_group_id" requestedFor
      sqlWhere $ "expires >= " <?> now
      sqlWhereIsNULL "signed_off_by_user_id"
      sqlWhereNotEq "requested_by_user_id" signedOffBy
      sqlSet "signed_off_by_user_id" signedOffBy
      sqlSetCmd "expires" "null"
      mapM_ sqlResult selectUserGroupDeletionRequest
    fetchMaybe fetchUserGroupDeletionRequest

newtype UserGroupCreateDeletionRequest = UserGroupCreateDeletionRequest UserGroupDeletionRequest
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m UserGroupCreateDeletionRequest (Maybe UserGroupDeletionRequest) where
  dbUpdate (UserGroupCreateDeletionRequest UserGroupDeletionRequest {..}) = do
    now <- currentTime
    -- delete expired deletion requests that would cause the insert to fail
    runQuery_ . sqlDelete "user_group_deletion_requests" $ do
      sqlWhere $ "expires <" <?> now
      sqlWhereEq "for_user_group_id" requestedFor

    runQuery_ . sqlInsertSelect "user_group_deletion_requests" "" $ do
      sqlSet "for_user_group_id"       requestedFor
      sqlSet "requested_by_user_id"    requestedBy
      sqlSet "requested_deletion_date" requestedDeletionDate
      sqlSet "expires"                 expires
      sqlWhereNotExists . sqlSelect "user_groups" $ do
        sqlWhereEq "parent_group_id" requestedFor
      mapM_ sqlResult selectUserGroupDeletionRequest
    fetchMaybe fetchUserGroupDeletionRequest

data DeleteExpiredUserGroupDeletionRequest = DeleteExpiredUserGroupDeletionRequest
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m DeleteExpiredUserGroupDeletionRequest () where
  dbUpdate DeleteExpiredUserGroupDeletionRequest = do
    now <- currentTime
    runQuery_ . sqlDelete "user_group_deletion_requests" . sqlWhere $ "expires <" <?> now

newtype GetUserGroupDeletionRequest = GetUserGroupDeletionRequest UserGroupID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetUserGroupDeletionRequest (Maybe UserGroupDeletionRequest) where
  dbQuery (GetUserGroupDeletionRequest requestedFor) = do
    now <- currentTime
    runQuery_ . sqlSelect "user_group_deletion_requests" $ do
      mapM_ sqlResult selectUserGroupDeletionRequest
      sqlWhereEq "for_user_group_id" requestedFor
      sqlWhere $ "expires >= " <?> now <+> "OR expires IS NULL"
    fetchMaybe fetchUserGroupDeletionRequest

newtype DeleteUserGroupDeletionRequest = DeleteUserGroupDeletionRequest UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteUserGroupDeletionRequest () where
  dbUpdate (DeleteUserGroupDeletionRequest requestedFor) = do
    runQuery_ . sqlDelete "user_group_deletion_requests" $ do
      sqlWhereEq "for_user_group_id" requestedFor

data GetSignedOffAndReadyDeletionRequests = GetSignedOffAndReadyDeletionRequests
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetSignedOffAndReadyDeletionRequests [UserGroupDeletionRequest] where
  dbQuery GetSignedOffAndReadyDeletionRequests = do
    UTCTime today _ <- currentTime
    runQuery_ . sqlSelect "user_group_deletion_requests" $ do
      sqlWhereIsNotNULL "signed_off_by_user_id"
      sqlWhere $ "requested_deletion_date <= " <?> today
      sqlWhereNotExists . sqlSelect "user_groups" $ do
        sqlWhere "parent_group_id = for_user_group_id"
      mapM_ sqlResult selectUserGroupDeletionRequest
    fetchMany fetchUserGroupDeletionRequest

newtype LogUserGroupDeletion = LogUserGroupDeletion UserGroupDeletionRequest
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m LogUserGroupDeletion () where
  dbUpdate (LogUserGroupDeletion UserGroupDeletionRequest {..}) = do
    now <- currentTime
    runQuery_ . sqlInsert "user_group_deletion_log" $ do
      sqlSet "deleted_user_group_id"   requestedFor
      sqlSet "requested_by_user_id"    requestedBy
      sqlSet "signed_off_by_user_id"   signedOffBy
      sqlSet "requested_deletion_date" requestedDeletionDate
      sqlSet "deletion_time"           now
