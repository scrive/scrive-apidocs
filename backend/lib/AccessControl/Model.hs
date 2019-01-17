module AccessControl.Model
  ( AccessControlGetRolesByUser(..)
  , AccessControlInsertUserGroupAdmin(..)
  , AccessControlRemoveUserGroupAdminRole(..)
  ) where

import Control.Monad.Catch

import AccessControl.Types
import DB
import User.UserID
import UserGroup.Types

rolesSelector :: [SQL]
rolesSelector =
  [ "role"
  , "trg_user_id"
  , "trg_user_group_id"
  ]

data AccessControlGetRolesByUser = AccessControlGetRolesByUser UserID
instance (MonadDB m, MonadThrow m) => DBQuery m AccessControlGetRolesByUser [AccessRole] where
  query (AccessControlGetRolesByUser uid) = do
    runQuery_ . sqlSelect "access_control" $ do
      mapM_ sqlResult $ rolesSelector
      sqlWhereEq "src_user_id" uid
    fetchMany fetchAccessRole

-- @devnote maybe significant enough an event so we should always log it?
data AccessControlInsertUserGroupAdmin = AccessControlInsertUserGroupAdmin UserID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m AccessControlInsertUserGroupAdmin Bool where
  update (AccessControlInsertUserGroupAdmin uid ugid) = do
    runQuery01 . sqlInsert "access_control" $ do
      sqlSet "role" (toAccessRoleType $ UserGroupAdminAR ugid)
      sqlSet "src_user_id" uid
      sqlSet "trg_user_group_id" ugid

data AccessControlRemoveUserGroupAdminRole =
    AccessControlRemoveUserGroupAdminRole UserID UserGroupID
instance (MonadDB m, MonadThrow m) =>
    DBUpdate m AccessControlRemoveUserGroupAdminRole Bool where
  update (AccessControlRemoveUserGroupAdminRole uid ugid) = do
    runQuery01 . sqlDelete "access_control" $ do
      sqlWhereEq "role" (toAccessRoleType $ UserGroupAdminAR ugid)
      sqlWhereEq "src_user_id" uid
      sqlWhereEq "trg_user_group_id" ugid

fetchAccessRole :: (AccessRoleType, Maybe UserID, Maybe UserGroupID) -> AccessRole
fetchAccessRole (UserART           , Just usrID, Nothing      ) = UserAR usrID
fetchAccessRole (UserGroupMemberART, Nothing   , Just usrGrpID) = UserGroupMemberAR usrGrpID
fetchAccessRole (UserAdminART      , Nothing   , Just usrGrpID) = UserAdminAR usrGrpID
fetchAccessRole (UserGroupAdminART , Nothing   , Just usrGrpID) = UserGroupAdminAR usrGrpID
fetchAccessRole _ = unexpectedError "invalid access_control row in database"
