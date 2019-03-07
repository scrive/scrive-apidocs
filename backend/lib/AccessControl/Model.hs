module AccessControl.Model
  ( AccessControlGetRolesByUser(..)
  , AccessControlGetRolesByUserGroup(..)
  , AccessControlInsertUserGroupAdmin(..)
  , AccessControlRemoveUserGroupAdminRole(..)
  ) where

import Control.Monad.Catch

import AccessControl.Types
import DB
import Folder.Types
import User.UserID
import UserGroup.Model
import UserGroup.Types

rolesSelector :: [SQL]
rolesSelector =
  [ "role"
  , "trg_user_id"
  , "trg_user_group_id"
  , "trg_folder_id"
  ]

data AccessControlGetRolesByUser = AccessControlGetRolesByUser UserID
instance (MonadDB m, MonadThrow m) => DBQuery m AccessControlGetRolesByUser [AccessRole] where
  query (AccessControlGetRolesByUser uid) = do
    runQuery_ . sqlSelect "access_control" $ do
      mapM_ sqlResult $ rolesSelector
      sqlWhereEq "src_user_id" uid
    fetchMany fetchAccessRole

data AccessControlGetRolesByUserGroup =
    AccessControlGetRolesByUserGroup UserGroupID
instance (MonadDB m, MonadThrow m) =>
  DBQuery m AccessControlGetRolesByUserGroup [AccessRole] where
  query (AccessControlGetRolesByUserGroup ugid) = do
    mugwp <- query . UserGroupGetWithParents $ ugid
    let ugids =
          case mugwp of
            Nothing -> []
            Just ugwp -> maybe [] (\(_, ugids') -> get ugID <$> ugids')
                                  (ugwpOnlyParents ugwp)
    runQuery_ . sqlSelect "access_control" $ do
      mapM_ sqlResult $ rolesSelector
      sqlWhereIn "src_user_group_id" ugids
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

fetchAccessRole :: ( AccessRoleType
                   , Maybe UserID
                   , Maybe UserGroupID
                   , Maybe FolderID
                   ) -> AccessRole
fetchAccessRole (UserART           , Just usrID, Nothing      , Nothing)
  = UserAR usrID
fetchAccessRole (UserGroupMemberART, Nothing   , Just usrGrpID, Nothing)
  = UserGroupMemberAR usrGrpID
fetchAccessRole (UserAdminART      , Nothing   , Just usrGrpID, Nothing)
  = UserAdminAR usrGrpID
fetchAccessRole (UserGroupAdminART , Nothing   , Just usrGrpID, Nothing)
  = UserGroupAdminAR usrGrpID
fetchAccessRole (DocumentAdminART  , Nothing   , Nothing      , Just fid)
  = DocumentAdminAR  fid
fetchAccessRole _ = unexpectedError "invalid access_control row in database"
