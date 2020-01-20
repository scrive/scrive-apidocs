module AccessControl.Model
  ( AccessControlCreateForUser(..)
  , AccessControlCreateForUserGroup(..)
  , AccessRoleGet(..)
  , GetRoles(..)
  , AccessControlGetRolesByUser(..)
  , AccessControlGetRolesByUserGroup(..)
  , AccessControlDeleteRolesByFolder(..)
  , AccessControlDeleteRolesByUserGroup(..)
  , AccessControlRemoveRole(..)
  , AccessControlInsertRoleForUser(..)
  , AccessControlRemoveUserGroupAdminRole(..)
  , addInheritedRoles
  ) where

import Control.Monad.Catch
import Control.Monad.Extra (concatForM)
import Control.Monad.State (State)
import Control.Monad.State.Class (MonadState)

import AccessControl.Types
import DB
import Folder.Model
import User.Types.User
import User.UserID
import UserGroup.Model
import UserGroup.Types

rolesSelector :: [SQL]
rolesSelector =
  [ "id"
  , "role"
  , "src_user_id"
  , "src_user_group_id"
  , "trg_user_id"
  , "trg_user_group_id"
  , "trg_folder_id"
  ]

data AccessControlCreateForUser
  = AccessControlCreateForUser UserID AccessRoleTarget
instance (MonadDB m, MonadThrow m)
  => DBUpdate m AccessControlCreateForUser (Maybe AccessRole) where
  update (AccessControlCreateForUser uid target) = do
    runQuery_ . sqlInsert "access_control" $ do
      sqlSet "role" $ toAccessRoleType target
      sqlSet "src_user_id" uid
      setTarget target
      sqlResult "id"
    fetchOne runIdentity >>= dbQuery . AccessRoleGet

data AccessControlCreateForUserGroup
  = AccessControlCreateForUserGroup UserGroupID AccessRoleTarget
instance (MonadDB m, MonadThrow m)
  => DBUpdate m AccessControlCreateForUserGroup (Maybe AccessRole) where
  update (AccessControlCreateForUserGroup ugid target) = do
    runQuery_ . sqlInsert "access_control" $ do
      sqlSet "role" $ toAccessRoleType target
      sqlSet "src_user_group_id" ugid
      setTarget target
      sqlResult "id"
    fetchOne runIdentity >>= dbQuery . AccessRoleGet

setTarget :: (MonadState v m, SqlSet v) => AccessRoleTarget -> m ()
setTarget target = case target of
  UserAR            uid  -> sqlSet "trg_user_id" uid
  UserGroupMemberAR ugid -> sqlSet "trg_user_group_id" ugid
  UserAdminAR       ugid -> sqlSet "trg_user_group_id" ugid
  UserGroupAdminAR  ugid -> sqlSet "trg_user_group_id" ugid
  FolderAdminAR     fid  -> sqlSet "trg_folder_id" fid
  FolderUserAR      fid  -> sqlSet "trg_folder_id" fid

data AccessRoleGet = AccessRoleGet AccessRoleID
instance (MonadDB m, MonadThrow m)
  => DBQuery m AccessRoleGet (Maybe AccessRole) where
  query (AccessRoleGet roleId) = do
    runQuery_ . sqlSelect "access_control" $ do
      mapM_ sqlResult $ rolesSelector
      sqlWhereEq "id" roleId
    fetchMaybe fetchAccessRole

data GetRoles = GetRoles User
instance (MonadDB m, MonadThrow m) => DBQuery m GetRoles [AccessRole] where
  query (GetRoles u) = do
    let ugid    = u ^. #groupID
        uid     = u ^. #id
        isAdmin = u ^. #isCompanyAdmin
    dbRolesByUser <- do
      query . AccessControlGetRolesByUser $ uid
    dbRolesByUserGroup <- do
      query . AccessControlGetRolesByUserGroup $ ugid
    -- Every user shall have DocumentAdminAR to his home folder
    -- Every is_company_admin shall have DocumentAdminAR to the company home
    -- folder
    mGroupHomeFolderID <- do
      (view #id <$>) <$> (query . FolderGetUserGroupHome $ ugid)
    mUserHomeFolderID <- do
      (view #id <$>) <$> (query . FolderGetUserHome $ uid)
    -- get company root folder
    let adminOrUserRoles =
          (if isAdmin then [UserAdminAR ugid] else [UserGroupMemberAR ugid])
            <> maybe []
                     (\hfid -> if isAdmin then [FolderAdminAR hfid] else [])
                     mGroupHomeFolderID
            <> maybe [] (\hfid -> [FolderUserAR hfid]) mUserHomeFolderID
        derivedRoles = AccessRoleImplicitUser uid <$> adminOrUserRoles <> [UserAR uid]
    return $ dbRolesByUser <> dbRolesByUserGroup <> derivedRoles

data AccessControlGetRolesByUser = AccessControlGetRolesByUser UserID
instance (MonadDB m, MonadThrow m)
  => DBQuery m AccessControlGetRolesByUser [AccessRole] where
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
    runQuery_ . sqlSelect "access_control" $ do
      mapM_ sqlResult $ rolesSelector
      sqlWhereEq "src_user_group_id" ugid
    fetchMany fetchAccessRole

data AccessControlDeleteRolesByUserGroup
  = AccessControlDeleteRolesByUserGroup UserGroupID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m AccessControlDeleteRolesByUserGroup () where
  update (AccessControlDeleteRolesByUserGroup ugid) = do
    runQuery_ . sqlDelete "access_control" $ sqlWhereAny
      [ sqlWhereEq "src_user_group_id" $ Just ugid
      , sqlWhereEq "trg_user_group_id" $ Just ugid
      ]

data AccessControlDeleteRolesByFolder
  = AccessControlDeleteRolesByFolder FolderID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m AccessControlDeleteRolesByFolder () where
  update (AccessControlDeleteRolesByFolder fdrid) = do
    runQuery_ . sqlDelete "access_control" $ sqlWhereEq "trg_folder_id" $ Just fdrid

data AccessControlRemoveRole = AccessControlRemoveRole AccessRoleID
instance (MonadDB m, MonadThrow m)
  => DBUpdate m AccessControlRemoveRole Bool where
  update (AccessControlRemoveRole roleId) =
    runQuery01 . sqlDelete "access_control" $ sqlWhereEq "id" roleId

data AccessControlInsertRoleForUser =
    AccessControlInsertRoleForUser UserID AccessRoleTarget
instance (MonadDB m, MonadThrow m) => DBUpdate m AccessControlInsertRoleForUser Bool where
  update (AccessControlInsertRoleForUser uid trg) = do
    runQuery01 . sqlInsert "access_control" $ do
      sqlSet "role"        (toAccessRoleType trg)
      sqlSet "src_user_id" uid
      setAccessRoleTarget trg
    where
      setAccessRoleTarget :: AccessRoleTarget -> State SqlInsert ()
      setAccessRoleTarget trg' = case trg' of
        UserAR            k -> sqlSet "trg_user_id" (unUserID k)
        UserGroupMemberAR k -> sqlSet "trg_user_group_id" (fromUserGroupID k)
        UserAdminAR       k -> sqlSet "trg_user_group_id" (fromUserGroupID k)
        UserGroupAdminAR  k -> sqlSet "trg_user_group_id" (fromUserGroupID k)
        FolderAdminAR     k -> sqlSet "trg_folder_id" (fromFolderID k)
        FolderUserAR      k -> sqlSet "trg_folder_id" (fromFolderID k)

data AccessControlRemoveUserGroupAdminRole =
    AccessControlRemoveUserGroupAdminRole UserID UserGroupID
instance (MonadDB m, MonadThrow m) =>
    DBUpdate m AccessControlRemoveUserGroupAdminRole Bool where
  update (AccessControlRemoveUserGroupAdminRole uid ugid) = do
    runQuery01 . sqlDelete "access_control" $ do
      sqlWhereEq "role" . toAccessRoleType $ UserGroupAdminAR ugid
      sqlWhereEq "src_user_id"       uid
      sqlWhereEq "trg_user_group_id" ugid

addInheritedRoles :: (MonadDB m, MonadThrow m) => [AccessRole] -> m [AccessRole]
addInheritedRoles roles = concatForM roles $ \role -> case accessRoleTarget role of
  UserAdminAR ugid -> do
    ugwcs <- dbQuery $ UserGroupGetAllChildrenRecursive ugid
    return . (role :) . for (ugwcToList ugwcs) $ \ug ->
      accessRoleSetTarget (UserAdminAR $ ug ^. #id) role
  _ -> return [role]

fetchAccessRole
  :: ( AccessRoleID
     , AccessRoleType
     , Maybe UserID
     , Maybe UserGroupID
     , Maybe UserID
     , Maybe UserGroupID
     , Maybe FolderID
     )
  -> AccessRole
fetchAccessRole (rid, rtype, Just uid, Nothing, trg_uid, trg_ugid, trg_foler) =
  AccessRoleUser rid uid $ fetchAccessRoleTarget (rtype, trg_uid, trg_ugid, trg_foler)
fetchAccessRole (rid, rtype, Nothing, Just ugid, trg_uid, trg_ugid, trg_foler) =
  AccessRoleUserGroup rid ugid
    $ fetchAccessRoleTarget (rtype, trg_uid, trg_ugid, trg_foler)
fetchAccessRole _ = unexpectedError "invalid access_control row in database"

fetchAccessRoleTarget
  :: (AccessRoleType, Maybe UserID, Maybe UserGroupID, Maybe FolderID) -> AccessRoleTarget
fetchAccessRoleTarget (UserART, Just usrID, Nothing, Nothing) = UserAR usrID
fetchAccessRoleTarget (UserGroupMemberART, Nothing, Just usrGrpID, Nothing) =
  UserGroupMemberAR usrGrpID
fetchAccessRoleTarget (UserAdminART, Nothing, Just usrGrpID, Nothing) =
  UserAdminAR usrGrpID
fetchAccessRoleTarget (UserGroupAdminART, Nothing, Just usrGrpID, Nothing) =
  UserGroupAdminAR usrGrpID
fetchAccessRoleTarget (FolderAdminART, Nothing, Nothing, Just fid) = FolderAdminAR fid
fetchAccessRoleTarget (FolderUserART, Nothing, Nothing, Just fid) = FolderUserAR fid
fetchAccessRoleTarget _ = unexpectedError "invalid access_control row in database"
