module AccessControl.Model
  ( AccessControlCreateForUser(..)
  , AccessControlCreateForUserGroup(..)
  , AccessRoleGet(..)
  , GetRoles(..)
  , AccessControlGetRolesByUser(..)
  , AccessControlGetRolesByUserGroup(..)
  , AccessControlDeleteRolesByUserGroup(..)
  , AccessControlInsertUserGroupAdmin(..)
  , AccessControlRemoveRole(..)
  , AccessControlRemoveUserGroupAdminRole(..)
  , addInheritedRoles
  ) where

import Control.Monad.Catch
import Control.Monad.Extra (concatForM)
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
  UserAR uid             -> sqlSet "trg_user_id" uid
  UserGroupMemberAR ugid -> sqlSet "trg_user_group_id" ugid
  UserAdminAR ugid       -> sqlSet "trg_user_group_id" ugid
  UserGroupAdminAR ugid  -> sqlSet "trg_user_group_id" ugid
  DocumentAdminAR fid    -> sqlSet "trg_folder_id" fid

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
    let ugid = usergroupid u
        uid = userid u
        isAdmin = useriscompanyadmin u
    dbRolesByUser <- do
      query . AccessControlGetRolesByUser $ uid
    dbRolesByUserGroup <- do
      query . AccessControlGetRolesByUserGroup $ ugid
    -- Every user shall have DocumentAdminAR to his home folder
    -- Every is_company_admin shall have DocumentAdminAR to the company home
    -- folder
    mGroupHomeFolderID <- do
      (get folderID <$>) <$> (query . FolderGetUserGroupHome $ ugid)
    mUserHomeFolderID <- do
      (get folderID <$>) <$> (query . FolderGetUserHome $ uid)
    -- get company root folder
    let adminOrUserRoles =
          (if isAdmin then [UserAdminAR ugid] else [UserGroupMemberAR ugid]) <>
          maybe []
                (\hfid -> if isAdmin then [DocumentAdminAR hfid] else [])
                mGroupHomeFolderID <>
          maybe []
                (\hfid -> [DocumentAdminAR hfid])
                mUserHomeFolderID
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

data AccessControlDeleteRolesByUserGroup
  = AccessControlDeleteRolesByUserGroup UserGroupID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m AccessControlDeleteRolesByUserGroup () where
  update (AccessControlDeleteRolesByUserGroup ugid) = do
    runQuery_ . sqlDelete "access_control" $
      sqlWhereAny [
        sqlWhereEq "src_user_group_id" $ Just ugid
      , sqlWhereEq "trg_user_group_id" $ Just ugid
      ]

-- @devnote maybe significant enough an event so we should always log it?
data AccessControlInsertUserGroupAdmin
  = AccessControlInsertUserGroupAdmin UserID UserGroupID
instance (MonadDB m, MonadThrow m)
  => DBUpdate m AccessControlInsertUserGroupAdmin Bool where
  update (AccessControlInsertUserGroupAdmin uid ugid) =
    fmap isJust . dbUpdate .
      AccessControlCreateForUser uid $ UserGroupAdminAR ugid

data AccessControlRemoveRole = AccessControlRemoveRole AccessRoleID
instance (MonadDB m, MonadThrow m)
  => DBUpdate m AccessControlRemoveRole Bool where
  update (AccessControlRemoveRole roleId) =
    runQuery01 . sqlDelete "access_control" $ sqlWhereEq "id" roleId

data AccessControlRemoveUserGroupAdminRole =
    AccessControlRemoveUserGroupAdminRole UserID UserGroupID
instance (MonadDB m, MonadThrow m) =>
    DBUpdate m AccessControlRemoveUserGroupAdminRole Bool where
  update (AccessControlRemoveUserGroupAdminRole uid ugid) = do
    runQuery01 . sqlDelete "access_control" $ do
      sqlWhereEq "role" . toAccessRoleType $ UserGroupAdminAR ugid
      sqlWhereEq "src_user_id" uid
      sqlWhereEq "trg_user_group_id" ugid

addInheritedRoles :: (MonadDB m, MonadThrow m) => [AccessRole] -> m [AccessRole]
addInheritedRoles roles = concatForM roles $ \role ->
  case accessRoleTarget role of
    UserAdminAR ugid -> do
      ugwcs <- dbQuery $ UserGroupGetAllChildrenRecursive ugid
      return . (role:) . for (ugwcToList ugwcs) $ \ug ->
        accessRoleSetTarget (UserAdminAR $ get ugID ug) role
    _ -> return [role]

fetchAccessRole :: ( AccessRoleID
                   , AccessRoleType
                   , Maybe UserID
                   , Maybe UserGroupID
                   , Maybe UserID
                   , Maybe UserGroupID
                   , Maybe FolderID
                   ) -> AccessRole
fetchAccessRole (rid, rtype, Just uid, Nothing, trg_uid, trg_ugid, trg_foler)
  = AccessRoleUser rid uid $ fetchAccessRoleTarget
    ( rtype
    , trg_uid
    , trg_ugid
    , trg_foler
    )
fetchAccessRole (rid, rtype, Nothing, Just ugid, trg_uid, trg_ugid, trg_foler)
  = AccessRoleUserGroup rid ugid $ fetchAccessRoleTarget
    ( rtype
    , trg_uid
    , trg_ugid
    , trg_foler
    )
fetchAccessRole _ = unexpectedError "invalid access_control row in database"

fetchAccessRoleTarget :: ( AccessRoleType
                         , Maybe UserID
                         , Maybe UserGroupID
                         , Maybe FolderID
                         ) -> AccessRoleTarget
fetchAccessRoleTarget (UserART           , Just usrID, Nothing      , Nothing)
  = UserAR usrID
fetchAccessRoleTarget (UserGroupMemberART, Nothing   , Just usrGrpID, Nothing)
  = UserGroupMemberAR usrGrpID
fetchAccessRoleTarget (UserAdminART      , Nothing   , Just usrGrpID, Nothing)
  = UserAdminAR usrGrpID
fetchAccessRoleTarget (UserGroupAdminART , Nothing   , Just usrGrpID, Nothing)
  = UserGroupAdminAR usrGrpID
fetchAccessRoleTarget (DocumentAdminART  , Nothing   , Nothing      , Just fid)
  = DocumentAdminAR  fid
fetchAccessRoleTarget _
  = unexpectedError "invalid access_control row in database"
