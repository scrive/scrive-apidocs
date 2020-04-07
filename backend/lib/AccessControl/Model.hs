module AccessControl.Model
  ( AccessControlCreateForUser(..)
  , AccessControlCreateForUserGroup(..)
  , AccessRoleGet(..)
  , GetRoles(..)
  , GetRolesIncludingInherited(..)
  , AccessControlDeleteRolesByFolder(..)
  , AccessControlGetExplicitRoles(..)
  , AccessControlDeleteRolesByUserGroup(..)
  , AccessControlRemoveRole(..)
  , AccessControlInsertRoleForUser(..)
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
  dbUpdate (AccessControlCreateForUser uid target) = do
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
  dbUpdate (AccessControlCreateForUserGroup ugid target) = do
    runQuery_ . sqlInsert "access_control" $ do
      sqlSet "role" $ toAccessRoleType target
      sqlSet "src_user_group_id" ugid
      setTarget target
      sqlResult "id"
    fetchOne runIdentity >>= dbQuery . AccessRoleGet

setTarget :: (MonadState v m, SqlSet v) => AccessRoleTarget -> m ()
setTarget target = case target of
  UserAR               uid  -> sqlSet "trg_user_id" uid
  UserGroupMemberAR    ugid -> sqlSet "trg_user_group_id" ugid
  UserAdminAR          ugid -> sqlSet "trg_user_group_id" ugid
  UserGroupAdminAR     ugid -> sqlSet "trg_user_group_id" ugid
  FolderAdminAR        fid  -> sqlSet "trg_folder_id" fid
  FolderUserAR         fid  -> sqlSet "trg_folder_id" fid
  SharedTemplateUserAR fid  -> sqlSet "trg_folder_id" fid
  EidImpersonatorAR    ugid -> sqlSet "trg_user_group_id" ugid

newtype AccessRoleGet = AccessRoleGet AccessRoleID
instance (MonadDB m, MonadThrow m)
  => DBQuery m AccessRoleGet (Maybe AccessRole) where
  dbQuery (AccessRoleGet roleId) = do
    runQuery_ . sqlSelect "access_control" $ do
      mapM_ sqlResult rolesSelector
      sqlWhereEq "id" roleId
    fetchMaybe fetchAccessRole

newtype GetRoles = GetRoles User
instance (MonadDB m, MonadThrow m) => DBQuery m GetRoles [AccessRole] where
  dbQuery (GetRoles u) = do
    let ugid = u ^. #groupID
        uid  = u ^. #id
    dbRoles <- dbQuery $ AccessControlGetExplicitRoles uid ugid
    ug      <- dbQuery $ UserGroupGetByUserID uid
    return $ dbRoles <> derivedRoles u ug

-- Every is_company_admin shall have DocumentAfterPreparationAdminAR to the company home
-- folder
-- Every user shall have SharedTemplateUserAR to the company home folder
derivedRoles :: User -> UserGroup -> [AccessRole]
derivedRoles user ug =
  let isAdmin            = user ^. #isCompanyAdmin
      ugid               = user ^. #groupID
      uid                = user ^. #id
      mGroupHomeFolderID = ug ^. #homeFolderID
      adminOrUserRoles =
          (if isAdmin then [UserAdminAR ugid] else [UserGroupMemberAR ugid])
            <> maybe []
                     (\hfid -> if isAdmin then [FolderAdminAR hfid] else [])
                     mGroupHomeFolderID
      userRoles =
          maybe [] (\hfid -> [SharedTemplateUserAR hfid]) mGroupHomeFolderID
            <> maybe [] (\hfid -> [FolderUserAR hfid]) (user ^. #homeFolderID)
            <> [UserAR uid]
  in  AccessRoleImplicitUser uid <$> adminOrUserRoles <> userRoles

data GetRolesIncludingInherited = GetRolesIncludingInherited User UserGroup
instance (MonadDB m, MonadThrow m) => DBQuery m GetRolesIncludingInherited [AccessRole] where
  dbQuery (GetRolesIncludingInherited u ug) = do
    let uid   = u ^. #id
        ugid  = ug ^. #id
        roles = derivedRoles u ug
    runQuery_ . sqlSelect "access_control_union acu" $ do
      sqlWith "access_control_union" . sqlSelect "" $ do
        sqlWith "access_control_from_db" . sqlSelect "access_control" $ do
          sqlResult "id"
          sqlResult "role"
          sqlResult "src_user_id"
          sqlResult "src_user_group_id"
          sqlResult "trg_user_id"
          sqlResult "trg_user_group_id"
          sqlResult "trg_folder_id"
          sqlWhereAny [sqlWhereEq "src_user_id" uid, sqlWhereEq "src_user_group_id" ugid]
        let
          derivedRolesVALUES =
            let values = sqlConcatComma . for roles $ \role ->
                  parenthesize $ sqlConcatComma
                    [ "NULL::bigint"
                    , sqlParam (toAccessRoleType $ accessRoleTarget role) <+> "::smallint"
                    , sqlParam (accessRoleGetSourceUserID role) <+> "::bigint"
                    , sqlParam (accessRoleGetSourceUserGroupID role) <+> "::bigint"
                    , sqlParam (accessRoleGetTargetUserID role) <+> "::bigint"
                    , sqlParam (accessRoleGetTargetUserGroupID role) <+> "::bigint"
                    , sqlParam (accessRoleGetTargetFolderID role) <+> "::bigint"
                    ]
            in
              parenthesize ("VALUES" <+> values)
                <+> "AS t (id, role, src_user_id, src_user_group_id, trg_user_id, trg_user_group_id, trg_folder_id)"
        sqlWith "access_control_derived" . sqlSelect derivedRolesVALUES $ do
          sqlResult "*"
        sqlResult
          "* FROM access_control_from_db UNION SELECT * FROM access_control_derived"
      sqlLeftJoinOn
        "folders f"
        "f.id=acu.trg_folder_id or (f.parent_path @> array[acu.trg_folder_id])"
      sqlLeftJoinOn
        "user_groups ug"
        "ug.id=acu.trg_user_group_id or (ug.parent_group_path @> array[acu.trg_user_group_id])"
      sqlResult "acu.id as id"
      sqlResult "acu.role as role"
      sqlResult "acu.src_user_id as src_user_id"
      sqlResult "acu.src_user_group_id as src_user_group_id"
      sqlResult "acu.trg_user_id"
      sqlResult "ug.id as trg_user_group_id"
      sqlResult "f.id as trg_folder_id"
    fetchMany fetchAccessRole

data AccessControlGetExplicitRoles = AccessControlGetExplicitRoles UserID UserGroupID
instance (MonadDB m, MonadThrow m)
  => DBQuery m AccessControlGetExplicitRoles [AccessRole] where
  dbQuery (AccessControlGetExplicitRoles uid ugid) = do
    runQuery_ . sqlSelect "access_control" $ do
      sqlWhereAny [sqlWhereEq "src_user_id" uid, sqlWhereEq "src_user_group_id" ugid]
      mapM_ sqlResult rolesSelector
    fetchMany fetchAccessRole

newtype AccessControlDeleteRolesByUserGroup
  = AccessControlDeleteRolesByUserGroup UserGroupID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m AccessControlDeleteRolesByUserGroup () where
  dbUpdate (AccessControlDeleteRolesByUserGroup ugid) = do
    runQuery_ . sqlDelete "access_control" $ sqlWhereAny
      [ sqlWhereEq "src_user_group_id" $ Just ugid
      , sqlWhereEq "trg_user_group_id" $ Just ugid
      ]

newtype AccessControlDeleteRolesByFolder
  = AccessControlDeleteRolesByFolder FolderID
instance (MonadDB m, MonadThrow m) =>
  DBUpdate m AccessControlDeleteRolesByFolder () where
  dbUpdate (AccessControlDeleteRolesByFolder fdrid) = do
    runQuery_ . sqlDelete "access_control" . sqlWhereEq "trg_folder_id" $ Just fdrid

newtype AccessControlRemoveRole = AccessControlRemoveRole AccessRoleID
instance (MonadDB m, MonadThrow m)
  => DBUpdate m AccessControlRemoveRole Bool where
  dbUpdate (AccessControlRemoveRole roleId) =
    runQuery01 . sqlDelete "access_control" $ sqlWhereEq "id" roleId

data AccessControlInsertRoleForUser =
    AccessControlInsertRoleForUser UserID AccessRoleTarget
instance (MonadDB m, MonadThrow m) => DBUpdate m AccessControlInsertRoleForUser Bool where
  dbUpdate (AccessControlInsertRoleForUser uid trg) = do
    runQuery01 . sqlInsert "access_control" $ do
      sqlSet "role"        (toAccessRoleType trg)
      sqlSet "src_user_id" uid
      setTarget trg

data AccessControlRemoveUserGroupAdminRole =
    AccessControlRemoveUserGroupAdminRole UserID UserGroupID
instance (MonadDB m, MonadThrow m) =>
    DBUpdate m AccessControlRemoveUserGroupAdminRole Bool where
  dbUpdate (AccessControlRemoveUserGroupAdminRole uid ugid) = do
    runQuery01 . sqlDelete "access_control" $ do
      sqlWhereEq "role" . toAccessRoleType $ UserGroupAdminAR ugid
      sqlWhereEq "src_user_id"       uid
      sqlWhereEq "trg_user_group_id" ugid

addInheritedRoles :: (MonadDB m, MonadThrow m) => [AccessRole] -> m [AccessRole]
addInheritedRoles roles = concatForM roles $ \baseRole ->
  case accessRoleTarget baseRole of
    UserAdminAR ugid -> mkRolesForUGChildren ugid UserAdminAR baseRole
    UserGroupAdminAR ugid -> mkRolesForUGChildren ugid UserGroupAdminAR baseRole
    SharedTemplateUserAR fid -> mkRolesForFdrChildren fid SharedTemplateUserAR baseRole
    FolderAdminAR fid -> mkRolesForFdrChildren fid FolderAdminAR baseRole
    FolderUserAR fid -> mkRolesForFdrChildren fid FolderUserAR baseRole
    _ -> return [baseRole]
  where
    mkRolesForUGChildren ugid roleCons role = do
      ugwcs <- dbQuery $ UserGroupGetAllChildrenRecursive ugid
      return . (role :) . for (ugwcToList ugwcs) $ \ug ->
        accessRoleSetTarget (roleCons $ ug ^. #id) role

    mkRolesForFdrChildren fid roleCons role = do
      fwcs <- dbQuery $ FolderGetAllChildrenRecursive fid
      return . (role :) . for (concatMap fwcToList fwcs) $ \fdr ->
        accessRoleSetTarget (roleCons $ fdr ^. #id) role

fetchAccessRole
  :: ( Maybe AccessRoleID
     , AccessRoleType
     , Maybe UserID
     , Maybe UserGroupID
     , Maybe UserID
     , Maybe UserGroupID
     , Maybe FolderID
     )
  -> AccessRole
fetchAccessRole (Just rid, rtype, Just uid, Nothing, trg_uid, trg_ugid, trg_foler) =
  AccessRoleUser rid uid $ fetchAccessRoleTarget (rtype, trg_uid, trg_ugid, trg_foler)
fetchAccessRole (Just rid, rtype, Nothing, Just ugid, trg_uid, trg_ugid, trg_foler) =
  AccessRoleUserGroup rid ugid
    $ fetchAccessRoleTarget (rtype, trg_uid, trg_ugid, trg_foler)
fetchAccessRole (Nothing, rtype, Just uid, Nothing, trg_uid, trg_ugid, trg_foler) =
  AccessRoleImplicitUser uid $ fetchAccessRoleTarget (rtype, trg_uid, trg_ugid, trg_foler)
fetchAccessRole (Nothing, rtype, Nothing, Just ugid, trg_uid, trg_ugid, trg_foler) =
  AccessRoleImplicitUserGroup ugid
    $ fetchAccessRoleTarget (rtype, trg_uid, trg_ugid, trg_foler)
fetchAccessRole row =
  unexpectedError $ "invalid access_control row in database" <> showt row



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
fetchAccessRoleTarget (FolderUserART , Nothing, Nothing, Just fid) = FolderUserAR fid
fetchAccessRoleTarget (SharedTemplateUserART, Nothing, Nothing, Just fid) =
  SharedTemplateUserAR fid
fetchAccessRoleTarget (EidImpersonatorART, Nothing, Just ugid, Nothing) =
  EidImpersonatorAR ugid
fetchAccessRoleTarget row =
  unexpectedError $ "invalid access_control row in database" <> showt row
