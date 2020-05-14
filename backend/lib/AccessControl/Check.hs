module AccessControl.Check
  ( accessControl
  , accessControlCheck
  , hasPermissions
  , canDo
  , canGrant
  , alternativePermissionCondition
  )
where

import Control.Monad.Catch (MonadThrow(..))
import Data.List.Extra (nubOrd)

import AccessControl.Types
import DB
import Folder.Model
import User.Model.Query
import UserGroup.Model
import UserGroup.Types

-- When granting a role we check that we can grant all permissions.
-- When granting a permission, following is a rule:
--   Permission PermCanDo      - cannot grant any Permissions
--   Permission PermCanGrant   - can grant both PermCanDo and PermCanGrant permissions
canDo :: AccessAction -> AccessResource -> Permission
canDo = Permission PermCanDo

--   calling addAlternativePermissions for every Permission will retrieve the same parents from DB
--   many times (once for each permission). This is acceptable for now because roles are not being
--   granted very often.
canGrant :: AccessRoleTarget -> [Permission]
canGrant = nubOrd . map (\p -> p { permKind = PermCanGrant }) . hasPermissions

crudActions :: [AccessAction]
crudActions = [CreateA, ReadA, UpdateA, DeleteA]

accessControlWith :: (Monad m) => [Permission] -> PermissionCondition -> m a -> m a -> m a
accessControlWith availablePerms requiredPerms onError onSuccess = do
  if accessControlCheck availablePerms requiredPerms then onSuccess else onError

accessControl :: (Monad m) => [AccessRole] -> PermissionCondition -> m a -> m a -> m a
accessControl roles =
  accessControlWith . join $ fmap (hasPermissions . accessRoleTarget) roles

accessControlCheck :: [Permission] -> PermissionCondition -> Bool
accessControlCheck availablePerms =
  evalPermissionCondition (\perm -> elem perm $ Data.List.Extra.nubOrd availablePerms)

evalPermissionCondition :: (Permission -> Bool) -> PermissionCondition -> Bool
evalPermissionCondition f (Cond    p   ) = f p
evalPermissionCondition f (OrCond  aces) = or $ fmap (evalPermissionCondition f) aces
evalPermissionCondition f (AndCond aces) = and $ fmap (evalPermissionCondition f) aces

-- By specification, it should be enough to have permission for the
-- wanted action on _any_ parent.
alternativePermissionCondition
  :: forall  m . (MonadThrow m, MonadDB m) => Permission -> m PermissionCondition
alternativePermissionCondition perm = case permResource perm of
  UserR              uid  -> addForAllParentsUid UserInGroupR uid
  UserInGroupR       ugid -> addForAllParentsUgid UserInGroupR ugid
  UserGroupR         ugid -> addForAllParentsUgid UserGroupR ugid
  UserPersonalTokenR uid  -> addForAllParentsUid PersonalTokenOfAnyUserInGroupR uid
  PersonalTokenOfAnyUserInGroupR ugid ->
    addForAllParentsUgid PersonalTokenOfAnyUserInGroupR ugid
  DocumentInFolderR         fid  -> addForAllParentsFid DocumentInFolderR fid
  FolderR                   fid  -> addForAllParentsFid FolderR fid
  SharedTemplateR           fid  -> addForAllParentsFid SharedTemplateR fid
  DocumentAfterPreparationR fid  -> addForAllParentsFid DocumentAfterPreparationR fid
  EidIdentityR              ugid -> addForAllParentsUgid EidIdentityR ugid
  where
    addForAllParentsFid
      :: (FolderID -> AccessResource) -> FolderID -> m PermissionCondition
    addForAllParentsFid mkRes fid = dbQuery (FolderGet fid) >>= \case
      Nothing     -> throwM . SomeDBExtraException $ FolderNonExistent fid
      Just folder -> do
        folderParents <- dbQuery . FolderGetParents $ fid
        let mkExprBase f = Cond $ perm { permResource = mkRes $ f ^. #id }
        return . OrCond . map mkExprBase $ (folder : folderParents)

    addForAllParentsUgid mkRes ugid = dbQuery (UserGroupGetWithParents ugid) >>= \case
      Nothing   -> throwM . SomeDBExtraException . UserGroupNonExistent $ ugid
      Just ugwp -> do
        let mkExprBase g = Cond $ perm { permResource = mkRes $ g ^. #id }
        return . OrCond . map mkExprBase $ ugwpToList ugwp

    addForAllParentsUid mkRes uid = dbQuery (GetUserByID uid) >>= maybe
      (throwM . SomeDBExtraException $ UserNonExistent uid)
      (\_ -> dbQuery (UserGroupGetWithParentsByUserID uid) >>= \ugwp -> do
        let mkExprBase g = Cond $ perm { permResource = mkRes $ g ^. #id }
        return . OrCond . (Cond perm :) . map mkExprBase $ ugwpToList ugwp
      )
hasPermissions :: AccessRoleTarget -> [Permission]
hasPermissions (UserAR uid) =
  -- user can read and update himself and cannot grant it to anyone
  [ Permission PermCanDo action $ UserR uid | action <- [ReadA, UpdateA, DeleteA] ]
hasPermissions (UserGroupMemberAR _) =
  -- no special permissions for members
  []
-- UserAdminAR can grant the whole role to others
hasPermissions (UserAdminAR ugid) =
       -- can CRUD users
       -- can allow someone else to manipulate users of this group and subgroups (and revoke)
  [ Permission kind action $ UserInGroupR ugid
    | kind   <- [PermCanDo, PermCanGrant]
    , action <- crudActions
    ]
    <>
       -- can read group and sub-groups
       -- can allow someone else to Read groups and subgroups
       [ Permission kind ReadA $ UserGroupR ugid | kind <- [PermCanDo, PermCanGrant] ]
    <>
       -- can CRUD tokens for all users of this group and subgroups
       -- and can grant this right to someone else
       [ Permission kind action $ PersonalTokenOfAnyUserInGroupR ugid
       | kind <- [PermCanDo, PermCanGrant]
       , action <- crudActions
       ]
-- UserGroupAdminAR can grant the whole role to others
hasPermissions (UserGroupAdminAR ugid) =
  -- can perform all actions upon a user group
  [ Permission kind action $ resource ugid
  | kind     <- [PermCanDo, PermCanGrant]
  , action   <- crudActions
  , resource <- [UserInGroupR, UserGroupR, PersonalTokenOfAnyUserInGroupR]
  ]
-- FolderAdminAR can grant the whole role to others
hasPermissions (FolderAdminAR fid) =
  -- can perform and grant all actions upon folder and sub-folders
  [ Permission kind action $ FolderR fid
    | kind   <- [PermCanDo, PermCanGrant]
    , action <- crudActions
    ]

    <>
  -- can perform and grant CRUD on documents in the folder
       [ Permission kind action $ DocumentAfterPreparationR fid
       | kind <- [PermCanDo, PermCanGrant]
       , action <- crudActions
       ]

hasPermissions (FolderUserAR fid) =
  -- can read the folder and cannot grant the permission to anyone else
  [Permission PermCanDo ReadA $ FolderR fid]
    <>
  -- can CRUD documents in the folder, but not grant it to anyone else
       [ Permission PermCanDo action $ DocumentInFolderR fid | action <- crudActions ]

hasPermissions (SharedTemplateUserAR fid) =
  [Permission PermCanDo ReadA $ SharedTemplateR fid]
hasPermissions (EidImpersonatorAR ugid) =
  [Permission PermCanDo ReadA $ EidIdentityR ugid]
