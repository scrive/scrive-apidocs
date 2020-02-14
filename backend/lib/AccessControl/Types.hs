{-# LANGUAGE ExistentialQuantification #-}
module AccessControl.Types
  ( accessControl
  , accessControlPure
  , toAccessRoleType
  , AccessAction(..)
  , AccessResource(..)
  , AccessRole(..)
  , accessRoleTarget
  , accessRoleSetTarget
  , accessRoleGetSourceUserID
  , accessRoleGetSourceUserGroupID
  , accessRoleGetTargetFolderID
  , accessRoleGetTargetUserID
  , accessRoleGetTargetUserGroupID
  , hasPermissions
  , AccessRoleTarget(..)
  , AccessRoleType(..)
  , UserGroupNonExistent(..)
  , UserNonExistent(..)
  , FolderNonExistent(..)
  , Permission(..)
  , PermissionKind(..)
  , AccessRoleID
  , unsafeAccessRoleID
  , emptyAccessRoleID
  , fromAccessRoleID
  , extractDeleteUserUGID
  , canDo
  , canGrant
  )
  where

import Control.Monad.Catch
import Data.Aeson
import Data.Int
import Data.Typeable (Typeable)
import Data.Unjson
import Happstack.Server
import Log
import Text.JSON.Gen
import qualified Control.Exception.Lifted as E
import qualified Data.Binary as B
import qualified Data.Text as T

import DB
import Folder.Model
import Log.Identifier
import User.Model.Query
import User.UserID
import UserGroup.Model
import UserGroup.Types

data AccessRole
  = AccessRoleUser AccessRoleID UserID AccessRoleTarget
  | AccessRoleUserGroup AccessRoleID UserGroupID AccessRoleTarget
  | AccessRoleImplicitUser UserID AccessRoleTarget
  | AccessRoleImplicitUserGroup UserGroupID AccessRoleTarget
  deriving (Eq, Show)

accessRoleTarget :: AccessRole -> AccessRoleTarget
accessRoleTarget (AccessRoleUser      _ _ target      ) = target
accessRoleTarget (AccessRoleUserGroup _ _ target      ) = target
accessRoleTarget (AccessRoleImplicitUser      _ target) = target
accessRoleTarget (AccessRoleImplicitUserGroup _ target) = target

accessRoleSetTarget :: AccessRoleTarget -> AccessRole -> AccessRole
accessRoleSetTarget new_target (AccessRoleUser roleid userid _) =
  AccessRoleUser roleid userid new_target
accessRoleSetTarget new_target (AccessRoleUserGroup roleid ugid _) =
  AccessRoleUserGroup roleid ugid new_target
accessRoleSetTarget new_target (AccessRoleImplicitUser userid _) =
  AccessRoleImplicitUser userid new_target
accessRoleSetTarget new_target (AccessRoleImplicitUserGroup ugid _) =
  AccessRoleImplicitUserGroup ugid new_target

accessRoleGetTargetUserGroupID :: AccessRole -> Maybe UserGroupID
accessRoleGetTargetUserGroupID role = case accessRoleTarget role of
  UserAR               _    -> Nothing
  UserGroupMemberAR    ugid -> Just ugid
  UserAdminAR          ugid -> Just ugid
  UserGroupAdminAR     ugid -> Just ugid
  FolderAdminAR        _    -> Nothing
  FolderUserAR         _    -> Nothing
  SharedTemplateUserAR _    -> Nothing
  EidImpersonatorAR    ugid -> Just ugid

accessRoleGetTargetFolderID :: AccessRole -> Maybe FolderID
accessRoleGetTargetFolderID role = case accessRoleTarget role of
  UserAR               _   -> Nothing
  UserGroupMemberAR    _   -> Nothing
  UserAdminAR          _   -> Nothing
  UserGroupAdminAR     _   -> Nothing
  FolderAdminAR        fid -> Just fid
  FolderUserAR         fid -> Just fid
  SharedTemplateUserAR fid -> Just fid
  EidImpersonatorAR    _   -> Nothing

accessRoleGetTargetUserID :: AccessRole -> Maybe UserID
accessRoleGetTargetUserID role = case accessRoleTarget role of
  UserAR               uid -> Just uid
  UserGroupMemberAR    _   -> Nothing
  UserAdminAR          _   -> Nothing
  UserGroupAdminAR     _   -> Nothing
  FolderAdminAR        _   -> Nothing
  FolderUserAR         _   -> Nothing
  SharedTemplateUserAR _   -> Nothing
  EidImpersonatorAR    _   -> Nothing

accessRoleGetSourceUserID :: AccessRole -> Maybe UserID
accessRoleGetSourceUserID role = case role of
  AccessRoleUser      _ uid _       -> Just uid
  AccessRoleUserGroup _ _   _       -> Nothing
  AccessRoleImplicitUser      uid _ -> Just uid
  AccessRoleImplicitUserGroup _   _ -> Nothing

accessRoleGetSourceUserGroupID :: AccessRole -> Maybe UserGroupID
accessRoleGetSourceUserGroupID role = case role of
  AccessRoleUser      _ _    _       -> Nothing
  AccessRoleUserGroup _ ugid _       -> Just ugid
  AccessRoleImplicitUser      _    _ -> Nothing
  AccessRoleImplicitUserGroup ugid _ -> Just ugid


-- | The roles we use are mostly rooted in some user group; rather than have
-- this implicit in implementation we expose it in the constructors. The meaning
-- is that for the supplied UserGroupID, say, the user has the role thus defined
-- (e.g. 'UserGroupMemberAR 1234' would mean "for user group ID 1234 the user is a regular user")
data AccessRoleTarget
  = UserAR UserID
  -- ^ A regular user; may read and edit himself
  | UserGroupMemberAR UserGroupID
  -- ^ A regular user; may e.g. use the system but not make structural changes
  | UserAdminAR UserGroupID
  -- ^ A users admin; admin of all users in a user group.
  --   May e.g. CRUD users but not add user groups
  | UserGroupAdminAR UserGroupID
  -- ^ A user group admin; may do most things like adding and moving user groups
  | FolderAdminAR FolderID
  -- ^ A can CRUD Folders and Documents after Preparation
  | FolderUserAR FolderID
  -- ^ Can CRUD Folders and CRUD Documents (including Drafts and Private Templates)
  --   Yes, he can do more than Folder AdminAR, this is intentional.
  --   This way users can see drafts and private templates in their User Home Folder, but
  --   is_company_admin cannot.
  | SharedTemplateUserAR FolderID
  -- ^ Can Read Shared Templates
  | EidImpersonatorAR UserGroupID
  -- ^ A singleton role expressing the permission `EidIdentityR ReadA <group
  -- id>`. A user with this role is allowed to 'impersonate' the given user
  -- group through the 'user_group_to_impersonate_for_eid' document field, using
  -- their EID display name rather than that of their own user group; in that
  -- case the impersonated group is charged for the EID transaction.
  -- Impersonation only applies to Swedish BankID at the moment!
  deriving (Eq, Show)

-- | We need to discern between permissions and actions that affect users, user
-- groups, policies and more.
data AccessResource
  -- This user only
  = UserR UserID
  -- User in Group or any subgroup
  | UserInGroupR UserGroupID
  -- UserGroup or any subgroup
  | UserGroupR UserGroupID
  -- This Users token only
  | UserPersonalTokenR UserID
  -- Token of user in a group or subgroups
  | PersonalTokenOfAnyUserInGroupR UserGroupID
  -- Document in Folder or any subfolder
  | DocumentInFolderR FolderID
  -- Folder or any subfolder
  | FolderR FolderID
  -- Template in Folder or any subfolder
  | SharedTemplateR FolderID
  -- Document after starting (not Draft, not Template) in Folderor any subfolder
  | DocumentAfterPreparationR FolderID
  -- Assignee of this role can use UserGroup (but not subgroups) for EID purposes (Display name and charging)
  | EidIdentityR UserGroupID
  deriving (Eq)

data PermissionKind = PermCanDo | PermCanGrant deriving (Eq, Show)

instance Show AccessResource where
  show (UserR                     _) = "user"
  show (UserInGroupR              _) = "user"
  show (UserGroupR                _) = "user_group"
  show (UserPersonalTokenR        _) = "user_personal_token"
  show (PersonalTokenOfAnyUserInGroupR _) = "user_personal_token"
  show (DocumentInFolderR         _) = "document"
  show (FolderR                   _) = "folder"
  show (SharedTemplateR           _) = "shared_template"
  show (DocumentAfterPreparationR _) = "document_after_preparation"
  show (EidIdentityR              _) = "eid_identity"

-- | Should be self-explanatory. The 'A' stands for 'Action'.
data AccessAction
  = CreateA
  | ReadA
  | UpdateA
  | DeleteA
  deriving (Eq, Enum)

instance Show AccessAction where
  show CreateA = "create"
  show ReadA   = "read"
  show UpdateA = "update"
  show DeleteA = "delete"

-- | Permission describes what action can be performed on what resource.
data Permission =
  Permission
    { permKind ::PermissionKind
    , permAction :: AccessAction
    , permResource :: AccessResource
    }
  deriving (Eq, Show)

-- | An 'NeededPermissionsExpr' is evaluated by means of 'evalNeededPermExpr' and is a
-- wrapper to do boolean logic on several levels.
data NeededPermissionsExpr
  = NeededPermissionsExprBase Permission
  | NeededPermissionsExprOr [NeededPermissionsExpr]
  | NeededPermissionsExprAnd [NeededPermissionsExpr]
  deriving Eq

evalNeededPermExpr :: (Permission -> Bool) -> NeededPermissionsExpr -> Bool
evalNeededPermExpr f (NeededPermissionsExprBase p) = f p
evalNeededPermExpr f (NeededPermissionsExprOr aces) =
  or $ fmap (evalNeededPermExpr f) aces
evalNeededPermExpr f (NeededPermissionsExprAnd aces) =
  and $ fmap (evalNeededPermExpr f) aces

hasPermissions :: AccessRoleTarget -> [Permission]
hasPermissions (UserAR uid) =
  -- user can read and update himself and cannot grant it to anyone
  [ Permission PermCanDo action $ UserR uid | action <- [ReadA, UpdateA] ]
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

crudActions :: [AccessAction]
crudActions = [CreateA, ReadA, UpdateA, DeleteA]

data UserGroupNonExistent = UserGroupNonExistent UserGroupID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue UserGroupNonExistent where
  toJSValue (UserGroupNonExistent ugid) = runJSONGen $ do
    value "message"       ("User Group does not exist" :: String)
    value "user_group_id" (show ugid)

instance DBExtraException UserGroupNonExistent

data UserNonExistent = UserNonExistent UserID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue UserNonExistent where
  toJSValue (UserNonExistent uid) = runJSONGen $ do
    value "message" ("User does not exist" :: String)
    value "user_id" (show uid)

instance DBExtraException UserNonExistent

data FolderNonExistent = FolderNonExistent FolderID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue FolderNonExistent where
  toJSValue (FolderNonExistent uid) = runJSONGen $ do
    value "message"   ("Folder does not exist" :: String)
    value "folder_id" (show uid)

instance DBExtraException FolderNonExistent

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
canGrant = nub . map (\p -> p { permKind = PermCanGrant }) . hasPermissions

-- By specification, it should be enough to have permission for the
-- wanted action on _any_ parent.
addAlternativePermissions
  :: (MonadThrow m, MonadDB m) => Permission -> m NeededPermissionsExpr
addAlternativePermissions perm = case permResource perm of
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
    addForAllParentsFid mkRes fid = dbQuery (FolderGet fid) >>= \case
      Nothing     -> throwM . SomeDBExtraException $ FolderNonExistent fid
      Just folder -> do
        folderParents <- dbQuery . FolderGetParents $ fid
        let mkExprBase f =
              NeededPermissionsExprBase $ perm { permResource = mkRes $ f ^. #id }
        return . NeededPermissionsExprOr . map mkExprBase $ (folder : folderParents)
    addForAllParentsUgid mkRes ugid = dbQuery (UserGroupGetWithParents ugid) >>= \case
      Nothing   -> throwM . SomeDBExtraException . UserGroupNonExistent $ ugid
      Just ugwp -> do
        let mkExprBase g =
              NeededPermissionsExprBase $ perm { permResource = mkRes $ g ^. #id }
        return . NeededPermissionsExprOr . map mkExprBase $ ugwpToList ugwp
    addForAllParentsUid mkRes uid = dbQuery (GetUserByID uid) >>= maybe
      (throwM . SomeDBExtraException $ UserNonExistent uid)
      (\_ -> dbQuery (UserGroupGetWithParentsByUserID uid) >>= \ugwp -> do
        let mkExprBase g =
              NeededPermissionsExprBase $ perm { permResource = mkRes $ g ^. #id }
        return
          . NeededPermissionsExprOr
          . (NeededPermissionsExprBase perm :)
          . map mkExprBase
          $ ugwpToList ugwp
      )

accessControl
  :: (MonadCatch m, MonadDB m, MonadThrow m, MonadLog m)
  => [AccessRole]
  -> [Permission]
  -> m a
  -> m a
  -> m a
accessControl roles permissions err ma = do
  accNeeded <- NeededPermissionsExprAnd <$> mapM addAlternativePermissions permissions
  if accessControlCheck roles accNeeded then ma else err

accessControlCheck :: [AccessRole] -> NeededPermissionsExpr -> Bool
accessControlCheck roles accNeeded =
  let accHad = nub . join $ map (hasPermissions . accessRoleTarget) roles
  in  evalNeededPermExpr (`elem` accHad) accNeeded

accessControlPure :: [AccessRole] -> [Permission] -> Bool
accessControlPure roles permissions =
  accessControlCheck roles
    . NeededPermissionsExprAnd
    . map NeededPermissionsExprBase
    $ permissions

-- IO (DB, frontend) boilerplate

data AccessRoleType
  = UserART
  | UserGroupMemberART
  | UserAdminART
  | UserGroupAdminART
  | FolderAdminART
  | FolderUserART
  | SharedTemplateUserART
  | EidImpersonatorART
  deriving (Eq)

instance PQFormat AccessRoleType where
  pqFormat = pqFormat @Int16

instance FromSQL AccessRoleType where
  type PQBase AccessRoleType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      0 -> return UserART
      1 -> return UserGroupMemberART
      2 -> return UserAdminART
      3 -> return UserGroupAdminART
      -- The DocumentAdmin role was removed, leaving this gap.
      -- When creating a new role, please use number 4 and
      -- remove this comment.
      5 -> return FolderAdminART
      6 -> return FolderUserART
      7 -> return SharedTemplateUserART
      8 -> return EidImpersonatorART
      _ -> E.throwIO $ RangeError { reRange = [(0, 8)], reValue = n }

instance ToSQL AccessRoleType where
  type PQDest AccessRoleType = PQDest Int16
  toSQL UserART               = toSQL (0 :: Int16)
  toSQL UserGroupMemberART    = toSQL (1 :: Int16)
  toSQL UserAdminART          = toSQL (2 :: Int16)
  toSQL UserGroupAdminART     = toSQL (3 :: Int16)
  toSQL FolderAdminART        = toSQL (5 :: Int16)
  toSQL FolderUserART         = toSQL (6 :: Int16)
  toSQL SharedTemplateUserART = toSQL (7 :: Int16)
  toSQL EidImpersonatorART    = toSQL (8 :: Int16)

instance Show AccessRoleType where
  show UserART               = "user"
  show UserGroupMemberART    = "user_group_member"
  show UserAdminART          = "user_admin"
  show UserGroupAdminART     = "user_group_admin"
  show FolderAdminART        = "folder_admin"
  show FolderUserART         = "folder_user"
  show SharedTemplateUserART = "shared_template_user"
  show EidImpersonatorART    = "eid_impersonator"

instance Read AccessRoleType where
  readsPrec _ "user"              = [(UserART, "")]
  readsPrec _ "user_admin"        = [(UserAdminART, "")]
  readsPrec _ "user_group_admin"  = [(UserGroupAdminART, "")]
  readsPrec _ "user_group_member" = [(UserGroupMemberART, "")]
  readsPrec _ "folder_admin"      = [(FolderAdminART, "")]
  readsPrec _ "folder_user"       = [(FolderUserART, "")]
  readsPrec _ "shared_template_user" = [(SharedTemplateUserART, "")]
  readsPrec _ "eid_impersonator"  = [(EidImpersonatorART, "")]
  readsPrec _ _                   = []

instance Unjson AccessRoleType where
  unjsonDef = unjsonInvmapR
    ((maybe (fail "Can't parse AccessRoleType") return) . maybeRead . T.pack)
    show
    unjsonDef

instance ToJSON AccessRoleType where
  toJSON = toJSON . show

instance FromJSON AccessRoleType where
  parseJSON v = do
    roleTypeStr <- parseJSON v
    case maybeRead roleTypeStr of
      Nothing       -> fail "Could not parse Access Role Type"
      Just roleType -> return roleType

toAccessRoleType :: AccessRoleTarget -> AccessRoleType
toAccessRoleType ar = case ar of
  UserAR               _ -> UserART
  UserGroupMemberAR    _ -> UserGroupMemberART
  UserAdminAR          _ -> UserAdminART
  UserGroupAdminAR     _ -> UserGroupAdminART
  FolderAdminAR        _ -> FolderAdminART
  FolderUserAR         _ -> FolderUserART
  SharedTemplateUserAR _ -> SharedTemplateUserART
  EidImpersonatorAR    _ -> EidImpersonatorART

-- AccessRoleID

newtype AccessRoleID = AccessRoleID Int64
  deriving (Eq, Ord)
deriving newtype instance Read AccessRoleID
deriving newtype instance Show AccessRoleID

instance ToJSON AccessRoleID where
  toJSON (AccessRoleID n) = toJSON $ show n

instance FromJSON AccessRoleID where
  parseJSON v = do
    ridStr <- parseJSON v
    case maybeRead ridStr of
      Nothing  -> fail "Could not parse Access Role ID"
      Just rid -> return rid

instance PQFormat AccessRoleID where
  pqFormat = pqFormat @Int64

instance FromSQL AccessRoleID where
  type PQBase AccessRoleID = PQBase Int64
  fromSQL mbase = AccessRoleID <$> fromSQL mbase

instance ToSQL AccessRoleID where
  type PQDest AccessRoleID = PQDest Int64
  toSQL (AccessRoleID n) = toSQL n

instance FromReqURI AccessRoleID where
  fromReqURI = maybeRead . T.pack

unsafeAccessRoleID :: Int64 -> AccessRoleID
unsafeAccessRoleID = AccessRoleID

emptyAccessRoleID :: AccessRoleID
emptyAccessRoleID = AccessRoleID 0

fromAccessRoleID :: AccessRoleID -> Int64
fromAccessRoleID (AccessRoleID ugid) = ugid

instance Identifier AccessRoleID where
  idDefaultLabel = "access_role_id"
  idValue (AccessRoleID k) = int64AsStringIdentifier k

instance B.Binary AccessRoleID where
  put (AccessRoleID ugid) = B.put ugid
  get = fmap AccessRoleID B.get

instance Unjson AccessRoleID where
  unjsonDef = unjsonInvmapR
    ((maybe (fail "Can't parse AccessRoleID") return) . maybeRead . T.pack)
    show
    unjsonDef

-- helpers

extractDeleteUserUGID :: Permission -> Maybe UserGroupID
extractDeleteUserUGID (Permission PermCanDo DeleteA (UserInGroupR ugid)) = Just ugid
extractDeleteUserUGID _ = Nothing
