{-# LANGUAGE ExistentialQuantification #-}
module AccessControl.Types
  ( accessControl
  , accessControlPure
  , extractResourceRef
  , toAccessRoleType
  , hasAction
  , hasResource
  , rolesToPermissions
  , AccessAction(..)
  , AccessPolicy
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
  , NeedsPermissions(..)
  , AccessPolicyItem
  , mkAccPolicy
  , mkAccPolicyItem
  , AccessRoleID
  , unsafeAccessRoleID
  , emptyAccessRoleID
  , fromAccessRoleID
  )
  where

import Control.Monad.Catch
import Data.Aeson
import Data.Int
import Data.Typeable (Typeable, cast)
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
  = UserR
  | UserGroupR
  | UserPolicyR
  | UserGroupPolicyR
  | UserPersonalTokenR
  | DocumentR
  | FolderPolicyR
  | FolderR
  | SharedTemplateR
  | DocumentAfterPreparationR
  | EidIdentityR
  deriving (Eq, Enum, Bounded)

instance Show AccessResource where
  show UserR                     = "user"
  show UserGroupR                = "user_group"
  show UserPolicyR               = "user_policy"
  show UserGroupPolicyR          = "user_group_policy"
  show UserPersonalTokenR        = "user_personal_token"
  show DocumentR                 = "document"
  show FolderPolicyR             = "folder_policy"
  show FolderR                   = "folder"
  show SharedTemplateR           = "shared_template"
  show DocumentAfterPreparationR = "document_after_preparation"
  show EidIdentityR              = "eid_identity"


-- | Should be self-explanatory. The 'A' stands for 'Action'.
data AccessAction
  = CreateA
  | ReadA
  | UpdateA
  | DeleteA
  deriving (Eq, Typeable, Bounded, Enum)

instance Show AccessAction where
  show CreateA = "create"
  show ReadA   = "read"
  show UpdateA = "update"
  show DeleteA = "delete"

-- | We use this to bundle different types. We only need to have an instance for
-- 'Eq' when comparing them at the end which is why we derive Typeable.
data Permission =
  forall t. (Eq t, Typeable t, Show t) =>
  Permission AccessAction AccessResource t
  deriving (Typeable)

instance Eq Permission where
  Permission xaa xat x == Permission yaa yat y = case cast y of
    Just y' -> x == y' && xaa == yaa && xat == yat
    _       -> False

-- Bundling by predicate and marshalling helpers
data AccessPolicyItem = forall t. (NeedsPermissions t) => AccessPolicyItem t
type AccessPolicy = [AccessPolicyItem]

mkAccPolicyItem :: (NeedsPermissions t) => t -> AccessPolicyItem
mkAccPolicyItem = AccessPolicyItem

mkAccPolicy :: (NeedsPermissions t) => [t] -> AccessPolicy
mkAccPolicy = map mkAccPolicyItem

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

-- local helper for mapping in `hasPermissions`
mkPerm
  :: forall t
   . (Eq t, Typeable t, Show t)
  => t
  -> AccessResource
  -> AccessAction
  -> Permission
mkPerm t res act = Permission act res t

hasPermissions :: AccessRoleTarget -> [Permission]
hasPermissions (UserAR usrID) =
  -- user can read and update himself
                                map (mkPerm usrID UserR) [ReadA, UpdateA]
  <>
  -- user can grant/revoke permissions related themselves; but only if they have
  -- permissions on both source and target, since that's how the Access Control
  -- API works
     map (mkPerm usrID UserPolicyR) [minBound .. maxBound]
hasPermissions (UserGroupMemberAR _usrGrpID) =
  -- no special permissions for members
  []
hasPermissions (UserAdminAR usrGrpID) =
  -- can CRUD users
  map (mkPerm usrGrpID UserR) allActions
    <>
  -- can read sub-groups
       map (mkPerm usrGrpID UserGroupR)         [ReadA]
    <>
  -- can set/remove any role on any user
       map (mkPerm usrGrpID UserPolicyR)        allActions
    <>
  -- can set/remove any role on any sub-group
       map (mkPerm usrGrpID UserGroupPolicyR)   allActions
    <>
  -- can CRUD tokens for all users
       map (mkPerm usrGrpID UserPersonalTokenR) allActions
  where allActions = [minBound .. maxBound]
hasPermissions (UserGroupAdminAR usrGrpID) =
  -- can perform all actions upon a user group
  [ mkPerm usrGrpID res act
  | act <- [minBound .. maxBound]
  , res <- [minBound .. maxBound]
  ]
hasPermissions (FolderAdminAR fid) =
  -- can perform all actions upon folder
  map (mkPerm fid FolderR) [minBound .. maxBound]
    <>
  -- can CRUD documents in the folder
       map (mkPerm fid DocumentAfterPreparationR) [minBound .. maxBound]
    <>
  -- can set/remove any role on the folder
       map (mkPerm fid FolderPolicyR)             [minBound .. maxBound]
hasPermissions (FolderUserAR fid) =
  -- can read the folder
  map (mkPerm fid FolderR) [minBound .. maxBound]
    <>
  -- does not need document after preparation, because DocumentAfterPreparationR is subset of DocumentR
       map (mkPerm fid DocumentR) [minBound .. maxBound]
hasPermissions (SharedTemplateUserAR fid     ) = [mkPerm fid SharedTemplateR ReadA]
hasPermissions (EidImpersonatorAR    usrGrpID) = [mkPerm usrGrpID EidIdentityR ReadA]

-- | Interface to get the proper combinations of 'Permission's needed to gain
-- access permission.
class NeedsPermissions s where
  neededPermissions :: (MonadCatch m, MonadDB m, MonadThrow m)
                    => s -> m NeededPermissionsExpr
  neededPermissionsPure :: s -> NeededPermissionsExpr

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

instance NeedsPermissions (AccessAction, AccessResource, UserGroupID) where
  neededPermissions (action, resource, usrGrpID) = do
    dbQuery (UserGroupGetWithParents usrGrpID) >>= \case
      Nothing   -> throwM . SomeDBExtraException . UserGroupNonExistent $ usrGrpID
      Just ugwp -> do
        -- By specification, it should be enough to have permission for the
        -- wanted action on _any_ parent.
        let mkExprBase g =
              NeededPermissionsExprBase (Permission action resource $ g ^. #id)
        return . NeededPermissionsExprOr . map mkExprBase $ ugwpToList ugwp
  neededPermissionsPure (action, resource, usrGrpID) =
    NeededPermissionsExprBase $ Permission action resource usrGrpID

instance NeedsPermissions (AccessAction, AccessResource, FolderID) where
  neededPermissions (action, resource, fid) = do
    dbQuery (FolderGet fid) >>= \case
      Nothing     -> throwM . SomeDBExtraException . FolderNonExistent $ fid
      Just folder -> do
        folderParents <- dbQuery . FolderGetParents $ fid
        let mkExprBase g =
              NeededPermissionsExprBase (Permission action resource $ g ^. #id)
        return . NeededPermissionsExprOr . map mkExprBase $ (folder : folderParents)
  neededPermissionsPure (action, resource, fid) =
    NeededPermissionsExprBase $ Permission action resource fid

instance NeedsPermissions (AccessAction, AccessResource, UserID) where
  neededPermissions (action, resource, usrID) = dbQuery (GetUserByID usrID) >>= \case
    Nothing -> throwM . SomeDBExtraException . UserNonExistent $ usrID
    Just _  -> do
      usrGrpID         <- view #id <$> (dbQuery . UserGroupGetByUserID $ usrID)
      groupPermissions <- neededPermissions (action, resource, usrGrpID)
      return $ NeededPermissionsExprOr
        [NeededPermissionsExprBase . Permission action resource $ usrID, groupPermissions]
  neededPermissionsPure (action, resource, usrID) =
    NeededPermissionsExprBase $ Permission action resource usrID

instance NeedsPermissions AccessPolicyItem where
  neededPermissions (AccessPolicyItem t) = neededPermissions t
  neededPermissionsPure (AccessPolicyItem t) = neededPermissionsPure t

accessControl
  :: (MonadCatch m, MonadDB m, MonadThrow m, MonadLog m)
  => [AccessRole]
  -> AccessPolicy
  -> m a
  -> m a
  -> m a
accessControl roles accessPolicy err ma = do
  accNeeded <- NeededPermissionsExprAnd <$> mapM neededPermissions accessPolicy
  if accessControlCheck roles accNeeded then ma else err

accessControlCheck :: [AccessRole] -> NeededPermissionsExpr -> Bool
accessControlCheck roles accNeeded =
  let accHad = nub . join $ map (hasPermissions . accessRoleTarget) roles
  in  evalNeededPermExpr (`elem` accHad) accNeeded

accessControlPure :: [AccessRole] -> AccessPolicy -> Bool
accessControlPure roles accessPolicy =
  accessControlCheck roles . NeededPermissionsExprAnd $ map neededPermissionsPure
                                                            accessPolicy

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

extractResourceRef :: (Typeable t) => Permission -> Maybe t
extractResourceRef (Permission _ _ x) = cast x

hasAction :: Permission -> AccessAction -> Bool
hasAction (Permission xaa _ _) aa = (xaa == aa)

hasResource :: Permission -> AccessResource -> Bool
hasResource (Permission _ xar _) ar = (xar == ar)

rolesToPermissions :: [AccessRole] -> [Permission]
rolesToPermissions rs = join $ map (hasPermissions . accessRoleTarget) rs
