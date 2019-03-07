{-# LANGUAGE ExistentialQuantification #-}
module AccessControl.Types
  ( accessControl
  , toAccessRoleType
  , AccessAction(..)
  , AccessPolicy
  , AccessResource(..)
  , AccessRole(..)
  , AccessRoleType(..)
  , NeedsPermissions(..)
  , mkAccPolicy
  , mkAccPolicyItem)
  where

import Control.Monad.Catch
import Data.Int (Int16)
import Data.Typeable (Typeable, cast)
import Log
import qualified Control.Exception.Lifted as E

import DB
import Folder.Model
import User.UserID
import UserGroup.Model
import UserGroup.Types

-- | The roles we use are mostly rooted in some user group; rather than have
-- this implicit in implementation we expose it in the constructors. The meaning
-- is that for the supplied UserGroupID, say, the user has the role thus defined
-- (e.g. 'UserGroupMemberAR 1234' would mean "for user group ID 1234 the user is a regular user")
data AccessRole
  = UserAR UserID
  -- ^ A regular user; may read and edit himself
  | UserGroupMemberAR UserGroupID
  -- ^ A regular user; may e.g. use the system but not make structural changes
  | UserAdminAR UserGroupID
  -- ^ A users admin; admin of all users in a user group.
  --   May e.g. CRUD users but not add user groups
  | UserGroupAdminAR UserGroupID
  -- ^ A user group admin; may do most things like adding and moving user groups
  | DocumentAdminAR FolderID
  -- ^ Document admin can do anything with documents in a Folder
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
  deriving (Eq, Show, Enum, Bounded)

-- | Should be self-explanatory. The 'A' stands for 'Action'.
data AccessAction
  = CreateA
  | ReadA
  | UpdateA
  | DeleteA
  deriving (Eq, Show, Typeable, Bounded, Enum)

-- | We use this to bundle different types. We only need to have an instance for
-- 'Eq' when comparing them at the end which is why we derive Typeable.
data Permission =
  forall t. (Eq t, Typeable t, Show t) =>
  Permission AccessAction AccessResource t
  deriving (Typeable)

instance Eq Permission where
  Permission xaa xat x == Permission yaa yat y =
    case cast y of
      Just y' -> x == y' && xaa == yaa && xat == yat
      _ -> False

instance Show Permission where
  show (Permission aa at t) =
    "Permission " ++ show aa ++ " " ++ show at ++ " " ++ show t

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
  deriving (Eq, Show)

evalNeededPermExpr :: (Permission -> Bool) -> NeededPermissionsExpr -> Bool
evalNeededPermExpr f (NeededPermissionsExprBase p) = f p
evalNeededPermExpr f (NeededPermissionsExprOr aces) = or $ fmap (evalNeededPermExpr f) aces
evalNeededPermExpr f (NeededPermissionsExprAnd aces) = and $ fmap (evalNeededPermExpr f) aces

-- local helper for mapping in `hasPermissions`
mkPerm :: forall t. (Eq t, Typeable t, Show t) =>
       t -> AccessResource -> AccessAction -> Permission
mkPerm t res act = Permission act res t

hasPermissions :: AccessRole -> [Permission]
hasPermissions (UserAR usrID) =
  -- user can read, update and delete himself
  map (mkPerm usrID UserR) [ReadA, UpdateA]
hasPermissions (UserGroupMemberAR _usrGrpID) = []  -- no special permissions for members
hasPermissions (UserAdminAR usrGrpID) =
  -- can CRUD users
  map (mkPerm usrGrpID UserR)              allActions <>
  -- can read sub-groups
  map (mkPerm usrGrpID UserGroupR)         [ReadA]  <>
  -- can set any permission to any user
  map (mkPerm usrGrpID UserPolicyR)        allActions <>
  -- can set any permission to any sub-group
  map (mkPerm usrGrpID UserGroupPolicyR)   allActions <>
  -- can CRUD tokens for all users
  map (mkPerm usrGrpID UserPersonalTokenR) allActions
    where allActions = [minBound..maxBound]
hasPermissions (UserGroupAdminAR usrGrpID) =
  [ mkPerm usrGrpID res act | act <- [minBound..maxBound], res <- [minBound..maxBound] ]
hasPermissions (DocumentAdminAR fid) =
  map (mkPerm fid DocumentR) [minBound..maxBound]

-- | Interface to get the proper combinations of 'Permission's needed to gain
-- access permission.
class NeedsPermissions s where
  neededPermissions :: (MonadCatch m, MonadDB m, MonadThrow m)
                    => s -> m NeededPermissionsExpr

instance NeedsPermissions (AccessAction, AccessResource, UserGroupID) where
  neededPermissions (action, resource, usrGrpID) = do
    (dbQuery . UserGroupGetWithParents $ usrGrpID) >>= \case
      Nothing -> unexpectedError $ "No user group with ID" <+> (show $ usrGrpID)
      Just ugwp -> do
        -- By specification, it should be enough to have permission for the
        -- wanted action on _any_ parent.
        let mkExprBase g = NeededPermissionsExprBase
                             (Permission action resource $ get ugID g)
        return . NeededPermissionsExprOr . map mkExprBase $ ugwpToList ugwp

instance NeedsPermissions (AccessAction, AccessResource, FolderID) where
  neededPermissions (action, resource, fid) = do
    (query . FolderGet $ fid) >>= \case
      Nothing -> unexpectedError $ "No folder with ID" <+>
                                   show fid
      Just folder -> do
        folderParents <- dbQuery . FolderGetParents $ fid
        let mkExprBase g = NeededPermissionsExprBase
                             (Permission action resource $ get folderID g)
        return . NeededPermissionsExprOr . map mkExprBase $ (folder:folderParents)

instance NeedsPermissions (AccessAction, AccessResource, UserID) where
  neededPermissions (action, resource, usrID) = do
    usrGrpID <- get ugID <$> (dbQuery . UserGroupGetByUserID $ usrID)
    groupPermissions <- neededPermissions (action, resource, usrGrpID)
    return $ NeededPermissionsExprOr
      [ NeededPermissionsExprBase . Permission action resource $ usrID
      , groupPermissions
      ]

instance NeedsPermissions AccessPolicyItem where
  neededPermissions (AccessPolicyItem t) = neededPermissions t

accessControl :: (MonadCatch m, MonadDB m, MonadThrow m, MonadLog m)
              => [AccessRole] -> AccessPolicy -> m a -> m a -> m a
accessControl roles accessPolicy err ma = do
  let accHad = nub . join $ map hasPermissions roles
  accNeeded <- NeededPermissionsExprAnd <$> mapM neededPermissions accessPolicy
  let cond = evalNeededPermExpr (`elem` accHad) accNeeded
  if cond then ma else err

-- IO (DB, frontend) boilerplate

instance PQFormat AccessRole where
  pqFormat = "%access_role"

data AccessRoleType
  = UserART
  | UserGroupMemberART
  | UserAdminART
  | UserGroupAdminART
  | DocumentAdminART
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
      4 -> return DocumentAdminART
      _  -> E.throwIO $ RangeError {
        reRange = [(0, 4)]
      , reValue = n
      }

instance ToSQL AccessRoleType where
  type PQDest AccessRoleType = PQDest Int16
  toSQL UserART            = toSQL (0 :: Int16)
  toSQL UserGroupMemberART = toSQL (1 :: Int16)
  toSQL UserAdminART       = toSQL (2 :: Int16)
  toSQL UserGroupAdminART  = toSQL (3 :: Int16)
  toSQL DocumentAdminART   = toSQL (4 :: Int16)

instance Show AccessRoleType where
  show UserART            = "user"
  show UserGroupMemberART = "user_group_member"
  show UserAdminART       = "user_admin"
  show UserGroupAdminART  = "user_group_admin"
  show DocumentAdminART   = "document_admin"

instance Read AccessRoleType where
  readsPrec _ "user"              = [(UserART, "")]
  readsPrec _ "user_admin"        = [(UserAdminART, "")]
  readsPrec _ "user_group_admin"  = [(UserGroupAdminART, "")]
  readsPrec _ "user_group_member" = [(UserGroupMemberART, "")]
  readsPrec _ "document_admin"    = [(DocumentAdminART, "")]
  readsPrec _ _  = []

toAccessRoleType :: AccessRole -> AccessRoleType
toAccessRoleType ar =
  case ar of
    UserAR            _ -> UserART
    UserGroupMemberAR _ -> UserGroupMemberART
    UserAdminAR       _ -> UserAdminART
    UserGroupAdminAR  _ -> UserGroupAdminART
    DocumentAdminAR   _ -> DocumentAdminART
