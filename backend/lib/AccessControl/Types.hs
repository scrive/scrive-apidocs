{-# LANGUAGE ExistentialQuantification #-}
module AccessControl.Types
  ( accessControl
  , AccessAction(..)
  , AccessPolicy
  , AccessResource(..)
  , AccessRole(..)
  , NeedsPermissions(..)
  , HasPermissions(..)
  , makePolicyItem )
  where

import Control.Monad.Catch
import Data.Typeable (Typeable, cast)
import Log

import DB
import User.UserID
import UserGroup.Types
import UserGroup.Model

-- | The roles we use are mostly rooted in some user group; rather than have
-- this implicit in implementation we expose it in the constructors. The meaning
-- is that for the supplied UserGroupID, say, the user has the role thus defined
-- (e.g. 'UserAR 1234' would mean "for user group ID 1234 the user is a regular user")
data AccessRole t
  = UserAR t
  -- ^ A regular user; may e.g. use the system but not make structural changes
  | UserAdminAR t
  -- ^ A user admin; may e.g. CRUD users but not add user groups
  | UserGroupAdminAR t
  -- ^ A user group admin; may do most things like adding and moving user groups
  deriving (Eq, Show)

-- | We need to discern between permissions and actions that affect users, user
-- groups, policies and more.
data AccessResource
  = UserR
  | UserGroupR
  | UserPolicyR
  | UserGroupPolicyR
  | UserPersonalTokenR
  deriving (Eq, Show, Enum, Bounded)

-- | Should be self-explanatory. The 'A' stands for 'Action'.
data AccessAction
  = CreateA
  | ReadA
  | UpdateA
  | DeleteA
  deriving (Eq, Show, Typeable)

-- | We use this to bundle different types. We only need to have an instance for
-- 'Eq' when comparing them at the end which is why we derive Typeable.
data Permission =
  forall t. (Eq t, Typeable t, Show t) => Permission AccessAction AccessResource t
  deriving (Typeable)

instance Eq Permission where
  Permission xaa xat x == Permission yaa yat y =
    case cast y of
      Just y' -> x == y' && xaa == yaa && xat == yat
      Nothing -> False

instance Show Permission where
  show (Permission aa at t) =
    "Permission " ++ show aa ++ " " ++ show at ++ " " ++ show t

-- | We need to bundle different types that implement 'NeedsPermissions'.
data AccessibleItem =
  forall t . NeedsPermissions t => AccessibleItem t

type AccessPolicy = [AccessibleItem]

makePolicyItem :: NeedsPermissions t => t -> AccessibleItem
makePolicyItem = AccessibleItem

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

-- | Interface for anything that could grant you some 'Permission's.  The
-- monadic context is there due to future features.
class HasPermissions s where
  hasPermissions :: (MonadCatch m, MonadDB m, MonadThrow m)
                 => s -> m [Permission]

-- | Interface to get the proper combinations of 'Permission's needed to gain
-- access permission.
class NeedsPermissions s where
  neededPermissions :: (MonadCatch m, MonadDB m, MonadThrow m)
                 => s -> m NeededPermissionsExpr

instance HasPermissions (AccessRole UserGroupID) where
  hasPermissions role = return . accessPermissions $ role
    where
    accessPermissions :: AccessRole UserGroupID -> [Permission]
    accessPermissions (UserAR _usrGrpID) = []
    accessPermissions (UserAdminAR usrGrpID) =
        map (construct usrGrpID UserR)              [CreateA, ReadA, UpdateA, DeleteA] <>
        map (construct usrGrpID UserGroupR)         [         ReadA                  ] <>
        map (construct usrGrpID UserPolicyR)        [CreateA, ReadA, UpdateA, DeleteA] <>
        map (construct usrGrpID UserGroupPolicyR)   [CreateA, ReadA, UpdateA, DeleteA] <>
        map (construct usrGrpID UserPersonalTokenR) [                                ]
    accessPermissions (UserGroupAdminAR usrGrpID) =
        let allTargetsAndActions =
                [ (t, a) |
                  a <- [CreateA, ReadA, UpdateA, DeleteA],
                  t <- [minBound..maxBound] ]
        in (map (uncurry (construct usrGrpID)) allTargetsAndActions) <>
            map (construct usrGrpID UserPersonalTokenR) [CreateA, ReadA]

    construct :: UserGroupID -> AccessResource -> AccessAction -> Permission
    construct usrGrpID target action = Permission action target usrGrpID

instance NeedsPermissions (AccessAction, AccessResource, UserGroupID) where
  neededPermissions (action, target, usrGrpID) = do
    -- @todo: important DB-optimisation point: retrieve only the list of parent IDs.
    (dbQuery . UserGroupGetWithParents $ usrGrpID) >>= \case
      Nothing -> unexpectedError $ "No user group with ID" <+> (show $ usrGrpID)
      Just ugwp ->
        -- By specification, it should be enough to have permission for the
        -- wanted action on _any_ parent.
        return . NeededPermissionsExprOr
          . map (\g -> NeededPermissionsExprBase (Permission action target $ get ugID g))
          $ ugwpToUGList ugwp

-- | Convenience instance only since access is enforced on group level.
instance NeedsPermissions (AccessAction, AccessResource, UserID) where
  neededPermissions (action, target, usrID) = do
      usrGrpID <- get ugID <$> (dbQuery . UserGroupGetByUserID $ usrID)
      neededPermissions (action, target, usrGrpID)

instance NeedsPermissions AccessibleItem where
    neededPermissions (AccessibleItem t) = neededPermissions t

accessControl :: (HasPermissions s, MonadCatch m, MonadDB m, MonadThrow m, MonadLog m)
              => [s] -> AccessPolicy -> m a -> m a -> m a
accessControl roles accessPolicy err ma = do
  accHad <- nub . join <$> mapM hasPermissions roles
  accNeeded <- NeededPermissionsExprAnd <$> mapM neededPermissions accessPolicy
  let cond = evalNeededPermExpr (`elem` accHad) accNeeded
  if cond then ma else err
