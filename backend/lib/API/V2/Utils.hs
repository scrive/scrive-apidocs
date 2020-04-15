module API.V2.Utils
    ( apiAccessControl
    , apiAccessControlWithError
    , apiRequirePermission
    , apiRequireAllPermissions
    , apiRequireAnyPermission
    , accessControlLoggedIn
    , apiAccessControlOrIsAdmin
    , apiAccessControlCheck
    , checkAdminOrSales
    , isApiAdmin
    , isApiSales
    ) where

import AccessControl.Check
import AccessControl.Model
import AccessControl.Types
import API.V2
import API.V2.Errors
import Context
import DB
import Kontra
import User.Model
import Util.MonadUtils

apiRequirePermission :: forall  m . (Kontrakcja m) => Permission -> m PermissionCondition
apiRequirePermission permission =
  alternativePermissionCondition permission
    `catchDBExtraException` (\(UserNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(UserGroupNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(FolderNonExistent _) -> apiError insufficientPrivileges)

apiRequireAllPermissions
  :: forall  m . (Kontrakcja m) => [Permission] -> m PermissionCondition
apiRequireAllPermissions = fmap AndCond . mapM apiRequirePermission

apiRequireAnyPermission
  :: forall  m . (Kontrakcja m) => [Permission] -> m PermissionCondition
apiRequireAnyPermission = fmap OrCond . mapM apiRequirePermission

apiAccessControlWithError
  :: (Kontrakcja m) => User -> PermissionCondition -> m a -> m a -> m a
apiAccessControlWithError apiuser requiredPerm failAction successAction = do
  roles <- dbQuery . GetRoles $ apiuser
  accessControl roles requiredPerm failAction successAction
    `catchDBExtraException` (\(UserNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(UserGroupNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(FolderNonExistent _) -> apiError insufficientPrivileges)

apiAccessControl :: (Kontrakcja m) => User -> PermissionCondition -> m a -> m a
apiAccessControl user requiredPerm successAction = do
  apiAccessControlWithError user
                            requiredPerm
                            (apiError insufficientPrivileges)
                            successAction

apiAccessControlCheck :: (Kontrakcja m) => User -> PermissionCondition -> m Bool
apiAccessControlCheck apiUser requiredPerm = do
  apiAccessControlWithError apiUser requiredPerm (return False) (return True)

apiAccessControlOrIsAdmin :: Kontrakcja m => User -> PermissionCondition -> m a -> m a
apiAccessControlOrIsAdmin apiuser requiredPerm successAction = do
  isAdminOrSales <- checkAdminOrSales
  -- If scrive admin or sales, should perform action anyway (unless non-existance error)
  let failAction =
        if isAdminOrSales then successAction else apiError insufficientPrivileges
  apiAccessControlWithError apiuser requiredPerm failAction successAction

accessControlLoggedIn :: Kontrakcja m => PermissionCondition -> m a -> m a
accessControlLoggedIn requiredPerm successAction = do
  user  <- guardJustM (view #maybeUser <$> getContext)
  roles <- dbQuery . GetRoles $ user
  accessControl roles requiredPerm internalError successAction

checkAdminOrSales :: Kontrakcja m => m Bool
checkAdminOrSales = (isApiAdmin || isApiSales) <$> getContext

{- Logged in user is admin with 2FA (2FA only enforced for production = true) -}
isApiAdmin :: Context -> Bool
isApiAdmin ctx = case ctx ^. #maybeApiUser of
  Nothing -> False
  Just user ->
    (user ^. #info % #email `elem` ctx ^. #adminAccounts)
      && (user ^. #totpActive || not (ctx ^. #production))

{- Logged in user is sales with 2FA (2FA only enforced for production = true) -}
isApiSales :: Context -> Bool
isApiSales ctx = case ctx ^. #maybeApiUser of
  Nothing -> False
  Just user ->
    (user ^. #info % #email `elem` ctx ^. #salesAccounts)
      && (user ^. #totpActive || not (ctx ^. #production))
