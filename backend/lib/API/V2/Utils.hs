module API.V2.Utils
    ( apiAccessControl
    , apiAccessControlWithError
    , apiHasPermission
    , apiHasAllPermissions
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

apiHasPermission :: forall  m . (Kontrakcja m) => Permission -> m PermissionCondition
apiHasPermission permission =
  alternativePermissionCondition permission
    `catchDBExtraException` (\(UserNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(UserGroupNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(FolderNonExistent _) -> apiError insufficientPrivileges)

apiHasAllPermissions
  :: forall  m . (Kontrakcja m) => [Permission] -> m PermissionCondition
apiHasAllPermissions = fmap AndCond . mapM apiHasPermission

apiAccessControlWithError
  :: (Kontrakcja m) => User -> PermissionCondition -> m a -> m a -> m a
apiAccessControlWithError apiuser perms failAction successAction = do
  roles <- dbQuery . GetRoles $ apiuser
  accessControl roles perms failAction successAction
    `catchDBExtraException` (\(UserNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(UserGroupNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(FolderNonExistent _) -> apiError insufficientPrivileges)

apiAccessControl :: (Kontrakcja m) => User -> PermissionCondition -> m a -> m a
apiAccessControl user perms successAction = do
  apiAccessControlWithError user perms (apiError insufficientPrivileges) successAction

apiAccessControlCheck :: (Kontrakcja m) => User -> PermissionCondition -> m Bool
apiAccessControlCheck apiUser perms = do
  apiAccessControlWithError apiUser perms (return False) (return True)

apiAccessControlOrIsAdmin :: (Kontrakcja m) => User -> PermissionCondition -> m a -> m a
apiAccessControlOrIsAdmin apiuser perms successAction = do
  isAdminOrSales <- checkAdminOrSales
  -- If scrive admin or sales, should perform action anyway (unless non-existance error)
  let failAction =
        if isAdminOrSales then successAction else apiError insufficientPrivileges
  apiAccessControlWithError apiuser perms failAction successAction

accessControlLoggedIn :: (Kontrakcja m) => PermissionCondition -> m a -> m a
accessControlLoggedIn perms successAction = do
  user  <- guardJustM (view #maybeUser <$> getContext)
  roles <- dbQuery . GetRoles $ user
  accessControl roles perms internalError successAction

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
