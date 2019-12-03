module API.V2.Utils
    ( apiAccessControl
    , accessControlLoggedIn
    , apiAccessControlOrIsAdmin
    , apiAccessControlOrIsAdminWithUser
    , apiAccessControlWithAnyPrivileges
    , apiAccessControlCheck
    , checkAdminOrSales
    , folderOrAPIError
    , isApiAdmin
    , isApiSales
    , userGroupOrAPIError
    , userGroupWithParentsOrAPIError
    ) where

import Control.Monad.Catch

import AccessControl.Model
import AccessControl.Types
import API.V2
import API.V2.Errors
import Context
import DB
import Folder.Model
import Kontra
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.MonadUtils

userAccessControlImpl :: Kontrakcja m => User -> AccessPolicy -> m a -> m a -> m a
userAccessControlImpl apiuser acc failAction successAction = do
  roles <- dbQuery . GetRoles $ apiuser
  accessControl roles acc failAction successAction
    `catchDBExtraException` (\(UserNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(UserGroupNonExistent _) -> apiError insufficientPrivileges)
    `catchDBExtraException` (\(FolderNonExistent _) -> apiError insufficientPrivileges)

apiAccessControl :: Kontrakcja m => User -> AccessPolicy -> m a -> m a
apiAccessControl user acc successAction = do
  userAccessControlImpl user acc (apiError insufficientPrivileges) successAction

apiAccessControlCheck :: Kontrakcja m => User -> AccessPolicy -> m Bool
apiAccessControlCheck apiUser acc = do
  userAccessControlImpl apiUser acc (return False) (return True)

apiAccessControlCheck :: Kontrakcja m => AccessPolicy -> m Bool
apiAccessControlCheck acc = do
  apiuser <- fst <$> getAPIUserWithPrivileges [APIPersonal]
  userAccessControlImpl apiuser acc (return False) (return True)

-- TEMPORARY: Remove after release 90. Implementation of apiAccessControlOrIsAdmin
-- will be the same after release 90.
apiAccessControlOrIsAdminWithUser :: Kontrakcja m => User -> AccessPolicy -> m a -> m a
apiAccessControlOrIsAdminWithUser apiuser acc successAction = do
  isAdminOrSales <- checkAdminOrSales
  -- If scrive admin or sales, should perform action anyway (unless non-existance error)
  let failAction =
        if isAdminOrSales then successAction else apiError insufficientPrivileges
  userAccessControlImpl apiuser acc failAction successAction

apiAccessControlOrIsAdmin :: Kontrakcja m => AccessPolicy -> m a -> m a
apiAccessControlOrIsAdmin acc successAction = do
  isAdminOrSales <- checkAdminOrSales
  -- If scrive admin or sales, should perform action anyway (unless non-existance error)
  let failAction =
        if isAdminOrSales then successAction else apiError insufficientPrivileges
  userAccessControlImpl apiuser acc failAction successAction

accessControlLoggedIn :: Kontrakcja m => AccessPolicy -> m a -> m a
accessControlLoggedIn acc successAction = do
  user  <- guardJustM $ (view #maybeUser <$> getContext)
  roles <- dbQuery . GetRoles $ user
  accessControl roles acc internalError successAction

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

userGroupOrAPIError :: (MonadDB m, MonadThrow m) => UserGroupID -> m UserGroup
userGroupOrAPIError ugid = dbQuery (UserGroupGet ugid) >>= \case
  Nothing ->
    apiError $ serverError "Impossible happened: No user group with ID, or deleted."
  Just ug -> return ug

userGroupWithParentsOrAPIError
  :: (MonadDB m, MonadThrow m) => UserGroupID -> m UserGroupWithParents
userGroupWithParentsOrAPIError ugid = dbQuery (UserGroupGetWithParents ugid) >>= \case
  Nothing ->
    apiError $ serverError "Impossible happened: No user group with ID, or deleted."
  Just ugwp -> return ugwp

folderOrAPIError :: (MonadDB m, MonadThrow m) => FolderID -> m Folder
folderOrAPIError fdrid = dbQuery (FolderGet fdrid) >>= \case
  Nothing  -> apiError $ serverError "Impossible happened: No folder with ID, or deleted."
  Just fdr -> return fdr
