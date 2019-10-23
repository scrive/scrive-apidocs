module API.V2.Utils
    ( apiAccessControl
    , accessControlLoggedIn
    , apiAccessControlOrIsAdmin
    , apiAccessControlWithAnyPrivileges
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
import OAuth.Model
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

apiAccessControlWithAnyPrivileges :: Kontrakcja m => AccessPolicy -> m a -> m a
apiAccessControlWithAnyPrivileges acc successAction = do
  user <- fst <$> getAPIUserWithAnyPrivileges
  userAccessControlImpl user acc (apiError insufficientPrivileges) successAction

apiAccessControl :: Kontrakcja m => AccessPolicy -> m a -> m a
apiAccessControl acc successAction = do
  apiuser <- fst <$> getAPIUserWithPrivileges [APIPersonal]
  userAccessControlImpl apiuser acc (apiError insufficientPrivileges) successAction

apiAccessControlOrIsAdmin :: Kontrakcja m => AccessPolicy -> m a -> m a
apiAccessControlOrIsAdmin acc successAction = do
  isAdminOrSales <- checkAdminOrSales
  -- If scrive admin or sales, should perform action anyway (unless non-existance error)
  let failAction = if isAdminOrSales then successAction else apiError insufficientPrivileges
  apiuser <- fst <$> getAPIUserWithPrivileges [APIPersonal]
  userAccessControlImpl apiuser acc failAction successAction

accessControlLoggedIn :: Kontrakcja m => AccessPolicy -> m a -> m a
accessControlLoggedIn acc successAction = do
  user <- guardJustM $ ((get ctxmaybeuser) <$> getContext)
  roles <- dbQuery . GetRoles $ user
  accessControl roles acc internalError successAction

checkAdminOrSales :: Kontrakcja m => m Bool
checkAdminOrSales = (isApiAdmin || isApiSales) <$> getContext

{- Logged in user is admin with 2FA (2FA only enforced for production = true) -}
isApiAdmin :: Context -> Bool
isApiAdmin ctx = case get ctxmaybeapiuser ctx of
                Nothing -> False
                Just user -> (useremail (userinfo user) `elem` get ctxadminaccounts ctx)
                            && (usertotpactive user || not (get ctxproduction ctx))

{- Logged in user is sales with 2FA (2FA only enforced for production = true) -}
isApiSales :: Context -> Bool
isApiSales ctx = case get ctxmaybeapiuser ctx of
                Nothing -> False
                Just user -> (useremail (userinfo user) `elem` get ctxsalesaccounts ctx)
                            && (usertotpactive user || not (get ctxproduction ctx))

userGroupOrAPIError :: (MonadDB m, MonadThrow m) => UserGroupID -> m UserGroup
userGroupOrAPIError ugid = dbQuery (UserGroupGet ugid) >>= \case
  Nothing -> apiError $
    serverError "Impossible happened: No user group with ID, or deleted."
  Just ug -> return ug

userGroupWithParentsOrAPIError
  :: (MonadDB m, MonadThrow m)
  => UserGroupID
  -> m UserGroupWithParents
userGroupWithParentsOrAPIError ugid =
  dbQuery (UserGroupGetWithParents ugid) >>= \case
    Nothing -> apiError $
      serverError "Impossible happened: No user group with ID, or deleted."
    Just ugwp -> return ugwp

folderOrAPIError :: (MonadDB m, MonadThrow m) => FolderID -> m Folder
folderOrAPIError fdrid = dbQuery (FolderGet fdrid) >>= \case
  Nothing -> apiError $ serverError "Impossible happened: No folder with ID, or deleted."
  Just fdr -> return fdr
