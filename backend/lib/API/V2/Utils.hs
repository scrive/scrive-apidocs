module API.V2.Utils
    ( apiAccessControl
    , accessControlLoggedIn
    , apiAccessControlOrIsAdmin
    , apiAccessControlCheck
    , checkAdminOrSales
    , isApiAdmin
    , isApiSales
    ) where

import AccessControl.Model
import AccessControl.Types
import API.V2
import API.V2.Errors
import Context
import DB
import Kontra
import User.Model
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

apiAccessControlOrIsAdmin :: Kontrakcja m => User -> AccessPolicy -> m a -> m a
apiAccessControlOrIsAdmin apiuser acc successAction = do
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
