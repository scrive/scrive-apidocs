module API.V2.Utils
    ( apiAccessControl
    , apiAccessControlOrIsAdmin
    , checkAdminOrSales
    , isApiAdmin
    , isApiSales
    , makeAPIUserAccessPolicyReq
    , makeAPIUserGroupAccessPolicyReq
    ) where

import Control.Monad.Catch
import Control.Monad.Extra (ifM)

import AccessControl.Types
import API.V2
import API.V2.Errors
import Context
import DB
import Kontra
import OAuth.Model
import User.Model
import UserGroup.Model
import UserGroup.Types

apiAccessControl :: (Kontrakcja m) => AccessPolicy -> m a -> m a
apiAccessControl acc ma = do
  apiuser <- fst <$> getAPIUser APIPersonal
  roles <- dbQuery . GetRoles $ apiuser
  accessControl roles acc (apiError insufficientPrivileges) ma

apiAccessControlOrIsAdmin :: Kontrakcja m => AccessPolicy -> m a -> m a
apiAccessControlOrIsAdmin acc ma =
  ifM checkAdminOrSales ma $ apiAccessControl acc ma

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

makeAPIUserAccessPolicyReq
  :: (MonadDB m, MonadThrow m)
  => AccessAction
  -> AccessResource
  -> UserID
  -> m (AccessPolicyItem, User)
makeAPIUserAccessPolicyReq action resource uid = do
  muser <- dbQuery $ GetUserByID uid
  case muser of
    Nothing -> apiError insufficientPrivileges
    Just user -> return (mkAccPolicyItem (action, resource, uid), user)

makeAPIUserGroupAccessPolicyReq
  :: (MonadDB m, MonadThrow m)
  => AccessAction
  -> AccessResource
  -> UserGroupID
  -> m (AccessPolicyItem, UserGroup)
makeAPIUserGroupAccessPolicyReq action resource uid = do
  mug <- dbQuery $ UserGroupGet uid
  case mug of
    Nothing -> apiError insufficientPrivileges
    Just ug -> return (mkAccPolicyItem (action, resource, uid), ug)
