module AccessControl.API (
    accessControlAPI
  , accessControlAPIV2GetUserRoles
  , accessControlAPIV2Get
  , accessControlAPIV2Delete
  , accessControlAPIV2Add
) where

import Happstack.Server.Types
import Happstack.StaticRouting
import qualified Text.JSON.Gen as J

import AccessControl.JSON
import AccessControl.Model
import AccessControl.Types
import API.V2
import API.V2.Errors
import API.V2.Utils
import DB
import Kontra
import Routing
import User.Model.Query
import User.UserID

accessControlAPI :: Route (Kontra Response)
accessControlAPI =
  dir "api" $ choice [dir "frontend" $ accessControlAPIV2, dir "v2" $ accessControlAPIV2]

accessControlAPIV2 :: Route (Kontra Response)
accessControlAPIV2 = choice
  [ dir "getuserroles" . hGet . toK1 $ accessControlAPIV2GetUserRoles
  , accessControlRolesAPIV2
  ]

accessControlRolesAPIV2 :: Route (Kontra Response)
accessControlRolesAPIV2 = dir "accesscontrol" . dir "roles" $ choice
  [ hGet . toK1 $ accessControlAPIV2Get
  , param . dir "delete" . hPost . toK1 $ accessControlAPIV2Delete
  , dir "add" . hPost . toK0 $ accessControlAPIV2Add
  ]

accessControlAPIV2GetUserRoles :: Kontrakcja m => UserID -> m Response
accessControlAPIV2GetUserRoles uid = api $ do
  -- Check user has permissions to view User
  apiuser <- getAPIUserWithAPIPersonal
  apiAccessControlOrIsAdmin apiuser [mkAccPolicyItem (ReadA, UserR, uid)] $ do
    -- Get roles for user
    dbQuery (GetUserByID uid) >>= \case
      Nothing ->
        apiError $ serverError "Impossible happened (No user with ID, or deleted)"
      Just user -> do
        roles <- addInheritedRoles =<< dbQuery (GetRoles user)
        return . Ok . encodeAccessRoles $ nub roles

accessControlAPIV2Get :: Kontrakcja m => AccessRoleID -> m Response
accessControlAPIV2Get roleId = api $ do
  dbQuery (AccessRoleGet roleId) >>= \case
    Nothing   -> apiError insufficientPrivileges
    Just role -> do
      apiuser <- getAPIUserWithAPIPersonal
      apiAccessControlOrIsAdmin apiuser (roleToAccessPolicyReq role ReadA)
        $ return
        . Ok
        $ encodeAccessRole role

accessControlAPIV2Delete :: Kontrakcja m => AccessRoleID -> m Response
accessControlAPIV2Delete roleId = api $ do
  dbQuery (AccessRoleGet roleId) >>= \case
    Nothing   -> apiError insufficientPrivileges
    Just role -> do
      apiuser <- getAPIUserWithAPIPersonal
      apiAccessControlOrIsAdmin apiuser (roleToAccessPolicyReq role DeleteA) $ do
        void . dbUpdate $ AccessControlRemoveRole roleId
        return . Ok . J.runJSONGen $ do
          J.value "role_id" $ show roleId
          J.value "action" ("deleted" :: String)

accessControlAPIV2Add :: Kontrakcja m => m Response
accessControlAPIV2Add = api $ do
  role    <- getApiRoleParameter
  apiuser <- getAPIUserWithAPIPersonal
  apiAccessControlOrIsAdmin apiuser (roleToAccessPolicyReq role CreateA) $ do
    mrole <- case role of
      AccessRoleUser _ uid target -> dbUpdate $ AccessControlCreateForUser uid target
      AccessRoleUserGroup _ ugid target ->
        dbUpdate $ AccessControlCreateForUserGroup ugid target
      AccessRoleImplicitUser uid target ->
        dbUpdate $ AccessControlCreateForUser uid target
      AccessRoleImplicitUserGroup ugid target ->
        dbUpdate $ AccessControlCreateForUserGroup ugid target
    case mrole of
      Nothing    -> apiError $ serverError "Impossible happened (new role does not exist)"
      Just role' -> return . Ok $ encodeAccessRole role'

-- This helper function constructs a set of roles that you need in order to
-- view/alter some *other* role
roleToAccessPolicyReq :: AccessRole -> AccessAction -> [AccessPolicyItem]
roleToAccessPolicyReq role act =
  -- if the source has user policy on a user group, this requirement will be
  -- also be satisfied automatically
  let mkAccPolicyUser uid = mkAccPolicyItem (act, UserPolicyR, uid)
      mkAccPolicyUserGroup ugid = mkAccPolicyItem (act, UserGroupPolicyR, ugid)
      mkAccPolicyFolder fid = mkAccPolicyItem (act, FolderPolicyR, fid)
      sourceRoleReq = case role of
        AccessRoleUser      _ uid  _       -> mkAccPolicyUser uid
        AccessRoleUserGroup _ ugid _       -> mkAccPolicyUserGroup ugid
        AccessRoleImplicitUser      uid  _ -> mkAccPolicyUser uid
        AccessRoleImplicitUserGroup ugid _ -> mkAccPolicyUserGroup ugid
      targetRoleReq = case accessRoleTarget role of
        UserAR               uid  -> mkAccPolicyUser uid
        UserGroupMemberAR    ugid -> mkAccPolicyUserGroup ugid
        UserAdminAR          ugid -> mkAccPolicyUserGroup ugid
        UserGroupAdminAR     ugid -> mkAccPolicyUserGroup ugid
        FolderAdminAR        fid  -> mkAccPolicyFolder fid
        FolderUserAR         fid  -> mkAccPolicyFolder fid
        SharedTemplateUserAR fid  -> mkAccPolicyFolder fid
        EidImpersonatorAR    ugid -> mkAccPolicyUserGroup ugid
  in  [sourceRoleReq, targetRoleReq]
