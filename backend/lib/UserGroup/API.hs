module UserGroup.API (
    userGroupAPI
  , userGroupApiV2Create
  , userGroupApiV2Get
  , userGroupApiV2Update
  , userGroupApiV2Delete
  , userGroupApiAddressV2Get
  , userGroupApiAddressV2Update
  , userGroupApiAddressV2Delete
  , userGroupApiSettingsV2Get
  , userGroupApiSettingsV2Update
  , userGroupApiSettingsV2Delete
  , userGroupApiUIV2Get
  , userGroupApiUIV2Update
  , userGroupApiUsersV2Get
) where

import Control.Monad.Extra (unlessM)
import Data.Unjson (arrayOf)
import Happstack.Server.Types
import Happstack.StaticRouting
import qualified Text.JSON.Gen as J

import AccessControl.Types
import API.V2
import API.V2.Errors
import API.V2.Parameters
import API.V2.Utils
import DB
import Kontra
import Routing
import User.JSON
import User.Model
import UserGroup.JSON
import UserGroup.Model
import UserGroup.Types

userGroupAPI :: Route (Kontra Response)
userGroupAPI = dir "api" $ choice
  [
    dir "frontend" $ userGroupAPIV2
  , dir "v2" $ userGroupAPIV2
  ]

userGroupAPIV2 :: Route (Kontra Response)
userGroupAPIV2 = dir "usergroups" $ choice
  [
    hGet . toK1 $ userGroupApiV2Get
  , dir "create" . hPost . toK0 $ userGroupApiV2Create
  , param . dir "update" . hPost . toK1 $ userGroupApiV2Update
  , param . dir "delete" . hPost . toK1 $ userGroupApiV2Delete
  , param . dir "address" . hGet . toK1 $ userGroupApiAddressV2Get
  , param . dir "address" . dir "update" . hPost . toK1 $ userGroupApiAddressV2Update
  , param . dir "address" . dir "delete" . hPost . toK1 $ userGroupApiAddressV2Delete
  , param . dir "settings" . hGet . toK1 $ userGroupApiSettingsV2Get
  , param . dir "settings" . dir "update" . hPost . toK1 $ userGroupApiSettingsV2Update
  , param . dir "settings" . dir "delete" . hPost . toK1 $ userGroupApiSettingsV2Delete
  , param . dir "ui" . hGet . toK1 $ userGroupApiUIV2Get
  , param . dir "ui" . dir "update" . hPost . toK1 $ userGroupApiUIV2Update
  , param . dir "users" . hGet . toK1 $ userGroupApiUsersV2Get
  ]

userGroupApiV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiV2Get ugid = api $
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    dbQuery (UserGroupGet ugid) >>= \case
      Nothing ->
        apiError $ serverError "Impossible happened: No user group with ID, or deleted."
      Just ug -> Ok <$> return (unjsonUserGroup, ug)

userGroupApiV2Create :: Kontrakcja m => m Response
userGroupApiV2Create = api $ do
  ugIn <- apiV2ParameterObligatory $ ApiV2ParameterJSON "usergroup" unjsonUserGroup
  ugOut <- case get ugParentGroupID ugIn of
    Nothing -> do
      -- Guard against non-Admins being able to create root UserGroups
      unlessM checkAdminOrSales $ apiError insufficientPrivileges
      dbUpdate $ UserGroupCreate ugIn
    Just parent_ugid -> do
      -- Check user has permissions to create child UserGroup
      let acc = mkAccPolicyItem (CreateA, UserGroupR, parent_ugid)
      apiAccessControlOrIsAdmin [acc] . dbUpdate $ UserGroupCreate ugIn
  -- Return response
  Ok <$> return (unjsonUserGroup, ugOut)

userGroupApiV2Update :: Kontrakcja m => UserGroupID -> m Response
userGroupApiV2Update ugid = api $ do
  ugNew <- apiV2ParameterObligatory $ ApiV2ParameterJSON "usergroup" unjsonUserGroup
  ugOriginal <- dbQuery (UserGroupGet ugid) >>= \case
    -- Must do manual existance checking since we haven't done access control checks yet
    Nothing -> apiError insufficientPrivileges
    Just ug -> return ug
  let moldparentugid = get ugParentGroupID ugOriginal
  let mnewparentugid = get ugParentGroupID ugNew
  -- Checks to see whether the usergroup is being moved (its parent ugid changed)
  -- Existence checks are implicitly handled via makeAPIUserGroupAccessPolicyReq
  movementAccs <- case (moldparentugid, mnewparentugid) of
    (Just newparentugid, Just oldparentugid) ->
      if newparentugid /= oldparentugid
      then do
        -- The usergroup is being moved from one or another.
        -- Permissions are required for "from" and "to" UserGroups
        let oldAcc = mkAccPolicyItem (UpdateA, UserGroupR, oldparentugid)
            newAcc = mkAccPolicyItem (UpdateA, UserGroupR, newparentugid)
            curAcc = mkAccPolicyItem (DeleteA, UserGroupR, get ugID ugOriginal)
        return [oldAcc, newAcc, curAcc]
      else
        -- The usergroup isn't being moved, no special permissions needed
        return []
    (Nothing, Just oldparentugid) -> do
      -- Only admin or sales can promote UserGroup to root
      unlessM checkAdminOrSales $ apiError insufficientPrivileges
      return [mkAccPolicyItem (UpdateA, UserGroupR, oldparentugid)]
    (Just newparentugid, Nothing) -> do
      -- Root usergroup is being made subordinate to another UserGroup
      return [mkAccPolicyItem (UpdateA, UserGroupR, newparentugid)]
    (Nothing, Nothing) ->
      -- Root usergroup is remaining root, no special privileges needed
      return []
  let acc = mkAccPolicyItem (UpdateA, UserGroupR, ugid)
  apiAccessControlOrIsAdmin (acc:movementAccs) $ do
    -- Invoicing and Features are't included in the UnjsonDef so we need to prevent them
    -- from being overwritten with the default value.
    let ugNew' = ugNew {
      _ugID = ugid,
      _ugInvoicing = get ugInvoicing ugOriginal,
      _ugFeatures = get ugFeatures ugOriginal
    }
    dbUpdate $ UserGroupUpdate ugNew'
    Ok <$> return (unjsonUserGroup, ugNew')

userGroupApiV2Delete :: Kontrakcja m => UserGroupID -> m Response
userGroupApiV2Delete ugid = api $
  -- Check user has permissions to delete UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (DeleteA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    when (isRootUserGroup ug) $
      -- Maybe Sales/Admin users should be allowed
      -- Note: Current implementation causes DB error. Will need to be fixed if so.
      apiError $ requestFailed "Root usergroups cannot be deleted."
    dbUpdate $ UserGroupDelete ugid
    return . Ok . J.runJSONGen $ do
      J.value "ugid" $ show ugid
      J.value "resource" ("usergroup" :: String)
      J.value "action" ("deleted" :: String)
        where
          isRootUserGroup = isNothing . _ugParentGroupID

userGroupApiAddressV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiAddressV2Get ugid = api $
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    case get ugAddress ug of
      Nothing -> apiError $
        resourceNotFound "This usergroup does not have an address object set."
      Just ugAddr -> Ok <$> return (unjsonUserGroupAddress, ugAddr)

userGroupApiAddressV2Update :: Kontrakcja m => UserGroupID -> m Response
userGroupApiAddressV2Update ugid = api $ do
  ugAddr <- apiV2ParameterObligatory $ ApiV2ParameterJSON "address" unjsonUserGroupAddress
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    let ugOut = ug { _ugAddress = Just ugAddr }
    dbUpdate $ UserGroupUpdate ugOut
    case get ugAddress ugOut of
      Nothing -> apiError $
        serverError "This should not happen, but there is no user group address."
      Just ugAddrOut -> Ok <$> return (unjsonUserGroupAddress, ugAddrOut)

userGroupApiAddressV2Delete :: Kontrakcja m => UserGroupID -> m Response
userGroupApiAddressV2Delete ugid = api $
  -- Check user has permissions to update UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    when (isNothing $ _ugParentGroupID ug) $
      apiError $ requestFailed "A root usergroup must have an address object."
    dbUpdate $ UserGroupUpdate ug { _ugAddress = Nothing }
    return . Ok . J.runJSONGen $ do
      J.value "ugid" $ show ugid
      J.value "resource" ("address" :: String)
      J.value "action" ("deleted" :: String)

userGroupApiSettingsV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiSettingsV2Get ugid = api $ do
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    case get ugSettings ug of
      Nothing -> apiError $
        resourceNotFound "This usergroup does not have a settings object set."
      Just ugSett -> Ok <$> return (unjsonUserGroupSettings, ugSett)

userGroupApiSettingsV2Update :: Kontrakcja m => UserGroupID -> m Response
userGroupApiSettingsV2Update ugid = api $ do
  ugSett <- apiV2ParameterObligatory
    $ ApiV2ParameterJSON "settings" unjsonUserGroupSettings
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    let ugOut = ug { _ugSettings = Just ugSett }
    dbUpdate $ UserGroupUpdate ugOut
    case get ugSettings ugOut of
      Nothing -> apiError $
        serverError "This should not happen, but there are no user group settings."
      Just ugSettOut -> Ok <$> return (unjsonUserGroupSettings, ugSettOut)

userGroupApiSettingsV2Delete :: Kontrakcja m => UserGroupID -> m Response
userGroupApiSettingsV2Delete ugid = api $ do
  -- Check user has permissions to update UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    when (isNothing $ _ugParentGroupID ug) $
      apiError $ requestFailed "A root usergroup must have a settings object."
    dbUpdate $ UserGroupUpdate ug { _ugSettings = Nothing }
    return . Ok . J.runJSONGen $ do
      J.value "ugid" $ show ugid
      J.value "resource" ("settings" :: String)
      J.value "action" ("deleted" :: String)

userGroupApiUIV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiUIV2Get ugid = api $
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    Ok <$> return (unjsonUserGroupUI, get ugUI ug)

userGroupApiUIV2Update :: Kontrakcja m => UserGroupID -> m Response
userGroupApiUIV2Update ugid = api $ do
  ugui <- apiV2ParameterObligatory $ ApiV2ParameterJSON "ui" unjsonUserGroupUI
  -- Check user has permissions to update UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    let ugOut = ug { _ugUI = ugui }
    dbUpdate $ UserGroupUpdate ugOut
    Ok <$> return (unjsonUserGroupUI, get ugUI ugOut)

userGroupApiUsersV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiUsersV2Get ugid = api $ do
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    users <- dbQuery (UserGroupGetUsers ugid)
    Ok <$> return (arrayOf unjsonUser, users)
