module UserGroup.API (
    userGroupAPI
  , userGroupApiV2Create
  , userGroupApiV2Get
  , userGroupApiV2Update
  , userGroupApiV2Delete
  , userGroupApiContactDetailsV2Get
  , userGroupApiContactDetailsV2Update
  , userGroupApiContactDetailsV2Delete
  , userGroupApiSettingsV2Get
  , userGroupApiSettingsV2Update
  , userGroupApiSettingsV2Delete
  , userGroupApiUsersV2Get
) where

import Control.Monad.Extra (unlessM)
import Data.Unjson
import Happstack.Server.Types
import Happstack.StaticRouting
import qualified Text.JSON.Gen as J

import AccessControl.Model
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
  , param . dir "contact_details" . hGet . toK1 $ userGroupApiContactDetailsV2Get
  , param . dir "contact_details" . dir "update" . hPost . toK1 $
      userGroupApiContactDetailsV2Update
  , param . dir "contact_details" . dir "delete" . hPost . toK1 $
      userGroupApiContactDetailsV2Delete
  , param . dir "settings" . hGet . toK1 $ userGroupApiSettingsV2Get
  , param . dir "settings" . dir "update" . hPost . toK1 $ userGroupApiSettingsV2Update
  , param . dir "settings" . dir "delete" . hPost . toK1 $ userGroupApiSettingsV2Delete
  , param . dir "users" . hGet . toK1 $ userGroupApiUsersV2Get
  ]

userGroupApiV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiV2Get ugid = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    dbQuery (UserGroupGet ugid) >>= \case
      Nothing ->
        apiError $ serverError "Impossible happened: No user group with ID, or deleted."
      Just ug -> Ok <$> constructUserGroupResponse ug inheritable

userGroupApiV2Create :: Kontrakcja m => m Response
userGroupApiV2Create = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  ugReq  <- apiV2ParameterObligatory $ ApiV2ParameterAeson "usergroup"
  ugIn <- case updateUserGroupFromRequest defaultChildUserGroup ugReq of
    Nothing -> apiError $ requestFailed "Error parsing user group create object."
    Just ugUpdated -> return ugUpdated
  ugOut <- case get ugParentGroupID ugIn of
    Nothing -> do
      -- Guard against non-Admins being able to create root UserGroups
      unlessM checkAdminOrSales $ apiError insufficientPrivileges
      dbUpdate $ UserGroupCreate defaultUserGroup {
          _ugName = get ugName ugIn
          -- _externalIDs = ....      -- TODO: Implement external_ids
        }
    Just parent_ugid -> do
      -- Check user has permissions to create child UserGroup
      let acc = mkAccPolicyItem (CreateA, UserGroupR, parent_ugid)
      apiAccessControlOrIsAdmin [acc] . dbUpdate $ UserGroupCreate ugIn
  -- Return response
  Ok <$> constructUserGroupResponse ugOut inheritable

userGroupApiV2Update :: Kontrakcja m => UserGroupID -> m Response
userGroupApiV2Update ugid = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  ugReq <- apiV2ParameterObligatory $ ApiV2ParameterAeson "usergroup"
  ugOriginal <- dbQuery (UserGroupGet ugid) >>= \case
    -- Must do manual existance checking since we haven't done access control checks yet
    Nothing -> apiError insufficientPrivileges
    Just ug -> return ug
  ugNew <- case updateUserGroupFromRequest ugOriginal ugReq of
    Nothing -> apiError $ requestFailed "Error parsing user group update object."
    Just ugUpdated -> return ugUpdated
  let moldparentugid = get ugParentGroupID ugOriginal
  let mnewparentugid = get ugParentGroupID ugNew
  -- Checks to see whether the usergroup is being moved (its parent ugid changed)
  movementAccs <- case (moldparentugid, mnewparentugid) of
    (Just oldparentugid, Just newparentugid) ->
      if oldparentugid /= newparentugid
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
    (Nothing, Just newparentugid) -> do
      -- Root usergroup is being made subordinate to another UserGroup
      let newAcc = mkAccPolicyItem (UpdateA, UserGroupR, newparentugid)
          curAcc = mkAccPolicyItem (DeleteA, UserGroupR, get ugID ugOriginal)
      return [newAcc, curAcc]
    (Just _, Nothing) -> do
      -- Only admin or sales can promote UserGroup to root
      unlessM checkAdminOrSales $ apiError insufficientPrivileges
      return []
    (Nothing, Nothing) ->
      -- Root usergroup is remaining root, no special privileges needed
      return []
  let acc = mkAccPolicyItem (UpdateA, UserGroupR, ugid)
  apiAccessControlOrIsAdmin (acc:movementAccs) $ do
    dbUpdate $ UserGroupUpdate ugNew
    dbQuery (UserGroupGet ugid) >>= \case
      Nothing ->
        apiError $ serverError "The UserGroup which was updated no longer exists."
      Just ug' -> Ok <$> constructUserGroupResponse ug' inheritable

userGroupApiV2Delete :: Kontrakcja m => UserGroupID -> m Response
userGroupApiV2Delete ugid = api $
  -- Check user has permissions to delete UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (DeleteA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    when (isRootUserGroup ug) $
      -- Maybe Sales/Admin users should be allowed
      -- Note: Current DB implementation causes causes DB error when deleting a root.
      -- Will need to be fixed if we want to allow deletion of roots.
      apiError $ requestFailed "Root usergroups cannot be deleted."
    dbUpdate $ UserGroupDelete ugid
    dbUpdate $ AccessControlDeleteRolesByUserGroup ugid
    return . Ok . J.runJSONGen $ do
      J.value "id" $ show ugid
      J.value "resource" ("usergroup" :: String)
      J.value "action" ("deleted" :: String)
        where
          isRootUserGroup = isNothing . _ugParentGroupID

userGroupApiContactDetailsV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiContactDetailsV2Get ugid = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    (inherited_from, ugAddr, inherit_preview) <- calculateAddressInheritance ug
    if inheritable
    then
      Ok <$> return (
        userGroupContactDetailsWithInheritableToResponse
          inherited_from
          ugAddr
          inherit_preview
        )
    else
      Ok <$> return (userGroupContactDetailsToResponse inherited_from ugAddr)

userGroupApiContactDetailsV2Update :: Kontrakcja m => UserGroupID -> m Response
userGroupApiContactDetailsV2Update ugid = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  contactDetailsChanges <- apiV2ParameterObligatory $ ApiV2ParameterAeson "contact_details"
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    -- New address creation DOES NOT inherit, since people will want to start
    -- from a blank address.
    let ugAddr = fromMaybe defaultUserGroupAddress $ get ugAddress ug
    ugAddrUpdated <- case updateUserGroupContactDetailsFromRequest ugAddr contactDetailsChanges of
      Nothing -> apiError $ requestFailed "Error parsing address update object."
      Just ugAddrUpdated -> return ugAddrUpdated
    dbUpdate . UserGroupUpdateAddress ugid $ Just ugAddrUpdated
    dbQuery (UserGroupGet ugid) >>= \case
      Nothing ->
        apiError $ serverError "The UserGroup which was updated no longer exists."
      Just ug' -> do
        (inherited_from, ugAddr', inherit_preview) <- calculateAddressInheritance ug'
        if inheritable
        then
          Ok <$> return (
            userGroupContactDetailsWithInheritableToResponse
              inherited_from
              ugAddr'
              inherit_preview
            )
        else
          Ok <$> return (userGroupContactDetailsToResponse inherited_from ugAddr')

userGroupApiContactDetailsV2Delete :: Kontrakcja m => UserGroupID -> m Response
userGroupApiContactDetailsV2Delete ugid = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  -- Check user has permissions to update UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    when (isNothing $ _ugParentGroupID ug) $
      apiError $ requestFailed "A root usergroup must have an address object."
    dbUpdate $ UserGroupUpdateAddress ugid Nothing
    dbQuery (UserGroupGet ugid) >>= \case
      Nothing ->
        apiError $ serverError "The UserGroup which was updated no longer exists."
      Just ug' -> do
        (inherited_from, ugAddr', inherit_preview) <- calculateAddressInheritance ug'
        if inheritable
        then
          Ok <$> return (
            userGroupContactDetailsWithInheritableToResponse
              inherited_from
              ugAddr'
              inherit_preview
            )
        else
          Ok <$> return (userGroupContactDetailsToResponse inherited_from ugAddr')

userGroupApiSettingsV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiSettingsV2Get ugid = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    (inherited_from, ugSett, inherit_preview) <- calculateSettingsInheritance ug
    if inheritable
    then
      Ok <$> return (
        userGroupSettingsWithInheritableToResponse
          inherited_from
          ugSett
          inherit_preview
        )
    else
      Ok <$> return (userGroupSettingsToResponse inherited_from ugSett)

userGroupApiSettingsV2Update :: Kontrakcja m => UserGroupID -> m Response
userGroupApiSettingsV2Update ugid = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  settingsChanges <- apiV2ParameterObligatory $ ApiV2ParameterAeson "settings"
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    -- New settings creation DOES inherit, since there are things that are
    -- not settable by the API which need to be preserved for children.
    ugSett <- case get ugSettings ug of
      Nothing -> case get ugParentGroupID ug of
        Nothing -> return defaultUserGroupSettings
        Just parent_ugid ->
          dbQuery (UserGroupGetWithParents parent_ugid) >>= \case
            Nothing ->
              unexpectedError "Impossible happened (parent ID does not exist)"
            Just ugwp -> return $ ugwpSettings ugwp
      Just ugSett -> return ugSett
    let dataRetention = get ugsDataRetentionPolicy ugSett
    dataRetentionUpdated <-
      case updateUserGroupDataRetentionFromRequest dataRetention settingsChanges of
        Nothing -> apiError $ requestFailed "Error parsing address update object."
        Just ugSettUpdated -> return ugSettUpdated
    dbUpdate . UserGroupUpdateSettings ugid . Just $ ugSett {
        _ugsDataRetentionPolicy = dataRetentionUpdated
      }
    dbQuery (UserGroupGet ugid) >>= \case
      Nothing ->
        apiError $ serverError "The UserGroup which was updated no longer exists."
      Just ug' -> do
        (inherited_from, ugSett', inherit_preview) <- calculateSettingsInheritance ug'
        if inheritable
        then
          Ok <$> return (
            userGroupSettingsWithInheritableToResponse
              inherited_from
              ugSett'
              inherit_preview
            )
        else
          Ok <$> return (userGroupSettingsToResponse inherited_from ugSett')

userGroupApiSettingsV2Delete :: Kontrakcja m => UserGroupID -> m Response
userGroupApiSettingsV2Delete ugid = api $ do
  inheritable <- apiV2ParameterDefault False $ ApiV2ParameterFlag "include-inheritable"
  -- Check user has permissions to update UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (UpdateA, UserGroupR, ugid)] $ do
    ug <- userGroupOrAPIError ugid
    when (isNothing $ _ugParentGroupID ug) $
      apiError $ requestFailed "A root usergroup must have a settings object."
    dbUpdate $ UserGroupUpdateSettings ugid Nothing
    dbQuery (UserGroupGet ugid) >>= \case
      Nothing ->
        apiError $ serverError "The UserGroup which was updated no longer exists."
      Just ug' -> do
        (inherited_from, ugSett', inherit_preview) <- calculateSettingsInheritance ug'
        if inheritable
        then
          Ok <$> return (
            userGroupSettingsWithInheritableToResponse
              inherited_from
              ugSett'
              inherit_preview
            )
        else
          Ok <$> return (userGroupSettingsToResponse inherited_from ugSett')

userGroupApiUsersV2Get :: Kontrakcja m => UserGroupID -> m Response
userGroupApiUsersV2Get ugid = api $ do
  -- Check user has permissions to view UserGroup
  apiAccessControlOrIsAdmin [mkAccPolicyItem (ReadA, UserGroupR, ugid)] $ do
    users <- dbQuery (UserGroupGetUsers ugid)
    Ok <$> return (arrayOf unjsonUser, users)

constructUserGroupResponse
  :: Kontrakcja m
  => UserGroup
  -> Bool
  -> m (UnjsonDef UserGroupResponseJSON, UserGroupResponseJSON)
constructUserGroupResponse ug showInheritPreview = do
  ugAddr@(mugid_addr, addr, _) <- calculateAddressInheritance ug
  ugSett@(mugid_sett, sett, _) <- calculateSettingsInheritance ug
  let childMap = map (\child -> (get ugID child, get ugName child))
  children <- fmap childMap . dbQuery . UserGroupGetImmediateChildren $ get ugID ug
  if showInheritPreview
  then
    return $ userGroupWithInheritableToResponse ug ugAddr ugSett children
  else
    return $ userGroupToResponse ug (mugid_addr, addr) (mugid_sett, sett) children

calculateAddressInheritance
  :: Kontrakcja m
  => UserGroup
  -> m (Maybe UserGroupID, UserGroupAddress, Maybe (UserGroupID, UserGroupAddress))
calculateAddressInheritance ug = do
  m_inherit_preview <- case get ugParentGroupID ug of
    Nothing -> return Nothing
    Just parent_ugid -> dbQuery (UserGroupGetWithParents parent_ugid) >>= \case
      Nothing -> return Nothing
      Just ugwp -> return . Just $ ugwpAddressWithID ugwp
  case (m_inherit_preview, get ugAddress ug) of
    (Nothing, Nothing) ->
      apiError $ requestFailed "Root User Group has no address object."
    (inherit_preview@(Just (inherited_from, inherited_address)), Nothing) ->
      return (Just inherited_from, inherited_address, inherit_preview)
    (Nothing, Just ugAddr) ->
      return (Nothing, ugAddr, Nothing)
    (Just inherit_preview, Just ugAddr) ->
      return (Nothing, ugAddr, Just inherit_preview)

calculateSettingsInheritance
  :: Kontrakcja m
  => UserGroup
  -> m (Maybe UserGroupID, UserGroupSettings, Maybe (UserGroupID, UserGroupSettings))
calculateSettingsInheritance ug = do
  m_inherit_preview <- case get ugParentGroupID ug of
    Nothing -> return Nothing
    Just parent_ugid -> dbQuery (UserGroupGetWithParents parent_ugid) >>= \case
      Nothing -> return Nothing
      Just ugwp -> return . Just $ ugwpSettingsWithID ugwp
  case (m_inherit_preview, get ugSettings ug) of
    (Nothing, Nothing) ->
      apiError $ requestFailed "Root User Group has no settings object."
    (inherit_preview@(Just (inherited_from, inherited_settings)), Nothing) ->
      return (Just inherited_from, inherited_settings, inherit_preview)
    (Nothing, Just ugSett) ->
      return (Nothing, ugSett, Nothing)
    (Just inherit_preview, Just ugSett) ->
      return (Nothing, ugSett, Just inherit_preview)
