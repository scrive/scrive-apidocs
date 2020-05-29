module UserGroup.Model (
    GetUserGroupFirstTOSDate(..)
  , UserGroupCreate(..)
  , UserGroupDelete(..)
  , UserGroupGet(..)
  , UserGroupGetByUserID(..)
  , UserGroupGetWithParents(..)
  , UserGroupGetWithParentsByUG(..)
  , UserGroupGetWithParentsByUserID(..)
  , UserGroupGetBySSOIDPID(..)
  , UserGroupsGetFiltered(..)
  , FindOldUserGroups(..)
  , UserGroupUpdate(..)
  , UserGroupUpdateSettings(..)
  , UserGroupUpdateAddress(..)
  , UserGroupUpdateUI(..)
  , UserGroupGetImmediateChildren(..)
  , UserGroupGetAllChildrenRecursive(..)
  , UserGroupsFormCycle(..)
  , UserGroupIsInvalidAsRoot(..)
  , UserGroupFilter(..)
  , ThemesNotOwnedByUserGroup(..)
  , ugGetChildrenInheritingProperty
  , minUserGroupIdleDocTimeout
  , maxUserGroupIdleDocTimeout
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Typeable
import Log
import Text.JSON.Gen
import qualified Data.Set as S
import qualified Data.Text as T

import DB
import FeatureFlags.Model
import FeatureFlags.Tables
import Tag (TagDomain(..))
import Tag.Tables
import Theme.Model
import User.UserID
import UserGroup.Tables
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import qualified UserGroup.Internal as I

newtype UserGroupCreate = UserGroupCreate UserGroup
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupCreate UserGroup where
  dbUpdate (UserGroupCreate ug) = do
    guardIfHasNoParentThenIsValidRoot ug
    new_parentpath <- case ug ^. #parentGroupID of
      Nothing       -> return $ Array1 ([] :: [UserGroupID])
      Just parentid -> do
        runQuery_ . sqlSelect "user_groups" $ do
          sqlResult "parent_group_path"
          sqlWhereEq "id" . Just $ parentid
        Array1 parentpath <- fetchOne runIdentity
        return . Array1 $ parentid : parentpath
    -- insert user group
    runQuery_ . sqlInsert "user_groups" $ do
      sqlSet "parent_group_id" $ ug ^. #parentGroupID
      sqlSet "parent_group_path" new_parentpath
      sqlSet "name" $ ug ^. #name
      sqlSet "home_folder_id" $ ug ^. #homeFolderID
      sqlResult "id"
    ugid <- fetchOne runIdentity
    -- insert group info
    whenJust (ug ^. #settings) $ insertUserGroupSettings ugid
    -- insert group address
    whenJust (ug ^. #address) $ insertUserGroupAddress ugid
    insertUserGroupTags ugid Tag.Internal (ug ^. #internalTags)
    insertUserGroupTags ugid Tag.External (ug ^. #externalTags)
    -- insert invoicing
    runQuery_ . sqlInsert "user_group_invoicings" $ do
      sqlSet "user_group_id" ugid
      sqlSet "invoicing_type" . ugInvoicingType $ ug
      sqlSet "payment_plan" . ugPaymentPlan $ ug

    -- Insert UI. We are not setting themes here, because a newly created
    -- UserGroup doesn't _own_ any themes yet (they have to be created after the
    -- user group has been created).
    whenJust (ug ^. #ui) $ \ui ->
      insertUserGroupUI ugid
        $ ui
        & (#mailTheme .~ Nothing)
        & (#signviewTheme .~ Nothing)
        & (#serviceTheme .~ Nothing)

    -- insert Features
    whenJust (ug ^. #features) $ insertFeatures ugid

    return . set #id ugid $ ug

newtype UserGroupDelete = UserGroupDelete UserGroupID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m UserGroupDelete () where
  dbUpdate (UserGroupDelete ugid) = do
    now <- currentTime
    void . runQuery . sqlUpdate "user_groups" $ do
      sqlSet "deleted" now
      sqlWhereEq "id" $ Just ugid

insertUserGroupTags :: MonadDB m => UserGroupID -> TagDomain -> S.Set Tag -> m ()
insertUserGroupTags ugid domain tags
  | S.null tags = return ()
  | otherwise = do
    let tags_list = S.toList tags
    runQuery_ . sqlInsert "user_group_tags" $ do
      sqlSet "user_group_id" ugid
      sqlSetList "name" $ view #name <$> tags_list
      sqlSetList "value" $ view #value <$> tags_list
      sqlSet "internal" (domain == Tag.Internal)

insertUserGroupSettings
  :: (MonadDB m, MonadThrow m) => UserGroupID -> UserGroupSettings -> m ()
insertUserGroupSettings ugid ugs = runQuery_ . sqlInsert "user_group_settings" $ do
  sqlSet "user_group_id" ugid
  sqlSet "ip_address_mask_list" $ case ugs ^. #ipAddressMaskList of
    [] -> Nothing
    x  -> Just (show x)
  let drp = ugs ^. #dataRetentionPolicy
  sqlSet "idle_doc_timeout_preparation" $ drp ^. #idleDocTimeoutPreparation
  sqlSet "idle_doc_timeout_closed" $ drp ^. #idleDocTimeoutClosed
  sqlSet "idle_doc_timeout_canceled" $ drp ^. #idleDocTimeoutCanceled
  sqlSet "idle_doc_timeout_timedout" $ drp ^. #idleDocTimeoutTimedout
  sqlSet "idle_doc_timeout_rejected" $ drp ^. #idleDocTimeoutRejected
  sqlSet "idle_doc_timeout_error" $ drp ^. #idleDocTimeoutError
  sqlSet "immediate_trash" $ drp ^. #immediateTrash
  sqlSet "cgi_display_name" $ ugs ^. #cgiDisplayName
  sqlSet "cgi_service_id" $ ugs ^. #cgiServiceID
  sqlSet "sms_provider" $ ugs ^. #smsProvider
  sqlSet "pad_app_mode" $ ugs ^. #padAppMode
  sqlSet "pad_earchive_enabled" $ ugs ^. #padEarchiveEnabled
  sqlSet "force_hide_pn" $ ugs ^. #forceHidePN
  sqlSet "legal_text" $ ugs ^. #legalText
  sqlSet "require_bpid_for_new_document" $ ugs ^. #requireBPIDForNewDoc
  sqlSet "send_timeout_notification" $ ugs ^. #sendTimeoutNotification
  sqlSet "use_folder_list_calls" $ ugs ^. #useFolderListCalls
  sqlSet "totp_is_mandatory" $ ugs ^. #totpIsMandatory
  sqlSet "session_timeout" $ ugs ^. #sessionTimeoutSecs
  sqlSet "portal_url" $ ugs ^. #portalUrl
  sqlSet "eid_service_token" $ ugs ^. #eidServiceToken
  sqlSet "sealing_method" $ ugs ^. #sealingMethod
  sqlSet "document_session_timeout" $ ugs ^. #documentSessionTimeoutSecs
  sqlSet "has_post_signview" $ ugs ^. #hasPostSignview
  sqlSet "sso_config" $ ugs ^. #ssoConfig

insertUserGroupAddress
  :: (MonadDB m, MonadThrow m) => UserGroupID -> UserGroupAddress -> m ()
insertUserGroupAddress ugid uga = runQuery_ . sqlInsert "user_group_addresses" $ do
  sqlSet "user_group_id" ugid
  sqlSet "company_number" $ uga ^. #companyNumber
  sqlSet "entity_name" $ uga ^. #entityName
  sqlSet "address" $ uga ^. #address
  sqlSet "zip" $ uga ^. #zipCode
  sqlSet "city" $ uga ^. #city
  sqlSet "country" $ uga ^. #country

insertUserGroupUI :: (MonadDB m, MonadThrow m) => UserGroupID -> UserGroupUI -> m ()
insertUserGroupUI ugid ui = do
  runQuery_ . sqlInsert "user_group_uis" $ do
    sqlSet "user_group_id" ugid
    sqlSet "browser_title" $ ui ^. #browserTitle
    sqlSet "sms_originator" $ ui ^. #smsOriginator
    sqlSet "favicon" $ ui ^. #favicon

  -- make sure we own the themes we are about to reference
  ownedthemes <- dbQuery $ GetThemesForUserGroup ugid
  let ownedids = map themeID ownedthemes
  let themeids = catMaybes [ui ^. #mailTheme, ui ^. #signviewTheme, ui ^. #serviceTheme]
  if all (`elem` ownedids) themeids
    then runQuery_ . sqlUpdate "user_group_uis" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "mail_theme" $ ui ^. #mailTheme
      sqlSet "signview_theme" $ ui ^. #signviewTheme
      sqlSet "service_theme" $ ui ^. #serviceTheme
    else throwM . SomeDBExtraException . ThemesNotOwnedByUserGroup ugid $ filter
      (not . (`elem` ownedids))
      themeids

data ThemesNotOwnedByUserGroup = ThemesNotOwnedByUserGroup UserGroupID [ThemeID]
  deriving (Eq, Show, Typeable)

instance ToJSValue ThemesNotOwnedByUserGroup where
  toJSValue (ThemesNotOwnedByUserGroup ugid themeids) = runJSONGen $ do
    value "message" ("Themes not owned by user group" :: String)
    value "user_group_id" $ show ugid
    value "offending_theme_ids" $ show themeids

instance DBExtraException ThemesNotOwnedByUserGroup

insertFeatures :: (MonadDB m, MonadThrow m) => UserGroupID -> Features -> m ()
insertFeatures ugid features = do
  runQuery_ . sqlInsert "feature_flags" $ do
    setFeatureFlagsSql $ fRegularUsers features
    sqlSet "user_group_id"   ugid
    sqlSet "flags_for_admin" False
  runQuery_ . sqlInsert "feature_flags" $ do
    setFeatureFlagsSql $ fAdminUsers features
    sqlSet "user_group_id"   ugid
    sqlSet "flags_for_admin" True

newtype UserGroupGet = UserGroupGet UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGet (Maybe UserGroup) where
  dbQuery (UserGroupGet ugid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "id" ugid
      sqlWhereIsNULL "deleted"
    fetchMaybe fetchUserGroup

newtype UserGroupGetByUserID = UserGroupGetByUserID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetByUserID UserGroup where
  dbQuery (UserGroupGetByUserID uid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      sqlJoinOn "users" "users.user_group_id = user_groups.id"
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "users.id" uid
      sqlWhereIsNULL "user_groups.deleted"
    fetchOne fetchUserGroup

newtype GetUserGroupFirstTOSDate = GetUserGroupFirstTOSDate UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserGroupFirstTOSDate UTCTime where
  dbQuery (GetUserGroupFirstTOSDate ugid) = do
    runQuery_ . sqlSelect "users" $ do
      sqlWhereEq "user_group_id" ugid
      sqlResult "min(has_accepted_terms_of_service)"
    fetchOne runIdentity

newtype UserGroupGetImmediateChildren = UserGroupGetImmediateChildren UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetImmediateChildren [UserGroup] where
  dbQuery (UserGroupGetImmediateChildren ugid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "parent_group_id" ugid
      sqlWhereIsNULL "deleted"
    fetchMany fetchUserGroup

newtype UserGroupGetWithParents = UserGroupGetWithParents UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetWithParents (Maybe UserGroupWithParents) where
  dbQuery (UserGroupGetWithParents ugid) = do
    mug <- dbQuery . UserGroupGet $ ugid
    case mug of
      Nothing -> return Nothing
      Just ug -> Just <$> dbQuery (UserGroupGetWithParentsByUG ug)

newtype UserGroupGetWithParentsByUG = UserGroupGetWithParentsByUG UserGroup
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetWithParentsByUG UserGroupWithParents where
  dbQuery (UserGroupGetWithParentsByUG ug) = do
    let ugid = ug ^. #id
    parents <- do
      -- JOIN does not guarantee to preserve order of rows, so we add ORDINALITY and ORDER BY it.
      -- WITH ORDINALITY can only appear inside FROM clause after a function call.
      runQuery_ . sqlSelect "user_groups" $ do
        sqlWith "parentids"
          . sqlSelect "user_groups, unnest(parent_group_path) WITH ORDINALITY"
          $ do
              sqlResult "unnest as id"
              sqlResult "ordinality"
              sqlWhereEq "id" ugid
        sqlJoinOn "parentids" "parentids.id = user_groups.id"
        mapM_ sqlResult userGroupSelectors
        sqlWhereIsNULL "user_groups.deleted"
        sqlOrderBy "ordinality"
      fetchMany fetchUserGroup
    let (ug_root0, ug_children_path) = case reverse parents of
          []                  -> (ug, [])
          (ugr : ug_rev_path) -> (ugr, ug : reverse ug_rev_path)
    case ugrFromUG ug_root0 of
      Nothing      -> throwM . SomeDBExtraException . UserGroupIsInvalidAsRoot $ ug_root0
      Just ug_root -> return (ug_root, ug_children_path)

newtype UserGroupGetWithParentsByUserID = UserGroupGetWithParentsByUserID UserID
instance (MonadDB m, MonadThrow m)
  => DBQuery m UserGroupGetWithParentsByUserID UserGroupWithParents where
  dbQuery (UserGroupGetWithParentsByUserID uid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      sqlJoinOn "users" "users.user_group_id = user_groups.id"
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "users.id" uid
      sqlWhereIsNULL "user_groups.deleted"
    ug <- fetchOne fetchUserGroup
    dbQuery . UserGroupGetWithParentsByUG $ ug

newtype UserGroupGetBySSOIDPID = UserGroupGetBySSOIDPID Text
instance (MonadDB m, MonadThrow m) =>
  DBQuery m UserGroupGetBySSOIDPID (Maybe UserGroup) where
  dbQuery (UserGroupGetBySSOIDPID idpID) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlJoinOn "user_group_settings as ugs" "user_groups.id = ugs.user_group_id"
      sqlWhereEq "ugs.sso_config->>'idp_id'" idpID
      sqlWhereIsNULL "user_groups.deleted"
    fetchMaybe fetchUserGroup

newtype FindOldUserGroups = FindOldUserGroups Int
instance (MonadDB m, MonadTime m) => DBQuery m FindOldUserGroups [UserGroup] where
  dbQuery (FindOldUserGroups batchLimit) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereIsNULL "user_groups.deleted"
      sqlWhereIsNULL "user_groups.parent_group_id"
      sqlWhereNotExists . sqlSelect "users u" $ do
        sqlResult "1"
        sqlWhere "u.user_group_id=user_groups.id"
        sqlWhereIsNULL "u.deleted"
      sqlWhereNotExists . sqlSelect "user_groups ug1" $ do
        sqlResult "1"
        sqlWhere "ug1.parent_group_id=user_groups.id"
        sqlWhereIsNULL "ug1.deleted"
      sqlLimit batchLimit
    fetchMany fetchUserGroup

newtype UserGroupUpdate = UserGroupUpdate UserGroup
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m UserGroupUpdate () where
  dbUpdate (UserGroupUpdate new_ug) = do
    guardIfHasNoParentThenIsValidRoot new_ug
    let ugid = new_ug ^. #id
    -- update group settings
    dbUpdate . UserGroupUpdateSettings ugid $ new_ug ^. #settings
    -- update group address
    dbUpdate . UserGroupUpdateAddress ugid $ new_ug ^. #address
    -- update group tags
    runQuery_ . sqlDelete "user_group_tags" $ sqlWhereEq "user_group_id" ugid
    insertUserGroupTags ugid Tag.Internal $ new_ug ^. #internalTags
    insertUserGroupTags ugid Tag.External $ new_ug ^. #externalTags
    -- update invoicing
    runQuery_ . sqlUpdate "user_group_invoicings" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "invoicing_type" . ugInvoicingType $ new_ug
      sqlSet "payment_plan" . ugPaymentPlan $ new_ug
    -- update UI
    runQuery_ . sqlDelete "user_group_uis" $ do
      sqlWhereEq "user_group_id" ugid
    whenJust (new_ug ^. #ui) $ insertUserGroupUI ugid

    -- update feature flags
    runQuery_ . sqlDelete "feature_flags" $ do
      sqlWhereEq "user_group_id" ugid
    whenJust (new_ug ^. #features) $ insertFeatures ugid

    -- updated group may have children already, these need to be adjusted
    Array1 (old_parentpath :: [UserGroupID]) <- do
      runQuery_ . sqlSelect "user_groups" $ do
        sqlResult "parent_group_path"
        sqlWhereEq "id" ugid
      fetchOne runIdentity
    (Array1 new_parentpath :: Array1 UserGroupID) <- case new_ug ^. #parentGroupID of
      Nothing       -> return $ Array1 ([] :: [UserGroupID])
      Just parentid -> do
        runQuery_ . sqlSelect "user_groups" $ do
          sqlResult "parent_group_path"
          sqlWhereEq "id" . Just $ parentid
        Array1 parentpath <- fetchOne runIdentity
        return . Array1 $ parentid : parentpath
    -- verify, that groups will not form a cycle
    when (ugid `elem` new_parentpath)
      . throwM
      . SomeDBExtraException
      . UserGroupsFormCycle
      $ ugid
    -- update user group
    runQuery_ . sqlUpdate "user_groups" $ do
      sqlSet "parent_group_id" $ new_ug ^. #parentGroupID
      sqlSet "parent_group_path" . Array1 $ new_parentpath
      sqlSet "name" $ new_ug ^. #name
      sqlSet "home_folder_id" $ new_ug ^. #homeFolderID
      sqlWhereEq "id" ugid
    -- update all child groups parentpaths
    runQuery_ . sqlUpdate "user_groups" $ do
      -- to remove multiple items at once from ARRAY, there is only slicing available
      -- inside slicing, we must specify index of the last item
      -- we cut old items from start and then prepend the new parent path
      sqlSetCmd "parent_group_path"
        $   "array_cat(parent_group_path[ 1"
        <>  ": ( array_length(parent_group_path, 1)"
        <>  "- "
        <?> length old_parentpath
        <+> ")]"
        <>  ","
        <?> Array1 new_parentpath
        <+> ")"
      sqlWhere $ "parent_group_path @> " <?> Array1 [ugid]

data UserGroupUpdateSettings = UserGroupUpdateSettings UserGroupID (Maybe UserGroupSettings)
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m UserGroupUpdateSettings () where
  dbUpdate (UserGroupUpdateSettings ugid mugSettings) = do
    runQuery_ . sqlDelete "user_group_settings" $ sqlWhereEq "user_group_id" ugid
    whenJust mugSettings $ insertUserGroupSettings ugid

data UserGroupUpdateAddress = UserGroupUpdateAddress UserGroupID (Maybe UserGroupAddress)
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m UserGroupUpdateAddress () where
  dbUpdate (UserGroupUpdateAddress ugid mugAddr) = do
    runQuery_ . sqlDelete "user_group_addresses" $ sqlWhereEq "user_group_id" ugid
    whenJust mugAddr $ insertUserGroupAddress ugid

data UserGroupUpdateUI = UserGroupUpdateUI UserGroupID (Maybe UserGroupUI)
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m UserGroupUpdateUI () where
  dbUpdate (UserGroupUpdateUI ugid mugUI) = do
    runQuery_ . sqlDelete "user_group_uis" $ sqlWhereEq "user_group_id" ugid
    whenJust mugUI $ insertUserGroupUI ugid

newtype UserGroupsFormCycle = UserGroupsFormCycle UserGroupID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue UserGroupsFormCycle where
  toJSValue (UserGroupsFormCycle ugid) = runJSONGen $ do
    value "message"       ("User Groups Form Cycle" :: String)
    value "user_group_id" (show ugid)

instance DBExtraException UserGroupsFormCycle

guardIfHasNoParentThenIsValidRoot :: (MonadDB m, MonadThrow m) => UserGroup -> m ()
guardIfHasNoParentThenIsValidRoot ug = case ug ^. #parentGroupID of
  Just _  -> return ()
  Nothing -> case ugrFromUG ug of
    Just _  -> return ()
    Nothing -> throwM . SomeDBExtraException . UserGroupIsInvalidAsRoot $ ug

newtype UserGroupIsInvalidAsRoot = UserGroupIsInvalidAsRoot UserGroup
  deriving (Eq, Show, Typeable)

instance ToJSValue UserGroupIsInvalidAsRoot where
  toJSValue (UserGroupIsInvalidAsRoot ug) = runJSONGen $ do
    value "message"    ("User Group is invalid as root" :: String)
    value "user_group" (show ug)

instance DBExtraException UserGroupIsInvalidAsRoot

userGroupSelectors :: [SQL]
userGroupSelectors =
  [ "user_groups.id"
  , "user_groups.parent_group_id"
  , "user_groups.name"
  , "user_groups.home_folder_id"
  , "(SELECT ("
    <>  mintercalate ", " ugInvoicingSelectors
    <>  ")::"
    <>  raw (ctName ctUserGroupInvoicing)
    <+> "FROM user_group_invoicings WHERE user_groups.id = user_group_invoicings.user_group_id)"
  , "(SELECT ("
    <>  mintercalate ", " ugSettingsSelectors
    <>  ")::"
    <>  raw (ctName ctUserGroupSettings)
    <+> "FROM user_group_settings WHERE user_groups.id = user_group_settings.user_group_id)"
  , "(SELECT ("
    <>  mintercalate ", " ugAddressSelectors
    <>  ")::"
    <>  raw (ctName ctUserGroupAddress)
    <+> "FROM user_group_addresses WHERE user_groups.id = user_group_addresses.user_group_id)"
  , "(SELECT ("
    <>  mintercalate ", " ugUISelectors
    <>  ")::"
    <>  raw (ctName ctUserGroupUI)
    <+> "FROM user_group_uis WHERE user_groups.id = user_group_uis.user_group_id)"
  , "(SELECT ("
    <>  mintercalate ", " selectFeatureFlagsSelectors
    <>  ")::"
    <>  raw (ctName ctFeatureFlags)
    <+> "FROM feature_flags WHERE user_groups.id = feature_flags.user_group_id AND feature_flags.flags_for_admin)"
  , "(SELECT ("
    <>  mintercalate ", " selectFeatureFlagsSelectors
    <>  ")::"
    <>  raw (ctName ctFeatureFlags)
    <+> "FROM feature_flags WHERE user_groups.id = feature_flags.user_group_id AND NOT feature_flags.flags_for_admin)"
  , "ARRAY(SELECT ("
    <>  mintercalate ", " ugTagSelectors
    <>  ")::"
    <>  raw (ctName ctTag)
    <+> "FROM user_group_tags ugt WHERE user_groups.id = ugt.user_group_id AND ugt.internal ORDER BY ugt.name)"
  , "ARRAY(SELECT ("
    <>  mintercalate ", " ugTagSelectors
    <>  ")::"
    <>  raw (ctName ctTag)
    <+> "FROM user_group_tags ugt WHERE user_groups.id = ugt.user_group_id AND NOT ugt.internal ORDER BY ugt.name)"
  ]

ugSettingsSelectors :: [SQL]
ugSettingsSelectors =
  [ "ip_address_mask_list"
  , "idle_doc_timeout_preparation"
  , "idle_doc_timeout_closed"
  , "idle_doc_timeout_canceled"
  , "idle_doc_timeout_timedout"
  , "idle_doc_timeout_rejected"
  , "idle_doc_timeout_error"
  , "immediate_trash"
  , "cgi_display_name"
  , "sms_provider"
  , "cgi_service_id"
  , "pad_app_mode"
  , "pad_earchive_enabled"
  , "legal_text"
  , "require_bpid_for_new_document"
  , "send_timeout_notification"
  , "use_folder_list_calls"
  , "totp_is_mandatory"
  , "session_timeout"
  , "portal_url"
  , "eid_service_token"
  , "sealing_method"
  , "document_session_timeout"
  , "force_hide_pn"
  , "has_post_signview"
  , "sso_config"
  ]


ugAddressSelectors :: [SQL]
ugAddressSelectors =
  ["company_number", "entity_name", "address", "zip", "city", "country"]

ugUISelectors :: [SQL]
ugUISelectors =
  [ "mail_theme"
  , "signview_theme"
  , "service_theme"
  , "browser_title"
  , "sms_originator"
  , "favicon"
  ]

ugInvoicingSelectors :: [SQL]
ugInvoicingSelectors = ["invoicing_type", "payment_plan"]

ugTagSelectors :: [SQL]
ugTagSelectors = ["name", "value"]

data UserGroupFilter
  = UGFilterByString Text    -- ^ Contains the string anywhere
  | UGManyUsers                -- ^ Has non-trivial amount of users (includes invited users)
  | UGWithNonFreePricePlan     -- ^ Has a non-free price plan attached
  | UGWithAnyIdleDocTimeoutSet -- ^ Has any ugIdleDocTimeout{STATUS} set

data UserGroupsGetFiltered = UserGroupsGetFiltered [UserGroupFilter] (Maybe (Integer, Integer))
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupsGetFiltered [UserGroup] where
  dbQuery (UserGroupsGetFiltered filters moffsetlimit) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      forM_ filters $ \case
        UGFilterByString text -> do
          sqlJoinOn "user_group_addresses"
                    "user_group_addresses.user_group_id = user_groups.id"
          mapM_ (sqlWhere . parenthesize . findWord) (words $ T.unpack text)
        UGManyUsers -> sqlWhereAny
          [ sqlWhere
            "((SELECT count(*) FROM users WHERE users.user_group_id = user_groups.id AND users.deleted IS NULL) > 1)"
          , sqlWhere
            "((SELECT count(*) FROM companyinvites WHERE companyinvites.user_group_id = user_groups.id) > 0)"
          ]
        UGWithNonFreePricePlan -> do
          sqlJoinOn "user_group_invoicings"
                    "user_group_invoicings.user_group_id = user_groups.id"
          sqlWhere $ "(user_group_invoicings.payment_plan != " <?> FreePlan <> ")"
        UGWithAnyIdleDocTimeoutSet -> do
          sqlJoinOn "user_group_settings"
                    "user_group_settings.user_group_id = user_groups.id"
          sqlWhere
            "(user_group_settings.idle_doc_timeout_preparation IS NOT NULL\
             \ OR user_group_settings.idle_doc_timeout_closed   IS NOT NULL\
             \ OR user_group_settings.idle_doc_timeout_canceled IS NOT NULL\
             \ OR user_group_settings.idle_doc_timeout_timedout IS NOT NULL\
             \ OR user_group_settings.idle_doc_timeout_rejected IS NOT NULL\
             \ OR user_group_settings.idle_doc_timeout_error    IS NOT NULL)"
      whenJust moffsetlimit $ \(offset, limit) -> do
        sqlOffset offset
        sqlLimit limit
      sqlWhereIsNULL "user_groups.deleted"
      sqlOrderBy "user_groups.id"
    fetchMany fetchUserGroup
    where
      findWordInField word fieldName =
        ("user_group_addresses." <+> fieldName) <+> "ILIKE" <?> sqlwordpat word
      findWordInName word = "user_groups.name" <+> "ILIKE" <?> sqlwordpat word
      findWordList word = map
        (findWordInField word)
        ["company_number", "entity_name", "address", "zip", "city", "country"]
      findWord word = sqlConcatOR $ findWordInName word : findWordList word
      sqlwordpat word = "%" ++ concatMap escape word ++ "%"
      escape '\\' = "\\\\"
      escape '%'  = "\\%"
      escape '_'  = "\\_"
      escape c    = [c]


-- Get recursively all children, which inherit a property of this group.
-- UserGroup inherits property, when the property is Nothing.
ugGetChildrenInheritingProperty
  :: (MonadDB m, MonadThrow m) => UserGroupID -> (UserGroup -> Maybe a) -> m [UserGroup]
ugGetChildrenInheritingProperty ugid ugProperty = do
  children <- dbQuery . UserGroupGetImmediateChildren $ ugid
  let inheriting_children = filter (isNothing . ugProperty) children
  grandchildren <- fmap concat . forM inheriting_children $ \c ->
    ugGetChildrenInheritingProperty (c ^. #id) ugProperty
  return $ inheriting_children ++ grandchildren

-- Get all children recursively
newtype UserGroupGetAllChildrenRecursive = UserGroupGetAllChildrenRecursive UserGroupID
instance (MonadDB m, MonadThrow m)
  => DBQuery m UserGroupGetAllChildrenRecursive [UserGroupWithChildren] where
  dbQuery (UserGroupGetAllChildrenRecursive ugid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhere $ "parent_group_path @> " <?> Array1 [ugid]
    allChildren <- fetchMany fetchUserGroup
    let directChildren parentID =
          filter ((== Just parentID) . view #parentGroupID) allChildren
        mkChildren parentID = mkChild <$> directChildren parentID
        mkChild ug = I.UserGroupWithChildren ug . mkChildren $ ug ^. #id
    return $ mkChildren ugid

-- Synchronize these definitions with frontend/app/js/account/company.js
minUserGroupIdleDocTimeout :: Int16
minUserGroupIdleDocTimeout = 1
maxUserGroupIdleDocTimeout :: Int16
maxUserGroupIdleDocTimeout = 365
