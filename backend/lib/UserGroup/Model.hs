module UserGroup.Model (
    GetUserGroupFirstTOSDate(..)
  , UserGroupCreate(..)
  , UserGroupDelete(..)
  , UserGroupGet(..)
  , UserGroupGetByUserID(..)
  , UserGroupGetWithParents(..)
  , UserGroupGetWithParentsByUserID(..)
  , UserGroupsGetFiltered(..)
  , UserGroupUpdate(..)
  , UserGroupUpdateSettings(..)
  , UserGroupUpdateAddress(..)
  , UserGroupGetImmediateChildren(..)
  , UserGroupGetAllChildrenRecursive(..)
  , UserGroupsFormCycle(..)
  , UserGroupIsInvalidAsRoot(..)
  , UserGroupFilter(..)
  , ugGetChildrenInheritingProperty
  , unsafeUserGroupIDToPartnerID
  , minUserGroupIdleDocTimeout
  , maxUserGroupIdleDocTimeout
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Typeable
import Log
import Text.JSON.Gen
import qualified Data.Text as T

import DataRetentionPolicy
import DB
import FeatureFlags.Model
import FeatureFlags.Tables
import Partner.Model
import User.UserID
import UserGroup.Tables
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import qualified UserGroup.Internal as I

data UserGroupCreate = UserGroupCreate UserGroup
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupCreate UserGroup where
  update (UserGroupCreate ug) = do
    guardIfHasNoParentThenIsValidRoot ug
    new_parentpath <- case ugParentGroupID ug of
      Nothing       -> return . Array1 $ ([] :: [UserGroupID])
      Just parentid -> do
        runQuery_ . sqlSelect "user_groups" $ do
          sqlResult "parent_group_path"
          sqlWhereEq "id" . Just $ parentid
        Array1 parentpath <- fetchOne runIdentity
        return . Array1 . (parentid :) $ parentpath
    -- insert user group
    runQuery_ . sqlInsert "user_groups" $ do
      sqlSet "parent_group_id" $ ugParentGroupID ug
      sqlSet "parent_group_path" $ new_parentpath
      sqlSet "name" $ ugName ug
      sqlSet "home_folder_id" $ ugHomeFolderID ug
      sqlResult "id"
    ugid <- fetchOne runIdentity
    -- insert group info
    whenJust (ugSettings ug) $ insertUserGroupSettings ugid
    -- insert group address
    whenJust (ugAddress ug) $ insertUserGroupAddress ugid
    -- insert invoicing
    runQuery_ . sqlInsert "user_group_invoicings" $ do
      sqlSet "user_group_id" ugid
      sqlSet "invoicing_type" . ugInvoicingType $ ug
      sqlSet "payment_plan" . ugPaymentPlan $ ug
    -- insert UI
    let ugui = ugUI ug
    runQuery_ . sqlInsert "user_group_uis" $ do
      sqlSet "user_group_id" ugid
      -- We are not setting themes here, because UserGroup, which does not
      -- exist yet, cannot own any themes.
      sqlSet "browser_title" $ ugui ^. #uguiBrowserTitle
      sqlSet "sms_originator" $ ugui ^. #uguiSmsOriginator
      sqlSet "favicon" $ ugui ^. #uguiFavicon

    -- insert Features
    whenJust (ugFeatures ug) $ insertFeatures ugid

    return . set #ugID ugid $ ug

data UserGroupDelete = UserGroupDelete UserGroupID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m UserGroupDelete () where
  update (UserGroupDelete ugid) = do
    now <- currentTime
    void . runQuery . sqlUpdate "user_groups" $ do
      sqlSet "deleted" now
      sqlWhereEq "id" $ Just ugid

insertUserGroupSettings
  :: (MonadDB m, MonadThrow m) => UserGroupID -> UserGroupSettings -> m ()
insertUserGroupSettings ugid ugs = runQuery_ . sqlInsert "user_group_settings" $ do
  sqlSet "user_group_id" ugid
  sqlSet "ip_address_mask_list" $ case ugs ^. #ugsIPAddressMaskList of
    [] -> Nothing
    x  -> Just (show x)
  let drp = ugs ^. #ugsDataRetentionPolicy
  sqlSet "idle_doc_timeout_preparation" $ drpIdleDocTimeoutPreparation drp
  sqlSet "idle_doc_timeout_closed" $ drpIdleDocTimeoutClosed drp
  sqlSet "idle_doc_timeout_canceled" $ drpIdleDocTimeoutCanceled drp
  sqlSet "idle_doc_timeout_timedout" $ drpIdleDocTimeoutTimedout drp
  sqlSet "idle_doc_timeout_rejected" $ drpIdleDocTimeoutRejected drp
  sqlSet "idle_doc_timeout_error" $ drpIdleDocTimeoutError drp
  sqlSet "immediate_trash" $ drpImmediateTrash drp
  sqlSet "cgi_display_name" $ ugs ^. #ugsCGIDisplayName
  sqlSet "cgi_service_id" $ ugs ^. #ugsCGIServiceID
  sqlSet "sms_provider" $ ugs ^. #ugsSMSProvider
  sqlSet "pad_app_mode" $ ugs ^. #ugsPadAppMode
  sqlSet "pad_earchive_enabled" $ ugs ^. #ugsPadEarchiveEnabled
  sqlSet "legal_text" $ ugs ^. #ugsLegalText
  sqlSet "require_bpid_for_new_document" $ ugs ^. #ugsRequireBPIDForNewDoc
  sqlSet "send_timeout_notification" $ ugs ^. #ugsSendTimeoutNotification
  sqlSet "totp_is_mandatory" $ ugs ^. #ugsTotpIsMandatory
  sqlSet "session_timeout" $ ugs ^. #ugsSessionTimeoutSecs
  sqlSet "portal_url" $ ugs ^. #ugsPortalUrl

insertUserGroupAddress
  :: (MonadDB m, MonadThrow m) => UserGroupID -> UserGroupAddress -> m ()
insertUserGroupAddress ugid uga = runQuery_ . sqlInsert "user_group_addresses" $ do
  sqlSet "user_group_id" ugid
  sqlSet "company_number" $ uga ^. #ugaCompanyNumber
  sqlSet "entity_name" $ uga ^. #ugaEntityName
  sqlSet "address" $ uga ^. #ugaAddress
  sqlSet "zip" $ uga ^. #ugaZip
  sqlSet "city" $ uga ^. #ugaCity
  sqlSet "country" $ uga ^. #ugaCountry

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

data UserGroupGet = UserGroupGet UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGet (Maybe UserGroup) where
  query (UserGroupGet ugid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "id" ugid
      sqlWhereIsNULL "deleted"
    fetchMaybe fetchUserGroup

data UserGroupGetByUserID = UserGroupGetByUserID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetByUserID UserGroup where
  query (UserGroupGetByUserID uid) = do
    runQuery_ $ sqlSelect "user_groups" $ do
      sqlJoinOn "users" "users.user_group_id = user_groups.id"
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "users.id" uid
      sqlWhereIsNULL "user_groups.deleted"
    fetchOne fetchUserGroup

data GetUserGroupFirstTOSDate = GetUserGroupFirstTOSDate UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserGroupFirstTOSDate UTCTime where
  query (GetUserGroupFirstTOSDate ugid) = do
    runQuery_ $ sqlSelect "users" $ do
      sqlWhereEq "user_group_id" ugid
      sqlResult "min(has_accepted_terms_of_service)"
    fetchOne runIdentity

data UserGroupGetImmediateChildren = UserGroupGetImmediateChildren UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetImmediateChildren [UserGroup] where
  query (UserGroupGetImmediateChildren ugid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "parent_group_id" ugid
      sqlWhereIsNULL "deleted"
    fetchMany fetchUserGroup

data UserGroupGetWithParents = UserGroupGetWithParents UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetWithParents (Maybe UserGroupWithParents) where
  query (UserGroupGetWithParents ugid) = do
    mug <- dbQuery . UserGroupGet $ ugid
    case mug of
      Nothing -> return Nothing
      Just ug -> Just <$> (dbQuery $ UserGroupGetWithParentsByUG ug)

data UserGroupGetWithParentsByUG = UserGroupGetWithParentsByUG UserGroup
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetWithParentsByUG UserGroupWithParents where
  query (UserGroupGetWithParentsByUG ug) = do
    let ugid = ugID ug
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

data UserGroupGetWithParentsByUserID = UserGroupGetWithParentsByUserID UserID
instance (MonadDB m, MonadThrow m)
  => DBQuery m UserGroupGetWithParentsByUserID UserGroupWithParents where
  query (UserGroupGetWithParentsByUserID uid) = do
    runQuery_ $ sqlSelect "user_groups" $ do
      sqlJoinOn "users" "users.user_group_id = user_groups.id"
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "users.id" uid
      sqlWhereIsNULL "user_groups.deleted"
    ug <- fetchOne fetchUserGroup
    dbQuery . UserGroupGetWithParentsByUG $ ug

data UserGroupUpdate = UserGroupUpdate UserGroup
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m UserGroupUpdate () where
  update (UserGroupUpdate new_ug) = do
    guardIfHasNoParentThenIsValidRoot new_ug
    let ugid = ugID new_ug
    -- update group settings
    dbUpdate . UserGroupUpdateSettings ugid $ ugSettings new_ug
    -- update group address
    dbUpdate . UserGroupUpdateAddress ugid $ ugAddress new_ug
    -- update invoicing
    runQuery_ . sqlUpdate "user_group_invoicings" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "invoicing_type" . ugInvoicingType $ new_ug
      sqlSet "payment_plan" . ugPaymentPlan $ new_ug
    -- update UI
    let ugui = ugUI new_ug
        chkUgOwnsTheme thmLbl ugui' ugid' = when (isJust $ ugui' ^. thmLbl) $ do
          sqlWhereExists $ sqlSelect "theme_owners" $ do
            sqlWhereEq "user_group_id" ugid'
            sqlWhereEq "theme_id"      (ugui' ^. thmLbl)

    runQuery_ . sqlUpdate "user_group_uis" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "mail_theme" $ ugui ^. #uguiMailTheme
      sqlSet "signview_theme" $ ugui ^. #uguiSignviewTheme
      sqlSet "service_theme" $ ugui ^. #uguiServiceTheme
      sqlSet "browser_title" $ ugui ^. #uguiBrowserTitle
      sqlSet "sms_originator" $ ugui ^. #uguiSmsOriginator
      sqlSet "favicon" $ ugui ^. #uguiFavicon

      chkUgOwnsTheme #uguiMailTheme     ugui ugid
      chkUgOwnsTheme #uguiSignviewTheme ugui ugid
      chkUgOwnsTheme #uguiServiceTheme  ugui ugid

    -- update feature flags
    runQuery_ . sqlDelete "feature_flags" $ do
      sqlWhereEq "user_group_id" ugid
    whenJust (ugFeatures new_ug) $ insertFeatures ugid

    -- updated group may have children already, these need to be adjusted
    Array1 (old_parentpath :: [UserGroupID]) <- do
      runQuery_ . sqlSelect "user_groups" $ do
        sqlResult "parent_group_path"
        sqlWhereEq "id" ugid
      fetchOne runIdentity
    (Array1 new_parentpath :: Array1 UserGroupID) <- case ugParentGroupID new_ug of
      Nothing       -> return . Array1 $ ([] :: [UserGroupID])
      Just parentid -> do
        runQuery_ . sqlSelect "user_groups" $ do
          sqlResult "parent_group_path"
          sqlWhereEq "id" . Just $ parentid
        Array1 parentpath <- fetchOne runIdentity
        return . Array1 . (parentid :) $ parentpath
    -- verify, that groups will not form a cycle
    when (ugid `elem` new_parentpath)
      $ throwM
      . SomeDBExtraException
      . UserGroupsFormCycle
      $ ugid
    -- update user group
    runQuery_ . sqlUpdate "user_groups" $ do
      sqlSet "parent_group_id" $ ugParentGroupID new_ug
      sqlSet "parent_group_path" . Array1 $ new_parentpath
      sqlSet "name" $ ugName new_ug
      sqlSet "home_folder_id" $ ugHomeFolderID new_ug
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
      sqlWhere $ "parent_group_path @> " <?> (Array1 [ugid])

data UserGroupUpdateSettings = UserGroupUpdateSettings UserGroupID (Maybe UserGroupSettings)
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m UserGroupUpdateSettings () where
  update (UserGroupUpdateSettings ugid mugSettings) = do
    runQuery_ . sqlDelete "user_group_settings" $ sqlWhereEq "user_group_id" ugid
    whenJust mugSettings $ insertUserGroupSettings ugid

data UserGroupUpdateAddress = UserGroupUpdateAddress UserGroupID (Maybe UserGroupAddress)
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m UserGroupUpdateAddress () where
  update (UserGroupUpdateAddress ugid mugAddr) = do
    runQuery_ . sqlDelete "user_group_addresses" $ sqlWhereEq "user_group_id" ugid
    whenJust mugAddr $ insertUserGroupAddress ugid

data UserGroupsFormCycle = UserGroupsFormCycle UserGroupID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue UserGroupsFormCycle where
  toJSValue (UserGroupsFormCycle ugid) = runJSONGen $ do
    value "message"       ("User Groups Form Cycle" :: String)
    value "user_group_id" (show ugid)

instance DBExtraException UserGroupsFormCycle

guardIfHasNoParentThenIsValidRoot :: (MonadDB m, MonadThrow m) => UserGroup -> m ()
guardIfHasNoParentThenIsValidRoot ug = case ugParentGroupID ug of
  Just _  -> return ()
  Nothing -> case ugrFromUG ug of
    Just _  -> return ()
    Nothing -> throwM . SomeDBExtraException . UserGroupIsInvalidAsRoot $ ug

data UserGroupIsInvalidAsRoot = UserGroupIsInvalidAsRoot UserGroup
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

unsafeUserGroupIDToPartnerID :: UserGroupID -> PartnerID
unsafeUserGroupIDToPartnerID = unsafePartnerID . fromUserGroupID

data UserGroupFilter
  = UGFilterByString Text    -- ^ Contains the string anywhere
  | UGManyUsers                -- ^ Has non-trivial amount of users (includes invited users)
  | UGWithNonFreePricePlan     -- ^ Has a non-free price plan attached
  | UGWithAnyIdleDocTimeoutSet -- ^ Has any ugIdleDocTimeout{STATUS} set

data UserGroupsGetFiltered = UserGroupsGetFiltered [UserGroupFilter] (Maybe (Integer, Integer))
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupsGetFiltered [UserGroup] where
  query (UserGroupsGetFiltered filters moffsetlimit) = do
    runQuery_ $ sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      forM_ filters $ \case
        UGFilterByString text -> do
          sqlJoinOn "user_group_addresses"
                    "user_group_addresses.user_group_id = user_groups.id"
          mapM_ (sqlWhere . parenthesize . findWord) (words $ T.unpack text)
        UGManyUsers -> sqlWhereAny
          [ sqlWhere
            $ "((SELECT count(*) FROM users WHERE users.user_group_id = user_groups.id AND users.deleted IS NULL) > 1)"
          , sqlWhere
            $ "((SELECT count(*) FROM companyinvites WHERE companyinvites.user_group_id = user_groups.id) > 0)"
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
      findWordInName word = ("user_groups.name") <+> "ILIKE" <?> sqlwordpat word
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
    ugGetChildrenInheritingProperty (ugID c) ugProperty
  return $ inheriting_children ++ grandchildren

-- Get all children recursively
data UserGroupGetAllChildrenRecursive = UserGroupGetAllChildrenRecursive UserGroupID
instance (MonadDB m, MonadThrow m)
  => DBQuery m UserGroupGetAllChildrenRecursive [UserGroupWithChildren] where
  query (UserGroupGetAllChildrenRecursive ugid) = do
    runQuery_ $ sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhere $ "parent_group_path @> " <?> (Array1 [ugid])
    allChildren <- fetchMany fetchUserGroup
    let directChildren parentID =
          filter ((== Just parentID) . ugParentGroupID) allChildren
        mkChildren parentID = mkChild <$> directChildren parentID
        mkChild ug = I.UserGroupWithChildren ug . mkChildren $ ugID ug
    return $ mkChildren ugid

-- Synchronize these definitions with frontend/app/js/account/company.js
minUserGroupIdleDocTimeout :: Int16
minUserGroupIdleDocTimeout = 1
maxUserGroupIdleDocTimeout :: Int16
maxUserGroupIdleDocTimeout = 365
