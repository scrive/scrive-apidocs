module UserGroup.Model (
    UserGroupCreate(..)
  , UserGroupGet(..)
  , UserGroupGetByUserID(..)
  , UserGroupGetWithParents(..)
  , UserGroupsGetFiltered(..)
  , UserGroupUpdate(..)
  , UserGetAllParentGroups(..)
  , UserGroupGetAllChildren(..)
  , UserGroupsFormCycle(..)
  , UserGroupFilter(..)
  , ugInherited
  , companyFromUserGroup
  , fromUserGroupUI
  , unsafeUserGroupIDToCompanyID
  , unsafeUserGroupIDToPartnerID
  , minUserGroupIdleDocTimeout
  , maxUserGroupIdleDocTimeout
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Typeable
import Log
import Text.JSON.Gen
import qualified Data.Aeson as A
import qualified Data.Text as T

import Company.CompanyUI.Data
import Company.Data
import DB
import Log.Identifier
import Partner.Model
import User.Data.User
import User.UserID
import UserGroup.Data

ugInherited :: (UserGroup -> Maybe a) -> UserGroupWithParents -> Maybe a
ugInherited getter (ug, parents) = listToMaybe . catMaybes . fmap getter $ ug:parents

data UserGroupCreate = UserGroupCreate UserGroup
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupCreate UserGroup where
  update (UserGroupCreate ug) = do
    new_parentpath <- case get ugParentGroupID ug of
      Nothing -> return . Array1 $ ([] :: [UserGroupID])
      Just parentid -> do
        runQuery_ . sqlSelect "user_groups" $ do
          sqlResult "parent_group_path"
          sqlWhereEq "id" . Just $ parentid
        Array1 parentpath <- fetchOne runIdentity
        return . Array1 . (parentid:) $ parentpath
    -- insert user group
    runQuery_ . sqlInsert "user_groups" $ do
      sqlSet "parent_group_id" . get ugParentGroupID $ ug
      sqlSet "parent_group_path" $ new_parentpath
      sqlSet "name" . get ugName $ ug
      sqlResult "id"
    ugid <- fetchOne runIdentity
    -- insert group info
    let ugs = get ugSettings ug
    runQuery_ . sqlInsert "user_group_settings" $ do
      sqlSet "user_group_id" ugid
      sqlSet "ip_address_mask_list" $ case get ugsIPAddressMaskList ugs of
        [] -> Nothing
        x  -> Just (show x)
      sqlSet "idle_doc_timeout" . get ugsIdleDocTimeout $ ugs
      sqlSet "cgi_display_name" . get ugsCGIDisplayName $ ugs
      sqlSet "cgi_service_id" . get ugsCGIServiceID $ ugs
      sqlSet "sms_provider" . get ugsSMSProvider $ ugs
      sqlSet "pad_app_mode" . get ugsPadAppMode $ ugs
      sqlSet "pad_earchive_enabled" . get ugsPadEarchiveEnabled $ ugs
    -- insert group address
    let uga = get ugAddress ug
    runQuery_ . sqlInsert "user_group_addresses" $ do
      sqlSet "user_group_id" ugid
      sqlSet "company_number" . get ugaCompanyNumber $ uga
      sqlSet "address" . get ugaAddress $ uga
      sqlSet "zip" . get ugaZip $ uga
      sqlSet "city" . get ugaCity $ uga
      sqlSet "country" . get ugaCountry $ uga
    -- insert invoicing
    runQuery_ . sqlInsert "user_group_invoicings" $ do
        sqlSet "user_group_id" ugid
        sqlSet "invoicing_type" . ugInvoicingType $ ug
        sqlSet "payment_plan" . ugPaymentPlan $ ug
    -- insert UI
    let ugui = get ugUI ug
    runQuery_ . sqlInsert "user_group_uis" $ do
      sqlSet "user_group_id" ugid
      -- We are not setting themes here, because UserGroup, which does not
      -- exist yet, cannot own any themes.
      sqlSet "browser_title" . get uguiBrowserTitle $ ugui
      sqlSet "sms_originator" . get uguiSmsOriginator $ ugui
      sqlSet "favicon" . get uguiFavicon $ ugui


    -- we need to setup a corresponding company
    runQuery_ $ sqlInsert "companies" $ do
      sqlSet "id" . unsafeUserGroupIDToCompanyID $ ugid
      sqlSet "name" . get ugName $ ug
      sqlSet "payment_plan" . fromMaybe FreePlan . ugPaymentPlan $ ug
      sqlSet "ip_address_mask_list" $ case get ugsIPAddressMaskList ugs of
        [] -> Nothing
        x -> Just (show x)
      sqlSet "idle_doc_timeout" . get ugsIdleDocTimeout $ ugs
      sqlSet "cgi_display_name" . get ugsCGIDisplayName $ ugs
      sqlSet "sms_provider" . get ugsSMSProvider $ ugs
      sqlSet "cgi_service_id" . get ugsCGIServiceID $ ugs
      sqlSet "pad_app_mode" . get ugsPadAppMode $ ugs
      sqlSet "pad_earchive_enabled" . get ugsPadEarchiveEnabled $ ugs
      sqlSet "user_group_id" ugid
      -- Parents/Partners
      case get ugParentGroupID ug of
        Just ugidparent ->
          sqlSetCmd "partner_id" $ "(SELECT partners.id FROM partners WHERE partners.user_group_id = " <?> ugidparent <+> " LIMIT 1)"
        Nothing ->
          sqlSetCmd "partner_id" "(SELECT partners.id FROM partners WHERE partners.default_partner LIMIT 1)"
      -- Address
      sqlSet "number" . get ugaCompanyNumber $ uga
      sqlSet "address" . get ugaAddress $ uga
      sqlSet "zip" . get ugaZip $ uga
      sqlSet "city" . get ugaCity $ uga
      sqlSet "country" . get ugaCountry $ uga

    runQuery_ $ sqlInsert "company_uis" $ do
      sqlSet "company_id" . unsafeUserGroupIDToCompanyID $ ugid
      -- We are not setting themes here, because UserGroup, which does not
      -- exist yet, cannot own any themes.
      sqlSet "browser_title" . get uguiBrowserTitle $ ugui
      sqlSet "sms_originator" . get uguiSmsOriginator $ ugui
      sqlSet "favicon" . get uguiFavicon $ ugui

    runQuery_ $ sqlInsert "feature_flags" $ do
      sqlSet "can_use_dk_authentication_to_view" False
      sqlSet "can_use_dk_authentication_to_sign" False
      sqlSet "can_use_no_authentication_to_view" False
      sqlSet "can_use_no_authentication_to_sign" False
      sqlSet "can_use_se_authentication_to_view" False
      sqlSet "can_use_se_authentication_to_sign" False
      sqlSet "company_id" $ unsafeUserGroupIDToCompanyID ugid
      sqlSet "user_group_id" ugid

    return . set ugID ugid $ ug

data UserGroupGet = UserGroupGet UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGet (Maybe UserGroup) where
  query (UserGroupGet ugid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "id" ugid
    fetchMaybe toComposite

data UserGroupGetByUserID = UserGroupGetByUserID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetByUserID UserGroup where
  query (UserGroupGetByUserID uid) = do
    runQuery_ $ sqlSelect "user_groups" $ do
      sqlJoinOn "users" "users.user_group_id = user_groups.id"
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "users.id" uid
    fetchOne toComposite

data UserGroupGetAllChildren = UserGroupGetAllChildren UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetAllChildren [UserGroup] where
  query (UserGroupGetAllChildren ugid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "parent_group_id" ugid
    fetchMany toComposite

data UserGroupGetWithParents = UserGroupGetWithParents UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGetWithParents (Maybe (UserGroup,[UserGroup])) where
  query (UserGroupGetWithParents ugid) = do
    mug <- dbQuery . UserGroupGet $ ugid
    case mug of
      Nothing -> return Nothing
      Just ug -> do
        parents <- do
          -- JOIN does not guarantee to preserve order of rows, so we add ORDINALITY and ORDER BY it.
          -- WITH ORDINALITY can only appear inside FROM clause after a function call.
          runQuery_ . sqlSelect "user_groups" $ do
            sqlWith "parentids" . sqlSelect "user_groups, unnest(parent_group_path) WITH ORDINALITY" $ do
              sqlResult "unnest as id"
              sqlResult "ordinality"
              sqlWhereEq "id" ugid
            sqlJoinOn "parentids" "parentids.id = user_groups.id"
            mapM_ sqlResult userGroupSelectors
            sqlOrderBy "ordinality"
          fetchMany toComposite
        return . Just $ (ug, parents)


data UserGroupUpdate = UserGroupUpdate UserGroup
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m UserGroupUpdate () where
  update (UserGroupUpdate new_ug) = do
    let ugid = get ugID new_ug
    -- update group info
    let ugs = get ugSettings new_ug
    runQuery_ . sqlUpdate "user_group_settings" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "ip_address_mask_list" $ case get ugsIPAddressMaskList ugs of
        [] -> Nothing
        x  -> Just (show x)
      sqlSet "idle_doc_timeout" . get ugsIdleDocTimeout $ ugs
      sqlSet "cgi_display_name" . get ugsCGIDisplayName $ ugs
      sqlSet "cgi_service_id" . get ugsCGIServiceID $ ugs
      sqlSet "sms_provider" . get ugsSMSProvider $ ugs
      sqlSet "pad_app_mode" . get ugsPadAppMode $ ugs
      sqlSet "pad_earchive_enabled" . get ugsPadEarchiveEnabled $ ugs
    -- insert group address
    let uga = get ugAddress new_ug
    runQuery_ . sqlUpdate "user_group_addresses" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "company_number" . get ugaCompanyNumber $ uga
      sqlSet "address" . get ugaAddress $ uga
      sqlSet "zip" . get ugaZip $ uga
      sqlSet "city" . get ugaCity $ uga
      sqlSet "country" . get ugaCountry $ uga
    -- update invoicing
    runQuery_ . sqlUpdate "user_group_invoicings" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "invoicing_type" . ugInvoicingType $ new_ug
      sqlSet "payment_plan" . ugPaymentPlan $ new_ug
    -- update UI
    let ugui = get ugUI new_ug
        chkUgOwnsTheme thmLbl ugui' ugid' =
          when (isJust $ get thmLbl ugui') $ do
            sqlWhereExists $ sqlSelect "theme_owners" $ do
              sqlWhereEq "user_group_id" ugid'
              sqlWhereEq "theme_id" (get thmLbl ugui')

    runQuery_ . sqlUpdate "user_group_uis" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "mail_theme" . get uguiMailTheme $ ugui
      sqlSet "signview_theme" . get uguiSignviewTheme $ ugui
      sqlSet "service_theme" . get uguiServiceTheme $ ugui
      sqlSet "browser_title" . get uguiBrowserTitle $ ugui
      sqlSet "sms_originator" . get uguiSmsOriginator $ ugui
      sqlSet "favicon" . get uguiFavicon $ ugui

      chkUgOwnsTheme uguiMailTheme ugui ugid
      chkUgOwnsTheme uguiSignviewTheme ugui ugid
      chkUgOwnsTheme uguiServiceTheme ugui ugid

    -- updated group may have children already, these need to be adjusted
    Array1 (old_parentpath :: [UserGroupID])<- do
      runQuery_ . sqlSelect "user_groups" $ do
        sqlResult "parent_group_path"
        sqlWhereEq "id" ugid
      fetchOne runIdentity
    (Array1 new_parentpath :: Array1 UserGroupID) <- case get ugParentGroupID new_ug of
      Nothing -> return . Array1 $ ([] :: [UserGroupID])
      Just parentid -> do
        runQuery_ . sqlSelect "user_groups" $ do
          sqlResult "parent_group_path"
          sqlWhereEq "id" . Just $ parentid
        Array1 parentpath <- fetchOne runIdentity
        return . Array1 . (parentid:) $ parentpath
    -- verify, that groups will not form a cycle
    when (ugid `elem` new_parentpath) $
      throwM . SomeDBExtraException . UserGroupsFormCycle $ ugid
    -- update user group
    runQuery_ . sqlUpdate "user_groups" $ do
      sqlSet "parent_group_id" . get ugParentGroupID $ new_ug
      sqlSet "parent_group_path" . Array1 $ new_parentpath
      sqlSet "name" . get ugName $ new_ug
      sqlWhereEq "id" ugid
    -- update all child groups parentpaths
    runQuery_ . sqlUpdate "user_groups" $ do
      -- to remove multiple items at once from ARRAY, there is only slicing available
      -- inside slicing, we must to specify the index of last item
      -- we cut old items from start and then prepend the new parent path
      sqlSetCmd "parent_group_path" $ "array_cat(parent_group_path[ 1"<>
                                                                 ": ( array_length(parent_group_path, 1)"<>
                                                                   "- " <?> length old_parentpath <+> ")]"<>
                                               "," <?> Array1 new_parentpath <+> ")"
      sqlWhere $ "parent_group_path @> " <?> (Array1 [ugid])

    -- we need to update the corresponding company
    let cid = unsafeUserGroupIDToCompanyID ugid
    runQuery_ $ sqlUpdate "companies" $ do
      sqlSet "name" . get ugName $ new_ug
      sqlSet "payment_plan" . fromMaybe FreePlan . ugPaymentPlan $ new_ug
      -- Info
      sqlSet "ip_address_mask_list" $ case get ugsIPAddressMaskList ugs of
        [] -> Nothing
        x -> Just (show x)
      sqlSet "idle_doc_timeout" . get ugsIdleDocTimeout $ ugs
      sqlSet "cgi_display_name" . get ugsCGIDisplayName $ ugs
      sqlSet "sms_provider" . get ugsSMSProvider $ ugs
      sqlSet "cgi_service_id" . get ugsCGIServiceID $ ugs
      sqlSet "pad_app_mode" . get ugsPadAppMode $ ugs
      sqlSet "pad_earchive_enabled" . get ugsPadEarchiveEnabled $ ugs
      -- Parent / Partner
      case get ugParentGroupID new_ug of
        Just ugidparent ->
          sqlSetCmd "partner_id" $ "(SELECT partners.id FROM partners WHERE partners.user_group_id = " <?> ugidparent <+> " LIMIT 1)"
        Nothing ->
          sqlSetCmd "partner_id" "(SELECT partners.id FROM partners WHERE partners.default_partner LIMIT 1)"
      -- Address
      sqlSet "number" . get ugaCompanyNumber $ uga
      sqlSet "address" . get ugaAddress $ uga
      sqlSet "zip" . get ugaZip $ uga
      sqlSet "city" . get ugaCity $ uga
      sqlSet "country" . get ugaCountry $ uga
      sqlWhereEq "id" cid

    is_success <- runQuery01 $ sqlUpdate "company_uis" $ do
      sqlSet "mail_theme" . get uguiMailTheme $ ugui
      sqlSet "signview_theme" . get uguiSignviewTheme $ ugui
      sqlSet "service_theme" . get uguiServiceTheme $ ugui
      sqlSet "browser_title" . get uguiBrowserTitle $ ugui
      sqlSet "sms_originator" . get uguiSmsOriginator $ ugui
      sqlSet "favicon" . get uguiFavicon $ ugui
      sqlWhereEq "company_id" cid

      whenJust (get uguiMailTheme ugui) $ \uguimailtheme ->
        sqlWhereExists $ sqlSelect "theme_owners" $ do
          sqlWhereEq "company_id" cid
          sqlWhereEq "theme_id" uguimailtheme

      whenJust (get uguiSignviewTheme ugui) $ \uguisignviewtheme ->
        sqlWhereExists $ sqlSelect "theme_owners" $ do
          sqlWhereEq "company_id" cid
          sqlWhereEq "theme_id" uguisignviewtheme

      whenJust (get uguiServiceTheme ugui) $ \uguiservicetheme ->
        sqlWhereExists $ sqlSelect "theme_owners" $ do
          sqlWhereEq "company_id" cid
          sqlWhereEq "theme_id" uguiservicetheme

    when (not is_success) $ logAttention "UserGroupUpdate synchronization to companyui failed" $ A.object [
        identifier_ ugid
      ]

data UserGroupsFormCycle = UserGroupsFormCycle UserGroupID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue UserGroupsFormCycle where
  toJSValue (UserGroupsFormCycle ugid) = runJSONGen $ do
      value "message" ("User Groups Form Cycle" :: String)
      value "user_group_id" (show ugid)

instance DBExtraException UserGroupsFormCycle

data UserGetAllParentGroups = UserGetAllParentGroups User
instance (MonadDB m, MonadThrow m) => DBQuery m UserGetAllParentGroups [UserGroup] where
  query (UserGetAllParentGroups u) = do
    Array1 (parentpath :: [UserGroupID]) <- do
      runQuery_ . sqlSelect "user_groups" $ do
        sqlResult "parent_group_path"
        sqlWhereEq "id" . usergroupid $ u
      fetchOne runIdentity
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereAny [
          sqlWhereEq "id" . usergroupid $ u
        , sqlWhereIn "id" parentpath
        ]
    fetchMany toComposite

userGroupSelectors :: [SQL]
userGroupSelectors = [
    "user_groups.id"
  , "user_groups.parent_group_id"
  , "user_groups.name"
  , "(SELECT (" <> mintercalate ", " ugInvoicingSelectors <> ")::user_group_invoicing FROM user_group_invoicings WHERE user_groups.id = user_group_invoicings.user_group_id)"
  , "(SELECT (" <> mintercalate ", " ugSettingsSelectors <> ")::user_group_setting FROM user_group_settings WHERE user_groups.id = user_group_settings.user_group_id)"
  , "(SELECT (" <> mintercalate ", " ugAddressSelectors <> ")::user_group_address FROM user_group_addresses WHERE user_groups.id = user_group_addresses.user_group_id)"
  , "(SELECT (" <> mintercalate ", " ugUISelectors <> ")::user_group_ui FROM user_group_uis WHERE user_groups.id = user_group_uis.user_group_id)"
  ]

ugSettingsSelectors :: [SQL]
ugSettingsSelectors = [
    "ip_address_mask_list"
  , "idle_doc_timeout"
  , "cgi_display_name"
  , "sms_provider"
  , "cgi_service_id"
  , "pad_app_mode"
  , "pad_earchive_enabled"
  ]

ugAddressSelectors :: [SQL]
ugAddressSelectors = [
    "company_number"
  , "address"
  , "zip"
  , "city"
  , "country"
  ]

ugUISelectors :: [SQL]
ugUISelectors = [
    "mail_theme"
  , "signview_theme"
  , "service_theme"
  , "browser_title"
  , "sms_originator"
  , "favicon"
  ]

ugInvoicingSelectors :: [SQL]
ugInvoicingSelectors = [
    "invoicing_type"
  , "payment_plan"
  ]

-- | Set `user_group_id` for `companies` - does _one_ by `id`
data SetCompanyUserGroupID = SetCompanyUserGroupID CompanyID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetCompanyUserGroupID Bool where
  update (SetCompanyUserGroupID cID ugid) = do
    runQuery01 . sqlUpdate "companies" $ do
      sqlSet "user_group_id" ugid
      sqlWhereEq "id" cID

-- | Set `user_group_id` for `companies` - does _one_ by `id`
data SetCompanyName = SetCompanyName CompanyID String
instance (MonadDB m, MonadThrow m) => DBUpdate m SetCompanyName Bool where
  update (SetCompanyName cID name) = do
    runQuery01 . sqlUpdate "companies" $ do
      sqlSet "name" name
      sqlWhereEq "id" cID

-- | Set `user_group_id` for `partners` - does _one_ by `id`
data SetPartnerUserGroupID = SetPartnerUserGroupID PartnerID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetPartnerUserGroupID Bool where
  update (SetPartnerUserGroupID pID ugid) = do
    runQuery01 . sqlUpdate "partners" $ do
      sqlSet "user_group_id" ugid
      sqlWhereEq "id" pID

-- | Set `user_group_id` for `users` - does _all_ by `company_id`
data SetCompanyUsersUserGroupID = SetCompanyUsersUserGroupID CompanyID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetCompanyUsersUserGroupID () where
  update (SetCompanyUsersUserGroupID cID ugid) = do
    runQuery_ . sqlUpdate "users" $ do
      sqlSet "user_group_id" ugid
      sqlWhereEq "company_id" cID

-- | Set `user_group_id` for `feature_flags` - does _all_ by `company_id`
data SetFFlagsUserGroupID = SetFFlagsUserGroupID CompanyID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetFFlagsUserGroupID () where
  update (SetFFlagsUserGroupID cID ugid) = do
    runQuery_ . sqlUpdate "feature_flags" $ do
      sqlSet "user_group_id" ugid
      sqlWhereEq "company_id" cID

-- | Set `user_group_id` for `chargeable_items` - does _all_ by `company_id`
data SetChargeableUserGroupID = SetChargeableUserGroupID CompanyID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetChargeableUserGroupID () where
  update (SetChargeableUserGroupID cID ugid) = do
    runQuery_ . sqlUpdate "chargeable_items" $ do
      sqlSet "user_group_id" ugid
      sqlWhereEq "company_id" cID

-- | Set `user_group_id` for `theme_owners` - does _all_ by `company_id`
data SetThemeUserGroupID = SetThemeUserGroupID CompanyID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetThemeUserGroupID () where
  update (SetThemeUserGroupID cID ugid) = do
    runQuery_ . sqlUpdate "theme_owners" $ do
      sqlSet "user_group_id" ugid
      sqlWhereEq "company_id" cID

unsafeUserGroupIDToCompanyID :: UserGroupID -> CompanyID
unsafeUserGroupIDToCompanyID = unsafeCompanyID . fromUserGroupID

unsafeUserGroupIDToPartnerID :: UserGroupID -> PartnerID
unsafeUserGroupIDToPartnerID = unsafePartnerID . fromUserGroupID

companyFromUserGroup :: UserGroup -> [Partner] -> Company
companyFromUserGroup ug allpartners =
  Company {
      companyid          = unsafeUserGroupIDToCompanyID . get ugID $ ug
    , companyusergroupid = get ugID ug
    , companyinfo = CompanyInfo {
        companyname                = T.unpack . get ugName $ ug
      , companynumber              = T.unpack . get (ugaCompanyNumber . ugAddress) $ ug
      , companyaddress             = T.unpack . get (ugaAddress       . ugAddress) $ ug
      , companyzip                 = T.unpack . get (ugaZip           . ugAddress) $ ug
      , companycity                = T.unpack . get (ugaCity          . ugAddress) $ ug
      , companycountry             = T.unpack . get (ugaCountry       . ugAddress) $ ug
      , companyipaddressmasklist   = get (ugsIPAddressMaskList   . ugSettings) ug
      , companyidledoctimeout      = get (ugsIdleDocTimeout      . ugSettings) ug
      , companypadappmode          = get (ugsPadAppMode          . ugSettings) ug
      , companypadearchiveenabled  = get (ugsPadEarchiveEnabled  . ugSettings) ug
      , companysmsprovider         = get (ugsSMSProvider         . ugSettings) ug
      , companycgidisplayname      = fmap T.unpack . get (ugsCGIDisplayName . ugSettings) $ ug
      , companycgiserviceid        = fmap T.unpack . get (ugsCGIServiceID   . ugSettings) $ ug
      , companypaymentplan         = fromJust . ugPaymentPlan $ ug
      , companypartnerid           = ptID . fromJust $ case get ugParentGroupID ug of
          Nothing         -> find (\p -> ptDefaultPartner p) $ allpartners
          Just parentugid -> find (\p -> ptUserGroupID p == Just parentugid) $ allpartners
      }
    }

fromUserGroupUI :: UserGroup -> CompanyUI
fromUserGroupUI ug = let ugui = get ugUI ug in
  CompanyUI {
      companyuicompanyid   = unsafeUserGroupIDToCompanyID . get ugID $ ug
    , companyMailTheme     = get uguiMailTheme ugui
    , companySignviewTheme = get uguiSignviewTheme ugui
    , companyServiceTheme  = get uguiServiceTheme ugui
    , companyBrowserTitle  = fmap T.unpack . get uguiBrowserTitle $ ugui
    , companySmsOriginator = fmap T.unpack . get uguiSmsOriginator $ ugui
    , companyFavicon       = get uguiFavicon ugui
    }

data UserGroupFilter
  = UGFilterByString String -- ^ Contains the string anywhere
  | UGManyUsers             -- ^ Has non-trivial amount of users (includes invited users)
  | UGWithNonFreePricePlan  -- ^ Has a non-free price plan attached
  | UGWithIdleDocTimeoutSet -- ^ Has ugIdleDocTimeout set

data UserGroupsGetFiltered = UserGroupsGetFiltered [UserGroupFilter] (Maybe (Integer, Integer))
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupsGetFiltered [UserGroup] where
  query (UserGroupsGetFiltered filters moffsetlimit) = do
    runQuery_ $ sqlSelect "user_groups" $ do
       mapM_ sqlResult userGroupSelectors
       forM_ filters $ \case
         UGFilterByString text -> do
           sqlJoinOn "user_group_addresses" "user_group_addresses.user_group_id = user_groups.id"
           mapM_ (sqlWhere . parenthesize . findWord) (words text)
         UGManyUsers -> sqlWhereAny
           [ sqlWhere $ "((SELECT count(*) FROM users WHERE users.user_group_id = user_groups.id AND users.deleted IS NULL) > 1)"
           , sqlWhere $ "((SELECT count(*) FROM companyinvites WHERE companyinvites.company_id = user_groups.id) > 0)"
           ]
         UGWithNonFreePricePlan -> do
           sqlJoinOn "user_group_invoicings" "user_group_invoicings.user_group_id = user_groups.id"
           sqlWhere $ "(user_group_invoicings.payment_plan != "<?> FreePlan <> ")"
         UGWithIdleDocTimeoutSet -> do
           sqlJoinOn "user_group_settings" "user_group_settings.user_group_id = user_groups.id"
           sqlWhereIsNotNULL "user_group_settings.idle_doc_timeout"
       whenJust moffsetlimit $ \(offset, limit) -> do
         sqlOffset offset
         sqlLimit limit
       sqlOrderBy "user_groups.id"
    fetchMany toComposite
    where
      findWordInField word fieldName = ("user_group_addresses." <+> fieldName) <+> "ILIKE" <?> sqlwordpat word
      findWordInName word = ("user_groups.name") <+> "ILIKE" <?> sqlwordpat word
      findWordList word = map (findWordInField word) ["company_number", "address", "zip", "city", "country"]
      findWord word = sqlConcatOR $ findWordInName word : findWordList word
      sqlwordpat word = "%" ++ concatMap escape word ++ "%"
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]

-- Synchronize these definitions with frontend/app/js/account/company.js
minUserGroupIdleDocTimeout :: Int16
minUserGroupIdleDocTimeout = 1
maxUserGroupIdleDocTimeout :: Int16
maxUserGroupIdleDocTimeout = 365
