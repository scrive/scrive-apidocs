module UserGroup.Model (
    UserGroupCreate(..)
  , UserGroupGet(..)
  , UserGroupGetWithParents(..)
  , UserGroupUpdate(..)
  , UserGetAllParentGroups(..)

  , UserGroupsFormCycle(..)

  , ugInherited

  , migrateToUserGroups
  , companyMigrateForDefaultPartner
  , companyToUserGroup
  , toUserGroupUI
  ) where

import Control.Monad.Catch
import Control.Monad.State (State)
import Control.Monad.Time
import Data.Aeson.Types ((.=))
import Data.Int (Int16)
import Data.Time (diffUTCTime)
import Data.Typeable
import Log (MonadLog, logInfo)
import Log as L
import Text.JSON.Gen
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Company.CompanyUI.Data
import Company.Data
import DB
import Log.Identifier
import PadApplication.Data (PadAppMode)
import Partner.PartnerID
import SMS.Data (SMSProvider)
import User.Data.User
import UserGroup.Data

ugInherited :: (UserGroup -> Maybe a) -> UserGroupWithParents -> Maybe a
ugInherited getter (ug, parents) = listToMaybe . catMaybes . fmap getter $ ug:parents

-- | Create an entirely new user group (with no connections to a previously existing
--   company).
data UserGroupCreate = UserGroupCreate UserGroup
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupCreate UserGroup where
  update (UserGroupCreate ug) = update (UserGroupCreate' ug True)

-- | Create a new user group with the assumption that it is based on a
-- company. The ID of the company is therefore inserted.
data UserGroupCreateRetainCompanyID = UserGroupCreateRetainCompanyID UserGroup
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupCreateRetainCompanyID UserGroup where
  update (UserGroupCreateRetainCompanyID ug) = update (UserGroupCreate' ug False)

data UserGroupCreate' = UserGroupCreate' UserGroup Bool
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupCreate' UserGroup where
  update (UserGroupCreate' ug mkNewID) = do
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
      unless mkNewID $ sqlSet "id" . get ugID $ ug
      sqlSet "parent_group_id" . get ugParentGroupID $ ug
      sqlSet "parent_group_path" $ new_parentpath
      sqlSet "name" . get ugName $ ug
      sqlResult "id"
    ugid <- fetchOne runIdentity
    -- insert group info
    let ugi = get ugInfo ug
    runQuery_ . sqlInsert "user_group_infos" $ do
      sqlSet "user_group_id" ugid
      sqlSet "ip_address_mask_list" $ case get ugiIPAddressMaskList ugi of
        [] -> Nothing
        x  -> Just (show x)
      sqlSet "idle_doc_timeout" . get ugiIdleDocTimeout $ ugi
      sqlSet "cgi_display_name" . get ugiCGIDisplayName $ ugi
      sqlSet "cgi_service_id" . get ugiCGIServiceID $ ugi
      sqlSet "sms_provider" . get ugiSMSProvider $ ugi
      sqlSet "pad_app_mode" . get ugiPadAppMode $ ugi
      sqlSet "pad_earchive_enabled" . get ugiPadEarchiveEnabled $ ugi
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
    -- insert UI, if there is some
    let ugui = get ugUI ug
    runQuery_ . sqlInsert "user_group_uis" $ do
      sqlSet "user_group_id" ugid
      sqlSet "mail_theme" . get uguiMailTheme $ ugui
      sqlSet "signview_theme" . get uguiSignviewTheme $ ugui
      sqlSet "service_theme" . get uguiServiceTheme $ ugui
      sqlSet "browser_title" . get uguiBrowserTitle $ ugui
      sqlSet "sms_originator" . get uguiSmsOriginator $ ugui
      sqlSet "favicon" . get uguiFavicon $ ugui
    return . set ugID ugid $ ug

data UserGroupGet = UserGroupGet UserGroupID
instance (MonadDB m, MonadThrow m) => DBQuery m UserGroupGet (Maybe UserGroup) where
  query (UserGroupGet ugid) = do
    runQuery_ . sqlSelect "user_groups" $ do
      mapM_ sqlResult userGroupSelectors
      sqlWhereEq "id" ugid
    fetchMaybe toComposite

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
instance (MonadDB m, MonadThrow m) => DBUpdate m UserGroupUpdate () where
  update (UserGroupUpdate new_ug) = do
    let ugid = get ugID new_ug
    -- update group info
    let ugi = get ugInfo new_ug
    runQuery_ . sqlUpdate "user_group_infos" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "ip_address_mask_list" $ case get ugiIPAddressMaskList ugi of
        [] -> Nothing
        x  -> Just (show x)
      sqlSet "idle_doc_timeout" . get ugiIdleDocTimeout $ ugi
      sqlSet "cgi_display_name" . get ugiCGIDisplayName $ ugi
      sqlSet "cgi_service_id" . get ugiCGIServiceID $ ugi
      sqlSet "sms_provider" . get ugiSMSProvider $ ugi
      sqlSet "pad_app_mode" . get ugiPadAppMode $ ugi
      sqlSet "pad_earchive_enabled" . get ugiPadEarchiveEnabled $ ugi
    -- insert group address
    let uga = get ugAddress new_ug
    runQuery_ . sqlUpdate "user_group_addresses" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "company_number" . get ugaCompanyNumber $ uga
      sqlSet "address" . get ugaAddress $ uga
      sqlSet "zip" . get ugaZip $ uga
      sqlSet "city" . get ugaCity $ uga
      sqlSet "country" . get ugaCountry $ uga
    -- insert invoicing
    runQuery_ . sqlUpdate "user_group_invoicings" $ do
        sqlWhereEq "user_group_id" ugid
        sqlSet "invoicing_type" . ugInvoicingType $ new_ug
        sqlSet "payment_plan" . ugPaymentPlan $ new_ug
    -- insert UI
    let ugui = get ugUI new_ug
    runQuery_ . sqlUpdate "user_group_uis" $ do
      sqlWhereEq "user_group_id" ugid
      sqlSet "mail_theme" . get uguiMailTheme $ ugui
      sqlSet "signview_theme" . get uguiSignviewTheme $ ugui
      sqlSet "service_theme" . get uguiServiceTheme $ ugui
      sqlSet "browser_title" . get uguiBrowserTitle $ ugui
      sqlSet "sms_originator" . get uguiSmsOriginator $ ugui
      sqlSet "favicon" . get uguiFavicon $ ugui
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
  , "(SELECT (" <> mintercalate ", " ugInfoSelectors <> ")::user_group_info FROM user_group_infos WHERE user_groups.id = user_group_infos.user_group_id)"
  , "(SELECT (" <> mintercalate ", " ugAddressSelectors <> ")::user_group_address FROM user_group_addresses WHERE user_groups.id = user_group_addresses.user_group_id)"
  , "(SELECT (" <> mintercalate ", " ugUISelectors <> ")::user_group_ui FROM user_group_uis WHERE user_groups.id = user_group_uis.user_group_id)"
  ]

ugInfoSelectors :: [SQL]
ugInfoSelectors = [
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

-- Companies that have a user that is a partner admin will become the roots in a
-- user group tree that includes the companies that are administrated by this
-- user. The next two actions make this happen.
data GetPartnerRootCompaniesToMigrate = GetPartnerRootCompaniesToMigrate Int
instance (MonadDB m, MonadThrow m) => DBQuery m GetPartnerRootCompaniesToMigrate [(CompanyID, PartnerID)] where
  query (GetPartnerRootCompaniesToMigrate limit) = do
    runQuery_ . sqlSelect "companies c" $ do
      sqlResult "c.id"
      sqlResult "p.id"
      sqlJoinOn "users u" "c.id = u.company_id"
      sqlJoinOn "partner_admins pa" "pa.user_id = u.id"
      sqlJoinOn "partners p" "p.id = pa.partner_id and not p.default_partner" -- on the safe side
      sqlWhereIsNULL "p.user_group_id"
      sqlLimit limit
    fetchMany id

data CronUserGroupUpdatePartnerRoots = CronUserGroupUpdatePartnerRoots Int
instance (MonadDB m, MonadLog m, MonadMask m, MonadThrow m) => DBUpdate m CronUserGroupUpdatePartnerRoots [Bool] where
  update (CronUserGroupUpdatePartnerRoots limit) = do
    cpIDs <- query $ GetPartnerRootCompaniesToMigrate limit
    forM cpIDs $ \(companyID, partnerID) -> do
      mCompany <- query $ GetCompany' companyID
      case mCompany of
        Nothing -> do
          logInfo "Invalid company ID or no UI" $ A.object
               [ ("company_id" :: T.Text) .= (fromCompanyID companyID) ]
          return False
        Just company -> do
          cui <- query $ GetCompanyUI' companyID
          let userGroup = companyToUserGroup company cui Nothing
          withSavepoint "create_user_group" $ do
            ug <- update $ UserGroupCreateRetainCompanyID userGroup
            let ugid = get ugID ug
            res  <- update $ SetCompanyUserGroupID companyID ugid
            res' <- update $ SetPartnerUserGroupID partnerID ugid
            _ <- update $ SetCompanyUsersUserGroupID companyID ugid
            _ <- update $ SetFFlagsUserGroupID companyID ugid
            _ <- update $ SetChargeableUserGroupID companyID ugid
            _ <- update $ SetThemeUserGroupID companyID ugid
            return (res && res')

-- Assumes that `CronUserGroupUpdatePartnerRoots` has successfully done its job
-- in order to get a root user group.
data GetCompaniesToMigrateForNonDefaultPartner = GetCompaniesToMigrateForNonDefaultPartner Int
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompaniesToMigrateForNonDefaultPartner [(CompanyID, UserGroupID)] where
  query (GetCompaniesToMigrateForNonDefaultPartner limit) = do
    runQuery_ . sqlSelect "companies c" $ do
      sqlResult "c.id"
      sqlResult "partner_admin_company.user_group_id" -- partner root user group
      sqlJoinOn "company_uis cui" "cui.company_id = c.id"
      -- ^ condition as in e.g. `GetCompany`,
      sqlJoinOn "partners p" "p.id = c.partner_id and not p.default_partner"
      sqlJoinOn "partner_admins pa" "pa.partner_id = c.partner_id"
      sqlJoinOn "users partner_admin_users" "partner_admin_users.id = pa.user_id"
      sqlJoinOn "companies partner_admin_company" "partner_admin_users.company_id = partner_admin_company.id"
      sqlWhereIsNULL "c.user_group_id"
      sqlLimit limit
    fetchMany id

data CronUserGroupUpdateNonDefaultPartner = CronUserGroupUpdateNonDefaultPartner Int
instance (MonadDB m, MonadLog m, MonadThrow m, MonadMask m) => DBUpdate m CronUserGroupUpdateNonDefaultPartner [Bool] where
  update (CronUserGroupUpdateNonDefaultPartner limit) = do
    companyIDsWithParents <- query $ GetCompaniesToMigrateForNonDefaultPartner limit
    forM companyIDsWithParents $ \(companyID, parentGrpID) -> do
      mCompany <- query $ GetCompany' companyID
      case mCompany of
        Nothing -> do
          logInfo "Invalid company ID or no UI" $ A.object
            [ ("company_id" :: T.Text) .= (fromCompanyID companyID) ]
          return False
        Just company -> do
          cui <- query $ GetCompanyUI' companyID
          let userGroup = companyToUserGroup company cui (Just parentGrpID)
          withSavepoint "update_non_default_partner" $ do
            ug <- update $ UserGroupCreateRetainCompanyID userGroup
            let ugid = get ugID ug
            _ <- update $ SetCompanyUsersUserGroupID companyID ugid
            _ <- update $ SetFFlagsUserGroupID companyID ugid
            _ <- update $ SetChargeableUserGroupID companyID ugid
            _ <- update $ SetThemeUserGroupID companyID ugid
            update $ SetCompanyUserGroupID companyID ugid

data GetCompaniesToMigrateForDefaultPartner = GetCompaniesToMigrateForDefaultPartner Int
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompaniesToMigrateForDefaultPartner [CompanyID] where
  query (GetCompaniesToMigrateForDefaultPartner limit) = do
    runQuery_ . sqlSelect "companies c" $ do
      sqlResult "c.id"
      sqlJoinOn "company_uis cui" "cui.company_id = c.id"
      -- ^ as in e.g. `GetCompany`
      sqlJoinOn "partners p" "p.id = c.partner_id and p.default_partner"
      sqlWhereIsNULL "c.user_group_id"
      sqlLimit limit
    fetchMany runIdentity

-- | To support the `UserGroupMigration` cron job.
data CronUserGroupUpdateDefaultPartner = CronUserGroupUpdateDefaultPartner Int
instance ( MonadDB m
         , MonadLog m
         , MonadThrow m
         , MonadMask m) => DBUpdate m CronUserGroupUpdateDefaultPartner [Bool] where
  update (CronUserGroupUpdateDefaultPartner limit) = do
    -- not heavy: query takes ~ 20ms on prod without the c.user_group_id
    -- criterion and on a cold cache, ~ 3 ms on a warm one
    -- As I write this there are 128600 companies having default partner.
    companyIDs <- query $ GetCompaniesToMigrateForDefaultPartner limit
    -- In order to keep the memory footprint low we do this one company at a
    -- time. These companies will be *root* user groups.
    forM companyIDs $ companyMigrateForDefaultPartner

companyMigrateForDefaultPartner :: ( MonadDB m
                                   , MonadLog m
                                   , MonadMask m
                                   , MonadThrow m) => CompanyID -> m Bool
companyMigrateForDefaultPartner companyID = do
  mCompany <- query $ GetCompany' companyID
  case mCompany of
    Nothing -> do
      logInfo "Invalid company ID or no UI" $ A.object
        [ ("company_id" :: T.Text) .= (fromCompanyID companyID) ]
      return False
    Just company -> do
      cui <- query $ GetCompanyUI' companyID
      let userGroup = companyToUserGroup company cui Nothing
      ug <- update $ UserGroupCreateRetainCompanyID userGroup
      let ugid = get ugID ug
      withSavepoint "migrate_default_partner" $ do
        res <- update $ SetCompanyUserGroupID companyID ugid
        _ <- update $ SetCompanyUsersUserGroupID companyID ugid
        _ <- update $ SetFFlagsUserGroupID companyID ugid
        _ <- update $ SetChargeableUserGroupID companyID ugid
        _ <- update $ SetThemeUserGroupID companyID ugid
        when (not res) $
          logAttention "Unable to migrate user group: " $ L.object [
              identifier_ companyID
            , identifier_ ugid
            ]
        return res

-- helpers

-- remove once Cron job has finished. Only a local copy of
-- Company.Model.GetCompany to avoid import loops.
data GetCompany' = GetCompany' CompanyID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompany' (Maybe Company) where
  query (GetCompany' compID) = do
    runQuery_ $ sqlSelect "companies" $ do
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "id" compID
    fetchMaybe fetchCompany
    where
      selectCompaniesSelectors :: (SqlResult command) => State command ()
      selectCompaniesSelectors = do
        sqlResult "companies.id"
        sqlResult "companies.name"
        sqlResult "companies.number"
        sqlResult "companies.address"
        sqlResult "companies.zip"
        sqlResult "companies.city"
        sqlResult "companies.country"
        sqlResult "companies.ip_address_mask_list"
        sqlResult "companies.idle_doc_timeout"
        sqlResult "companies.cgi_display_name"
        sqlResult "companies.sms_provider"
        sqlResult "companies.cgi_service_id"
        sqlResult "companies.payment_plan"
        sqlResult "companies.partner_id"
        sqlResult "companies.pad_app_mode"
        sqlResult "companies.pad_earchive_enabled"
        sqlResult "companies.user_group_id"

      fetchCompany :: (CompanyID, String, String, String, String, String, String,
                       Maybe String, Maybe Int16, Maybe String,
                       SMSProvider, Maybe String, PaymentPlan, PartnerID, PadAppMode,
                       Bool, Maybe UserGroupID) -> Company
      fetchCompany (cid, name, number, address, zip', city, country,
                    ip_address_mask_list, idle_doc_timeout, cgi_display_name,
                    sms_provider, cgi_service_id, payment_plan,  partner_id, pad_app_mode,
                    pad_earchive_enabled, muser_group_id) = Company {
        companyid = cid
      , companyusergroupid = muser_group_id
      , companyinfo = CompanyInfo {
          companyname = name
        , companynumber = number
        , companyaddress = address
        , companyzip = zip'
        , companycity = city
        , companycountry = country
        , companyipaddressmasklist = maybe [] read ip_address_mask_list
        , companyidledoctimeout = idle_doc_timeout
        , companycgidisplayname = cgi_display_name
        , companysmsprovider = sms_provider
        , companycgiserviceid = cgi_service_id
        , companypaymentplan = payment_plan
        , companypartnerid = partner_id
        , companypadappmode = pad_app_mode
        , companypadearchiveenabled = pad_earchive_enabled
        }
      }

-- remove once Cron job has finished. Only a local copy of
-- Company.CompanyUI.Model.GetCompanyUI to avoid import loops.
data GetCompanyUI' = GetCompanyUI' CompanyID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompanyUI' CompanyUI where
  query (GetCompanyUI' cid) = do
    runQuery_ . sqlSelect "company_uis" $ do
      sqlWhereEq "company_id" cid
      selectCompanyUIsSelectors
    fetchOne fetchCompanyUI
    where
      selectCompanyUIsSelectors = do
        sqlResult "company_uis.company_id"
        sqlResult "company_uis.mail_theme"
        sqlResult "company_uis.signview_theme"
        sqlResult "company_uis.service_theme"
        sqlResult "browser_title"
        sqlResult "sms_originator"
        sqlResult "favicon"

      fetchCompanyUI (company_id,mail_theme,signview_theme,service_theme,browser_title,sms_originator,favicon) = CompanyUI {
        companyuicompanyid = company_id
      , companyMailTheme = mail_theme
      , companySignviewTheme = signview_theme
      , companyServiceTheme = service_theme
      , companyBrowserTitle = browser_title
      , companySmsOriginator = sms_originator
      , companyFavicon = faviconFromBinary favicon
      }
        where
          -- We should interpret empty logos as no logos.
          faviconFromBinary (Just f) = if (BS.null f) then Nothing else Just f
          faviconFromBinary Nothing = Nothing

migrateToUserGroups :: ( MonadTime m
                       , MonadDB m
                       , MonadLog m
                       , MonadThrow m
                       , MonadMask m)
                    => Int
                    -> m Int
migrateToUserGroups batchLimit = do
  startTime <- currentTime
  -- 1. Update the partner roots
  updatedPartners <- dbUpdate $ CronUserGroupUpdatePartnerRoots batchLimit
  -- 2. Update the companies that will have one of these as a root
  --    *if* the roots are done.
  -- 3. Update all the companies that have default partner, but also only if the
  -- roots are done.
  remNumNonDefaultPartners <- do
      length <$> (dbQuery $ GetPartnerRootCompaniesToMigrate batchLimit)
  (updatedNonDefPartnerGroups, updatedDefPartnerGroups) <-
      if remNumNonDefaultPartners > 0
      then do
        logInfo "Partner root user groups remain" $ A.object
           [ "partners_remaining" .= remNumNonDefaultPartners ]
        return ([],[])
      else do
        rs  <- dbUpdate $ CronUserGroupUpdateNonDefaultPartner batchLimit
        rs' <- dbUpdate $ CronUserGroupUpdateDefaultPartner batchLimit
        return (rs, rs')
  endTime <- currentTime
  let delta = diffUTCTime endTime startTime
      countTrues = length . filter id
  logInfo "User groups updated" $ A.object
          [ "partner_roots" .= (countTrues updatedPartners)
          , "non_default_partner_ugs" .= (countTrues updatedNonDefPartnerGroups)
          , "default_partner_ugs" .= (countTrues updatedDefPartnerGroups)
          , "elapsed_time" .= (realToFrac delta :: Double) ]
  return . countTrues $ updatedPartners <>
                        updatedNonDefPartnerGroups <>
                        updatedDefPartnerGroups

companyIDToUserGroupID :: CompanyID -> UserGroupID
companyIDToUserGroupID = unsafeUserGroupID . fromCompanyID

paymentPlanToUserGroupInvoicing :: Bool -> PaymentPlan -> UserGroupInvoicing
paymentPlanToUserGroupInvoicing hasNonDefaultPartner =
    case hasNonDefaultPartner of
      True ->  BillItem . Just
      False -> Invoice
-- or: _ -> Invoice

-- @devnote deal with companyusergroupid :: Maybe UserGroupID ???

-- Since this is to be used only to migrate the old structure into the new, and
-- having only potentially one level below a possible partner root company, we
-- use a simple Maybe to indicate whether it is connected to a partner or
-- not. If not, the company is a root user group of its own.
companyToUserGroup :: Company -> CompanyUI -> Maybe UserGroupID -> UserGroup
companyToUserGroup c cui mParentGroupID =
  UserGroup
    {
      _ugID            = companyIDToUserGroupID . companyid $ c
    , _ugParentGroupID = mParentGroupID
    , _ugName          = T.pack . companyname . companyinfo $ c
    , _ugAddress       = toUserGroupAddress . companyinfo $ c
    , _ugInfo          = toUserGroupInfo . companyinfo $ c
    , _ugInvoicing     = paymentPlanToUserGroupInvoicing (isJust mParentGroupID) .
                          companypaymentplan . companyinfo $ c
    , _ugUI            = toUserGroupUI $ cui
    }

toUserGroupInfo :: CompanyInfo -> UserGroupInfo
toUserGroupInfo ci =
  UserGroupInfo
    {
      _ugiIPAddressMaskList   = companyipaddressmasklist ci
    , _ugiIdleDocTimeout      = companyidledoctimeout ci
    , _ugiCGIDisplayName      = T.pack <$> companycgidisplayname ci
    , _ugiCGIServiceID        = T.pack <$> companycgiserviceid ci
    , _ugiSMSProvider         = companysmsprovider ci
    , _ugiPadAppMode          = companypadappmode ci
    , _ugiPadEarchiveEnabled  = companypadearchiveenabled ci
    }

toUserGroupAddress :: CompanyInfo -> UserGroupAddress
toUserGroupAddress ci =
  UserGroupAddress
    {
      _ugaCompanyNumber = T.pack $ companynumber ci
    , _ugaAddress       = T.pack $ companyaddress ci
    , _ugaZip           = T.pack $ companyzip ci
    , _ugaCity          = T.pack $ companycity ci
    , _ugaCountry       = T.pack $ companycountry ci
    }

toUserGroupUI :: CompanyUI -> UserGroupUI
toUserGroupUI cui =
  UserGroupUI
    {
      _uguiMailTheme     = companyMailTheme cui
    , _uguiSignviewTheme = companySignviewTheme cui
    , _uguiServiceTheme  = companyServiceTheme cui
    , _uguiBrowserTitle  = T.pack <$> companyBrowserTitle cui
    , _uguiSmsOriginator = T.pack <$> companySmsOriginator cui
    , _uguiFavicon       = companyFavicon cui
    }
