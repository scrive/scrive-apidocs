module Company.Model (
    module Company.CompanyID
  , module Company.Data
  , Company(..)
  , CompanyInfo(..)
  , minCompanyIdleDocTimeout
  , maxCompanyIdleDocTimeout
  , GetCompanies(..)
  , GetCompaniesByPartnerID(..)
  , GetCompaniesWithIdleDocTimeoutSet(..)
  , GetCompany(..)
  , GetCompanyByUserID(..)
  , CreateCompany(..)
  , CreateCompanyWithoutUserGroup(..)
  , SetCompanyInfo(..)
  , SetCompanyPaymentPlan(..)
  , CompanyFilter(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Int (Int16)
import Log
import qualified Data.Text as T

import Company.CompanyID
import Company.Data
import DB
import Log.Identifier
import PadApplication.Data
import Partner.Model
import SMS.Data (SMSProvider(..))
import User.UserID
import UserGroup.Data
import UserGroup.Model

-- Synchronize these definitions with frontend/app/js/account/company.js
minCompanyIdleDocTimeout, maxCompanyIdleDocTimeout :: Int16
minCompanyIdleDocTimeout = 1
maxCompanyIdleDocTimeout = 365

data CompanyFilter
  =   CompanyFilterByString String             -- ^ Contains the string anywhere
    | CompanyManyUsers             -- ^ Has more users then given number
    | CompanyWithNonFreePricePlan  -- ^ has a non-free price plan attached

companyFilterToWhereClause :: (SqlWhere command) => CompanyFilter -> State command ()
companyFilterToWhereClause (CompanyFilterByString text) = do
  -- ALL words from 'text' are in ANY of the fields
  mapM_ (sqlWhere . parenthesize . findWord) (words text)
  where
      findWordInField word fieldName = ("companies." <> fieldName) <+> "ILIKE" <?> sqlwordpat word
      findWordList word = map (findWordInField word) ["name", "number", "address", "zip", "city", "country"]
      findWord word = sqlConcatOR $ findWordList word
      sqlwordpat word = "%" ++ concatMap escape word ++ "%"
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]

companyFilterToWhereClause (CompanyManyUsers) = do
  sqlWhereAny
    [ sqlWhere $ "((SELECT count(*) FROM users WHERE users.company_id = companies.id AND users.deleted IS NULL) > 1)"
    , sqlWhere $ "((SELECT count(*) FROM companyinvites WHERE companyinvites.company_id = companies.id) > 0)"
    ]

companyFilterToWhereClause (CompanyWithNonFreePricePlan) = do
  sqlWhere $ "(payment_plan != "<?> FreePlan <> ")"

data GetCompanies = GetCompanies [CompanyFilter] Integer Integer
instance MonadDB m => DBQuery m GetCompanies [Company] where
  query (GetCompanies filters offset limit) = do
    runQuery_ $ sqlSelect "companies" $ do
       sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
       selectCompaniesSelectors
       mapM_ companyFilterToWhereClause filters
       sqlOffset offset
       sqlLimit limit
       sqlOrderBy "companies.id"
    fetchMany fetchCompany

data GetCompany = GetCompany CompanyID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompany (Maybe Company) where
  query (GetCompany cid) = do
    runQuery_ $ sqlSelect "companies" $ do
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "id" cid
    fetchMaybe fetchCompany

data GetCompanyByUserID = GetCompanyByUserID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompanyByUserID Company where
  query (GetCompanyByUserID uid) = do
    runQuery_ $ sqlSelect "companies" $ do
      sqlJoinOn "users" "users.company_id = companies.id"
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "users.id" uid
    fetchOne fetchCompany

data GetCompaniesByPartnerID = GetCompaniesByPartnerID PartnerID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompaniesByPartnerID [Company] where
  query (GetCompaniesByPartnerID partnerID) = do
    runQuery_ $ sqlSelect "companies" $ do
      selectCompaniesSelectors
      sqlWhereEq "partner_id" partnerID
    fetchMany fetchCompany

data GetCompaniesWithIdleDocTimeoutSet = GetCompaniesWithIdleDocTimeoutSet
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompaniesWithIdleDocTimeoutSet [Company] where
  query GetCompaniesWithIdleDocTimeoutSet = do
    runQuery_ $ sqlSelect "companies" $ do
      selectCompaniesSelectors
      sqlWhereIsNotNULL "idle_doc_timeout"
    fetchMany fetchCompany

data CreateCompanyWithoutUserGroup = CreateCompanyWithoutUserGroup
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m CreateCompanyWithoutUserGroup Company where
  update (CreateCompanyWithoutUserGroup) = do
    runQuery_ $ sqlInsert "companies" $ do
      sqlSetCmd "id" "DEFAULT"
      sqlSetCmd "partner_id" "(SELECT partners.id FROM partners WHERE partners.default_partner LIMIT 1)"
      sqlResult "id"
    companyidx :: CompanyID <- fetchOne runIdentity
    runQuery_ $ sqlInsert "company_uis" $ do
      sqlSet "company_id" companyidx
    runQuery_ $ sqlInsert "feature_flags" $ do
      sqlSet "company_id" companyidx
      sqlSet "can_use_dk_authentication_to_view" False
      sqlSet "can_use_dk_authentication_to_sign" False
      sqlSet "can_use_no_authentication_to_view" False
      sqlSet "can_use_no_authentication_to_sign" False
      sqlSet "can_use_se_authentication_to_view" False
      sqlSet "can_use_se_authentication_to_sign" False
    runQuery_ $ sqlSelect "companies" $ do
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "companies.id" companyidx
    fetchOne fetchCompany

data CreateCompany = CreateCompany
instance (MonadDB m, MonadThrow m, MonadMask m, MonadLog m) => DBUpdate m CreateCompany Company where
  update (CreateCompany) = do
    company0 <- dbUpdate $ CreateCompanyWithoutUserGroup
    result <- companyMigrateForDefaultPartner $ companyid company0
    when (not result) $
      logAttention "UserGroup cannot migrate a newly created company" $ object [
          identifier_ $ companyid company0
        ]
    -- re-read company, because the migration changed it
    fromJust <$> dbQuery (GetCompany $ companyid company0)

data SetCompanyPaymentPlan = SetCompanyPaymentPlan CompanyID PaymentPlan
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m SetCompanyPaymentPlan Bool where
  update (SetCompanyPaymentPlan cid pp) = do
    result <- runQuery01 . sqlUpdate "companies" $ do
      sqlSet "payment_plan" $ pp
      sqlWhereEq "id" cid
    when result $ do
      dbQuery (GetCompany cid) >>= \case
        Nothing ->
          logAttention "SetCompanyPaymentPlan company does not exist" $ object [
              identifier_ cid
            ]
        Just company -> do
          p <- dbQuery . GetPartnerByID . companypartnerid . companyinfo $ company
          -- The motivation is, that in future the partner will get only 1 invoice with all
          -- the billed companies. Until then, our current invoicing code will just grab the
          -- `PaymentPlan`, so there will be no difference between `Invoice pp` and `BillItem $ Just pp`.
          let inv = case ptDefaultPartner p of
                True  -> Invoice pp
                False -> BillItem (Just pp)
          modifyCompanyUserGroup company $ set ugInvoicing inv
    return result

data SetCompanyInfo = SetCompanyInfo CompanyID CompanyInfo
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m SetCompanyInfo Bool where
  update (SetCompanyInfo cid CompanyInfo{..}) = do
    result <- runQuery01 . sqlUpdate "companies" $ do
      sqlSet "name" companyname
      sqlSet "number" companynumber
      sqlSet "address" companyaddress
      sqlSet "zip" companyzip
      sqlSet "city" companycity
      sqlSet "country" companycountry
      sqlSet "ip_address_mask_list" $ case companyipaddressmasklist of
                                        [] -> Nothing
                                        x -> Just (show x)
      sqlSet "idle_doc_timeout" companyidledoctimeout
      sqlSet "cgi_display_name" companycgidisplayname
      sqlSet "sms_provider" companysmsprovider
      sqlSet "cgi_service_id" companycgiserviceid
      -- !We don't update payment plan here - there is other function for that
      sqlSet "partner_id" companypartnerid
      sqlSet "pad_app_mode" companypadappmode
      sqlSet "pad_earchive_enabled" companypadearchiveenabled
      sqlWhereEq "id" cid
    when result $ dbQuery (GetCompany cid) >>= \case
      Nothing ->
        logAttention "SetCompanyInfo company does not exist" $ object [
            identifier_ cid
          ]
      Just (company@Company{..}) -> do
        Partner{..} <- dbQuery . GetPartnerByID $ companypartnerid
        let company_and_partner_have_same_status_of_usergroup_migration =
                isJust    companyusergroupid && isJust    ptUserGroupID
             || isNothing companyusergroupid && isNothing ptUserGroupID
        case (  company_and_partner_have_same_status_of_usergroup_migration
             -- Default partner will not be migrated to user_groups, so in this case
             -- the migration status of usergroups can be different.
             || ptDefaultPartner) of
          False -> logAttention "UserGroup inconsistent partner migration" $ object [
              identifier_ cid
            , identifier_ ptID
            ]
          True -> modifyCompanyUserGroup company $
              set ugName          (T.pack companyname)
            -- We can do this for default partner, because that does not have
            -- a usergroup associated.
            . set ugParentGroupID ptUserGroupID
            . set ugInfo
                  (UserGroupInfo {
                      _ugiIPAddressMaskList   = companyipaddressmasklist
                    , _ugiIdleDocTimeout      = companyidledoctimeout
                    , _ugiCGIDisplayName      = T.pack <$> companycgidisplayname
                    , _ugiCGIServiceID        = T.pack <$> companycgiserviceid
                    , _ugiSMSProvider         = companysmsprovider
                    , _ugiPadAppMode          = companypadappmode
                    , _ugiPadEarchiveEnabled  = companypadearchiveenabled
                    })
            . set ugAddress
                  (UserGroupAddress {
                      _ugaCompanyNumber = T.pack companynumber
                    , _ugaAddress       = T.pack companyaddress
                    , _ugaZip           = T.pack companyzip
                    , _ugaCity          = T.pack companycity
                    , _ugaCountry       = T.pack companycountry
                    })
    return result

-- helpers

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

modifyCompanyUserGroup
  :: (MonadDB m, MonadThrow m, MonadLog m)
  => Company -> (UserGroup -> UserGroup) -> m ()
modifyCompanyUserGroup company modify_ug =
  case companyusergroupid company of
    Nothing -> return () -- This company was not migrated to user_groups yet. This is just fine.
    Just ugid -> dbQuery (UserGroupGet ugid) >>= \case
      Nothing ->
        logAttention "UserGroup inconsistent ForeignKey" $ object [
            identifier_ ugid
          , identifier_ $ companyid company
          ]
      Just ug -> dbUpdate . UserGroupUpdate . modify_ug $ ug
