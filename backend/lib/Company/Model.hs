module Company.Model (
    module Company.CompanyID
  , PaymentPlan(..)
  , Company(..)
  , CompanyInfo(..)
  , minCompanyIdleDocTimeout
  , maxCompanyIdleDocTimeout
  , GetCompanies(..)
  , GetCompaniesByPartnerID(..)
  , GetCompany(..)
  , GetCompanyByUserID(..)
  , CreateCompany(..)
  , SetCompanyInfo(..)
  , SetCompanyIPAddressMaskList(..)
  , SetCompanyPaymentPlan(..)
  , CompanyFilter(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Default
import Data.Int (Int16)
import Data.Typeable

import Company.CompanyID
import DB
import IPAddress
import KontraPrelude
import PadApplication.Data
import Partner.Model
import SMS.Data (SMSProvider(..))
import User.UserID

data PaymentPlan =
    FreePlan
  | OnePlan
  | TeamPlan
  | EnterprisePlan
  | TrialPlan
  deriving (Eq, Ord, Show)

instance PQFormat PaymentPlan where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL PaymentPlan where
  type PQBase PaymentPlan = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      -- Note:
      -- If changing this, please also update `pure_sql/invoice_stat.sql`
      0 -> return FreePlan
      1 -> return OnePlan
      2 -> return TeamPlan
      3 -> return EnterprisePlan
      4 -> return TrialPlan
      _ -> throwM RangeError {
        reRange = [(0, 4)]
      , reValue = n
      }

instance ToSQL PaymentPlan where
  type PQDest PaymentPlan = PQDest Int16
  toSQL FreePlan        = toSQL (0::Int16)
  toSQL OnePlan         = toSQL (1::Int16)
  toSQL TeamPlan        = toSQL (2::Int16)
  toSQL EnterprisePlan  = toSQL (3::Int16)
  toSQL TrialPlan       = toSQL (4::Int16)

data Company = Company {
    companyid         :: CompanyID
  , companyinfo       :: CompanyInfo
  } deriving (Eq, Ord, Show, Typeable)

data CompanyInfo = CompanyInfo {
    companyname    :: String
  , companynumber  :: String
  , companyaddress :: String
  , companyzip     :: String
  , companycity    :: String
  , companycountry :: String
  , companyipaddressmasklist :: [IPAddressWithMask]
  , companyallowsavesafetycopy :: Bool
  , companyidledoctimeout :: Maybe Int16
  , companycgidisplayname :: Maybe String
  , companysmsprovider    :: SMSProvider
  , companycgiserviceid   :: Maybe String
  , companypaymentplan    :: PaymentPlan
  , companypartnerid      :: PartnerID
  , companypadappmode     :: PadAppMode
  , companypadearchiveenabled :: Bool
  } deriving (Eq, Ord, Show)

-- @devnote will be no partner ID '0' so we'll remove this in the API endpoints when we
-- encounter it (i.e. when not given explicitly by the API client)
instance Default CompanyInfo where
    def = CompanyInfo
          { companyname                = ""
          , companynumber              = ""
          , companyaddress             = ""
          , companyzip                 = ""
          , companycity                = ""
          , companycountry             = ""
          , companyipaddressmasklist   = []
          , companyallowsavesafetycopy = True
          , companyidledoctimeout      = Nothing
          , companycgidisplayname      = Nothing
          , companysmsprovider         = SMSDefault
          , companycgiserviceid        = Nothing
          , companypaymentplan         = FreePlan
          , companypartnerid           = unsafePartnerID 0
          , companypadappmode          = ListView
          , companypadearchiveenabled  = True
          }

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

data CreateCompany = CreateCompany
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateCompany Company where
  update (CreateCompany) = do
    runQuery_ $ sqlInsert "companies" $ do
      sqlSetCmd "id" "DEFAULT"
      sqlSetCmd "partner_id" "(SELECT partners.id FROM partners WHERE partners.default_partner LIMIT 1)"
      sqlResult "id"
    companyidx :: CompanyID <- fetchOne runIdentity
    runQuery_ $ sqlInsert "company_uis" $ do
      sqlSet "company_id" companyidx

    runQuery_ $ sqlSelect "companies" $ do
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "companies.id" companyidx
    fetchOne fetchCompany

data SetCompanyIPAddressMaskList = SetCompanyIPAddressMaskList CompanyID [IPAddress]
instance (MonadDB m, MonadThrow m) => DBUpdate m SetCompanyIPAddressMaskList Bool where
  update (SetCompanyIPAddressMaskList cid ads) =
    runQuery01 . sqlUpdate "companies" $ do
      sqlSet "ip_address_mask" $ show ads
      sqlWhereEq "id" cid

data SetCompanyPaymentPlan = SetCompanyPaymentPlan CompanyID PaymentPlan
instance (MonadDB m, MonadThrow m) => DBUpdate m SetCompanyPaymentPlan Bool where
  update (SetCompanyPaymentPlan cid pp) =
    runQuery01 . sqlUpdate "companies" $ do
      sqlSet "payment_plan" $ pp
      sqlWhereEq "id" cid

data SetCompanyInfo = SetCompanyInfo CompanyID CompanyInfo
instance (MonadDB m, MonadThrow m) => DBUpdate m SetCompanyInfo Bool where
  update (SetCompanyInfo cid CompanyInfo{..}) =
    runQuery01 $ sqlUpdate "companies" $ do
      sqlSet "name" companyname
      sqlSet "number" companynumber
      sqlSet "address" companyaddress
      sqlSet "zip" companyzip
      sqlSet "city" companycity
      sqlSet "country" companycountry
      sqlSet "ip_address_mask_list" $ case companyipaddressmasklist of
                                        [] -> Nothing
                                        x -> Just (show x)
      sqlSet "allow_save_safety_copy" companyallowsavesafetycopy
      sqlSet "idle_doc_timeout" companyidledoctimeout
      sqlSet "cgi_display_name" companycgidisplayname
      sqlSet "sms_provider" companysmsprovider
      sqlSet "cgi_service_id" companycgiserviceid
      -- !We don't update payment plan here - there is other function for that
      sqlSet "partner_id" companypartnerid
      sqlSet "pad_app_mode" companypadappmode
      sqlSet "pad_earchive_enabled" companypadearchiveenabled
      sqlWhereEq "id" cid

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
  sqlResult "companies.allow_save_safety_copy"
  sqlResult "companies.idle_doc_timeout"
  sqlResult "companies.cgi_display_name"
  sqlResult "companies.sms_provider"
  sqlResult "companies.cgi_service_id"
  sqlResult "companies.payment_plan"
  sqlResult "companies.partner_id"
  sqlResult "companies.pad_app_mode"
  sqlResult "companies.pad_earchive_enabled"

fetchCompany :: (CompanyID, String, String, String, String, String, String,
                 Maybe String, Bool, Maybe Int16, Maybe String,
                 SMSProvider, Maybe String, PaymentPlan, PartnerID, PadAppMode, Bool) -> Company
fetchCompany (cid, name, number, address, zip', city, country,
              ip_address_mask_list, allow_save_safety_copy, idle_doc_timeout, cgi_display_name,
              sms_provider, cgi_service_id, payment_plan,  partner_id, pad_app_mode, pad_earchive_enabled) = Company {
  companyid = cid
, companyinfo = CompanyInfo {
    companyname = name
  , companynumber = number
  , companyaddress = address
  , companyzip = zip'
  , companycity = city
  , companycountry = country
  , companyipaddressmasklist = maybe [] read ip_address_mask_list
  , companyallowsavesafetycopy = allow_save_safety_copy
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
