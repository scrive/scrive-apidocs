module Company.Model (
    module Company.CompanyID
  , module Company.Data
  , Company(..)
  , CompanyInfo(..)
  , GetCompany(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Int (Int16)

import Company.CompanyID
import Company.Data
import DB
import PadApplication.Data
import Partner.Model
import SMS.Data (SMSProvider(..))
import UserGroup.Data

data GetCompany = GetCompany CompanyID
instance (MonadDB m, MonadThrow m) => DBQuery m GetCompany (Maybe Company) where
  query (GetCompany cid) = do
    runQuery_ $ sqlSelect "companies" $ do
      sqlJoinOn "company_uis" "company_uis.company_id = companies.id"
      selectCompaniesSelectors
      sqlWhereEq "id" cid
    fetchMaybe fetchCompany

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
                 Bool, UserGroupID) -> Company
fetchCompany (cid, name, number, address, zip', city, country,
              ip_address_mask_list, idle_doc_timeout, cgi_display_name,
              sms_provider, cgi_service_id, payment_plan,  partner_id, pad_app_mode,
              pad_earchive_enabled, user_group_id) = Company {
  companyid = cid
, companyusergroupid = user_group_id
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
