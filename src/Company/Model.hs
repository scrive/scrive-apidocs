module Company.Model (
    module Company.CompanyID
  , ExternalCompanyID(..)
  , Company(..)
  , CompanyInfo(..)
  , CompanyUI(..)
  , GetCompanies(..)
  , GetCompany(..)
  , GetCompanyByExternalID(..)
  , CreateCompany(..)
  , SetCompanyInfo(..)
  , UpdateCompanyUI(..)
  , GetOrCreateCompanyWithExternalID(..)
  , GetCompanyByEmailDomain(..)
  , SetCompanyEmailDomain(..)
  ) where

import Data.Typeable

import DB
import DB.SQL2
import API.Service.Model
import Company.CompanyID
import Control.Monad.State

newtype ExternalCompanyID = ExternalCompanyID { unExternalCompanyID :: String }
  deriving (Eq, Ord, Show)
$(newtypeDeriveConvertible ''ExternalCompanyID)

data Company = Company {
    companyid         :: CompanyID
  , companyexternalid :: Maybe ExternalCompanyID
  , companyservice    :: Maybe ServiceID
  , companyinfo       :: CompanyInfo
  , companyui         :: CompanyUI
  } deriving (Eq, Ord, Show, Typeable)

data CompanyInfo = CompanyInfo {
    companyname    :: String
  , companynumber  :: String
  , companyaddress :: String
  , companyzip     :: String
  , companycity    :: String
  , companycountry :: String
  , companyemaildomain :: Maybe String
  } deriving (Eq, Ord, Show)

data CompanyUI = CompanyUI {
    companybarsbackground    :: Maybe String
  , companybarstextcolour    :: Maybe String
  , companylogo              :: Maybe Binary -- File with the logo
} deriving (Eq, Ord, Show)

data GetCompanies = GetCompanies (Maybe ServiceID)
instance MonadDB m => DBQuery m GetCompanies [Company] where
  query (GetCompanies msid) = do
    kRun_ $ sqlSelect "companies" $ do
       selectCompaniesSelectors
       sqlWhere $ SQL "service_id IS NOT DISTINCT FROM ?" [toSql msid]
       sqlOrderBy "id DESC"
    fetchCompanies

data GetCompany = GetCompany CompanyID
instance MonadDB m => DBQuery m GetCompany (Maybe Company) where
  query (GetCompany cid) = do
    kRun_ $ sqlSelect "companies" $ do
      selectCompaniesSelectors
      sqlWhereEq "id" cid
    fetchCompanies >>= oneObjectReturnedGuard

data GetCompanyByExternalID = GetCompanyByExternalID (Maybe ServiceID) ExternalCompanyID
instance MonadDB m => DBQuery m GetCompanyByExternalID (Maybe Company) where
  query (GetCompanyByExternalID msid ecid) = do
    kRun_ $ sqlSelect "companies" $ do
      selectCompaniesSelectors
      sqlWhere $ SQL "service_id IS NOT DISTINCT FROM ?" [toSql msid]
      sqlWhereEq "external_id" ecid
    fetchCompanies >>= oneObjectReturnedGuard

data CreateCompany = CreateCompany (Maybe ServiceID) (Maybe ExternalCompanyID)
instance MonadDB m => DBUpdate m CreateCompany Company where
  update (CreateCompany msid mecid) = do
    kRun_ $ sqlInsert "companies" $ do
      sqlSet "external_id" mecid
      sqlSet "service_id" msid
      selectCompaniesSelectors
    fetchCompanies >>= exactlyOneObjectReturnedGuard

data SetCompanyInfo = SetCompanyInfo CompanyID CompanyInfo
instance MonadDB m => DBUpdate m SetCompanyInfo Bool where
  update (SetCompanyInfo cid CompanyInfo{..}) =
    kRun01 $ sqlUpdate "companies" $ do
      sqlSet "name" companyname
      sqlSet "number" companynumber
      sqlSet "address" companyaddress
      sqlSet "zip" companyzip
      sqlSet "city" companycity
      sqlSet "country" companycountry
      sqlSet "email_domain" companyemaildomain
      sqlWhereEq "id" cid

data UpdateCompanyUI = UpdateCompanyUI CompanyID CompanyUI
instance MonadDB m => DBUpdate m UpdateCompanyUI Bool where
  update (UpdateCompanyUI cid cui) = do
    kRun01 $ sqlUpdate "companies" $ do
      sqlSet "bars_background" $ companybarsbackground cui
      sqlSet "bars_textcolour" $ companybarstextcolour cui
      sqlSetCmd "logo" $ SQL "decode(?, 'base64')" [toSql $ companylogo cui]
      sqlWhereEq "id" cid

data GetOrCreateCompanyWithExternalID = GetOrCreateCompanyWithExternalID (Maybe ServiceID) ExternalCompanyID
instance MonadDB m => DBUpdate m GetOrCreateCompanyWithExternalID Company where
  update (GetOrCreateCompanyWithExternalID msid ecid) = do
    mc <- query $ GetCompanyByExternalID msid ecid
    case mc of
      Just c  -> return c
      Nothing -> update $ CreateCompany msid $ Just ecid

data SetCompanyEmailDomain = SetCompanyEmailDomain CompanyID (Maybe String)
instance MonadDB m => DBUpdate m SetCompanyEmailDomain Bool where
  update (SetCompanyEmailDomain cid mdomain) = do
    kRun01 $ sqlUpdate "companies" $ do
      sqlSet "email_domain" mdomain
      sqlWhereEq "id" cid
      sqlWhere $ SQL "NOT EXISTS (SELECT 1 FROM companies WHERE email_domain = ?)" [toSql mdomain]

data GetCompanyByEmailDomain = GetCompanyByEmailDomain String
instance MonadDB m => DBQuery m GetCompanyByEmailDomain (Maybe Company) where
  query (GetCompanyByEmailDomain domain) = do
    kRun_ $ sqlSelect "companies" $ do
      selectCompaniesSelectors
      sqlWhereEq "email_domain" domain
    fetchCompanies >>= oneObjectReturnedGuard

-- helpers

selectCompaniesSelectors :: (SqlResult command) => State command ()
selectCompaniesSelectors = do
  sqlResult "id"
  sqlResult "external_id"
  sqlResult "service_id"
  sqlResult "name"
  sqlResult "number"
  sqlResult "address"
  sqlResult "zip"
  sqlResult "city"
  sqlResult "country"
  sqlResult "bars_background"
  sqlResult "bars_textcolour"
  sqlResult "encode(logo, 'base64') AS logo_base64"
  sqlResult "email_domain"


fetchCompanies :: MonadDB m => DBEnv m [Company]
fetchCompanies = foldDB decoder []
  where
    decoder acc cid eid sid name number address zip' city country
      bars_background bars_textcolour logo email_domain = Company {
        companyid = cid
      , companyexternalid = eid
      , companyservice = sid
      , companyinfo = CompanyInfo {
          companyname = name
        , companynumber = number
        , companyaddress = address
        , companyzip = zip'
        , companycity = city
        , companycountry = country
        , companyemaildomain = email_domain
        }
      , companyui = CompanyUI {
          companybarsbackground = bars_background
        , companybarstextcolour = bars_textcolour
        , companylogo = logo
        }
      } : acc
