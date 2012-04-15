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
import API.Service.Model
import Company.CompanyID
import Company.Tables
import Misc

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
    _ <- kRun $ selectCompaniesSQL <++> SQL "WHERE service_id IS NOT DISTINCT FROM ? ORDER BY id DESC" [toSql msid]
    fetchCompanies

data GetCompany = GetCompany CompanyID
instance MonadDB m => DBQuery m GetCompany (Maybe Company) where
  query (GetCompany cid) = do
    _ <- kRun $ selectCompaniesSQL <++> SQL "WHERE id = ?" [toSql cid]
    fetchCompanies >>= oneObjectReturnedGuard

data GetCompanyByExternalID = GetCompanyByExternalID (Maybe ServiceID) ExternalCompanyID
instance MonadDB m => DBQuery m GetCompanyByExternalID (Maybe Company) where
  query (GetCompanyByExternalID msid ecid) = do
    _ <- kRun $ selectCompaniesSQL <++> SQL "WHERE service_id IS NOT DISTINCT FROM ? AND external_id = ?" [toSql msid, toSql ecid]
    fetchCompanies >>= oneObjectReturnedGuard

data CreateCompany = CreateCompany (Maybe ServiceID) (Maybe ExternalCompanyID)
instance MonadDB m => DBUpdate m CreateCompany Company where
  update (CreateCompany msid mecid) = do
    _ <- kRun $ mkSQL INSERT tableCompanies
      [ sql "external_id" mecid
      , sql "service_id" msid
      , sql "name" ""
      , sql "number" ""
      , sql "address" ""
      , sql "zip" ""
      , sql "city" ""
      , sql "country" ""
      ] <++> SQL (" RETURNING " ++ selectCompaniesSelectors) []
    fetchCompanies >>= exactlyOneObjectReturnedGuard

data SetCompanyInfo = SetCompanyInfo CompanyID CompanyInfo
instance MonadDB m => DBUpdate m SetCompanyInfo Bool where
  update (SetCompanyInfo cid CompanyInfo{..}) =
    kRun01 $ mkSQL UPDATE tableCompanies [
        sql "name" companyname
      , sql "number" companynumber
      , sql "address" companyaddress
      , sql "zip" companyzip
      , sql "city" companycity
      , sql "country" companycountry
      , sql "email_domain" companyemaildomain
      ] <++> SQL "WHERE id = ?" [toSql cid]

data UpdateCompanyUI = UpdateCompanyUI CompanyID CompanyUI
instance MonadDB m => DBUpdate m UpdateCompanyUI Bool where
  update (UpdateCompanyUI cid cui) = do
    kPrepare $ "UPDATE companies SET"
      ++ "  bars_background = ?"
      ++ ", bars_textcolour = ?"
      ++ ", logo = decode(?, 'base64')"
      ++ "  WHERE id = ?"
    kExecute01 [
        toSql $ companybarsbackground cui
      , toSql $ companybarstextcolour cui
      , toSql $ companylogo cui
      , toSql cid
      ]

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
    kPrepare $ "UPDATE companies SET email_domain = ? WHERE id = ? AND NOT EXISTS (SELECT 1 FROM companies WHERE email_domain = ?)"
    kExecute01 [toSql mdomain, toSql cid, toSql mdomain]

data GetCompanyByEmailDomain = GetCompanyByEmailDomain String
instance MonadDB m => DBQuery m GetCompanyByEmailDomain (Maybe Company) where
  query (GetCompanyByEmailDomain domain) = do
    _ <- kRun $ selectCompaniesSQL <++> SQL "WHERE email_domain = ?" [toSql domain]
    fetchCompanies >>= oneObjectReturnedGuard

-- helpers

selectCompaniesSelectors :: String
selectCompaniesSelectors = "id"
  ++ ", external_id"
  ++ ", service_id"
  ++ ", name"
  ++ ", number"
  ++ ", address"
  ++ ", zip"
  ++ ", city"
  ++ ", country"
  ++ ", bars_background"
  ++ ", bars_textcolour"
  ++ ", encode(logo, 'base64')"
  ++ ", email_domain"

selectCompaniesSQL :: SQL
selectCompaniesSQL = SQL ("SELECT " ++ selectCompaniesSelectors ++ " FROM companies ") []

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
