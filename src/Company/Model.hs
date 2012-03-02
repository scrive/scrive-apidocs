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
  ) where

import Data.Monoid
import Database.HDBC
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import DB.Classes
import DB.Derive
import DB.Fetcher2
import DB.Types
import DB.Utils
import API.Service.Model
import Company.CompanyID
import Company.Tables
import Misc

newtype ExternalCompanyID = ExternalCompanyID { unExternalCompanyID :: BS.ByteString }
  deriving (Eq, Ord, Show)
$(newtypeDeriveConvertible ''ExternalCompanyID)

data Company = Company {
    companyid         :: CompanyID
  , companyexternalid :: Maybe ExternalCompanyID
  , companyservice    :: Maybe ServiceID
  , companyinfo       :: CompanyInfo
  , companyui         :: CompanyUI
  } deriving (Eq, Ord, Show)

data CompanyInfo = CompanyInfo {
    companyname    :: BS.ByteString
  , companynumber  :: BS.ByteString
  , companyaddress :: BS.ByteString
  , companyzip     :: BS.ByteString
  , companycity    :: BS.ByteString
  , companycountry :: BS.ByteString
  } deriving (Eq, Ord, Show)

data CompanyUI = CompanyUI {
    companybarsbackground    :: Maybe BS.ByteString
  , companybarstextcolour    :: Maybe BS.ByteString
  , companylogo              :: Maybe Binary -- File with the logo
} deriving (Eq, Ord, Show)

data GetCompanies = GetCompanies (Maybe ServiceID)
instance DBQuery GetCompanies [Company] where
  dbQuery (GetCompanies msid) = do
    _ <- kRun $ selectCompaniesSQL <++> SQL "WHERE service_id IS NOT DISTINCT FROM ? ORDER BY id DESC" [toSql msid]
    fetchCompanies

data GetCompany = GetCompany CompanyID
instance DBQuery GetCompany (Maybe Company) where
  dbQuery (GetCompany cid) = do
    _ <- kRun $ selectCompaniesSQL <++> SQL "WHERE id = ?" [toSql cid]
    fetchCompanies >>= oneObjectReturnedGuard

data GetCompanyByExternalID = GetCompanyByExternalID (Maybe ServiceID) ExternalCompanyID
instance DBQuery GetCompanyByExternalID (Maybe Company) where
  dbQuery (GetCompanyByExternalID msid ecid) = do
    _ <- kRun $ selectCompaniesSQL <++> SQL "WHERE service_id IS NOT DISTINCT FROM ? AND external_id = ?" [toSql msid, toSql ecid]
    fetchCompanies >>= oneObjectReturnedGuard

data CreateCompany = CreateCompany (Maybe ServiceID) (Maybe ExternalCompanyID)
instance DBUpdate CreateCompany Company where
  dbUpdate (CreateCompany msid mecid) = do
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
    fetchCompanies >>= oneObjectReturnedGuard >>= maybe (E.throw $ NoObject mempty) return

data SetCompanyInfo = SetCompanyInfo CompanyID CompanyInfo
instance DBUpdate SetCompanyInfo Bool where
  dbUpdate (SetCompanyInfo cid CompanyInfo{..}) =
    kRun01 $ mkSQL UPDATE tableCompanies [
        sql "name" companyname
      , sql "number" companynumber
      , sql "address" companyaddress
      , sql "zip" companyzip
      , sql "city" companycity
      , sql "country" companycountry
      ] <++> SQL "WHERE id = ?" [toSql cid]

data UpdateCompanyUI = UpdateCompanyUI CompanyID CompanyUI
instance DBUpdate UpdateCompanyUI Bool where
  dbUpdate (UpdateCompanyUI cid cui) = do
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
instance DBUpdate GetOrCreateCompanyWithExternalID Company where
  dbUpdate (GetOrCreateCompanyWithExternalID msid ecid) = do
    mc <- dbQuery $ GetCompanyByExternalID msid ecid
    case mc of
      Just c  -> return c
      Nothing -> dbUpdate $ CreateCompany msid $ Just ecid

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

selectCompaniesSQL :: SQL
selectCompaniesSQL = SQL ("SELECT " ++ selectCompaniesSelectors ++ " FROM companies ") []

fetchCompanies :: DB [Company]
fetchCompanies = foldDB decoder []
  where
    decoder acc cid eid sid name number address zip' city country
      bars_background bars_textcolour logo = Company {
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
        }
      , companyui = CompanyUI {
          companybarsbackground = bars_background
        , companybarstextcolour = bars_textcolour
        , companylogo = logo
        }
      } : acc
