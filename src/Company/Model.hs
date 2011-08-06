module Company.Model (
    CompanyID(..)
  , ExternalCompanyID(..)
  , Company(..)
  , CompanyInfo(..)
  , GetCompanies(..)
  , GetCompany(..)
  , GetCompanyByExternalID(..)
  , CreateCompany(..)
  , SetCompanyInfo(..)
  , GetOrCreateCompanyWithExternalID(..)
  ) where

import Control.Applicative
import Data.Int
import Database.HDBC
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU

import DB.Classes
import DB.Derive
import DB.Utils
import API.Service.Model

newtype CompanyID = CompanyID { unCompanyID :: Int64 }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''CompanyID)
$(newtypeDeriveUnderlyingReadShow ''CompanyID)

newtype ExternalCompanyID = ExternalCompanyID { unExternalCompanyID :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''ExternalCompanyID)

instance Show ExternalCompanyID where
  show (ExternalCompanyID val) = BSU.toString val
instance Read ExternalCompanyID where
  readsPrec _ s = [(ExternalCompanyID $ BSU.fromString s, "")]

data Company = Company {
    companyid         :: CompanyID
  , companyexternalid :: Maybe ExternalCompanyID
  , companyservice    :: Maybe ServiceID
  , companyinfo       :: CompanyInfo
  } deriving (Eq, Ord, Show)

data CompanyInfo = CompanyInfo {
    companyname    :: BS.ByteString
  , companynumber  :: BS.ByteString
  , companyaddress :: BS.ByteString
  , companyzip     :: BS.ByteString
  , companycity    :: BS.ByteString
  , companycountry :: BS.ByteString
  } deriving (Eq, Ord, Show)

data GetCompanies = GetCompanies (Maybe ServiceID)
instance DBQuery GetCompanies [Company] where
  dbQuery (GetCompanies msid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompaniesSQL
      ++ "WHERE (?::TEXT IS NULL AND c.service_id IS NULL) OR c.service_id = ? ORDER BY c.id DESC"
    _ <- execute st [toSql msid, toSql msid]
    fetchCompanies st []

data GetCompany = GetCompany CompanyID
instance DBQuery GetCompany Company where
  dbQuery (GetCompany cid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompaniesSQL ++ "WHERE c.id = ?"
    _ <- execute st [toSql cid]
    cs <- fetchCompanies st []
    oneObjectReturnedGuard cs

data GetCompanyByExternalID = GetCompanyByExternalID (Maybe ServiceID) ExternalCompanyID
instance DBQuery GetCompanyByExternalID Company where
  dbQuery (GetCompanyByExternalID msid ecid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompaniesSQL
      ++ "WHERE ((?::TEXT IS NULL AND c.service_id IS NULL) OR c.service_id = ?) AND c.external_id = ?"
    _ <- execute st [toSql msid, toSql msid, toSql ecid]
    cs <- fetchCompanies st []
    oneObjectReturnedGuard cs

data CreateCompany = CreateCompany (Maybe ServiceID) (Maybe ExternalCompanyID)
instance DBUpdate CreateCompany Company where
  dbUpdate (CreateCompany msid mecid) = do
    cid <- CompanyID <$> getUniqueID "companies"
    wrapDB $ \conn -> do
      _ <- run conn ("INSERT INTO companies ("
        ++ "  id"
        ++ ", external_id"
        ++ ", service_id) VALUES (?, ?, ?)") [
            toSql cid
          , toSql mecid
          , toSql msid
          ]
      _ <- run conn ("INSERT INTO company_infos ("
        ++ "  company_id"
        ++ ", name"
        ++ ", number"
        ++ ", address"
        ++ ", zip"
        ++ ", city"
        ++ ", country) VALUES (?, ?, ?, ?, ?, ?, ?)")
          $ toSql cid : replicate 6 (toSql "")
      return ()
    dbQuery $ GetCompany cid

data SetCompanyInfo = SetCompanyInfo CompanyID CompanyInfo
instance DBUpdate SetCompanyInfo () where
  dbUpdate (SetCompanyInfo cid ci) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE company_infos SET"
      ++ "  name = ?"
      ++ ", number = ?"
      ++ ", address = ?"
      ++ ", zip = ?"
      ++ ", city = ?"
      ++ ", country = ?"
      ++ "  WHERE company_id = ?") [
        toSql $ companyname ci
      , toSql $ companynumber ci
      , toSql $ companyaddress ci
      , toSql $ companyzip ci
      , toSql $ companycity ci
      , toSql $ companycountry ci
      , toSql cid
      ]
    oneRowAffectedGuard r

data GetOrCreateCompanyWithExternalID = GetOrCreateCompanyWithExternalID (Maybe ServiceID) ExternalCompanyID
instance DBUpdate GetOrCreateCompanyWithExternalID Company where
  dbUpdate (GetOrCreateCompanyWithExternalID msid ecid) =
    dbQuery (GetCompanyByExternalID msid ecid) `catchDB` handle
    where
      handle e = case e of
        NoObject -> dbUpdate $ CreateCompany msid $ Just ecid
        _ -> E.throw e

-- helpers

selectCompaniesSQL :: String
selectCompaniesSQL = "SELECT c.id, c.external_id, c.service_id, ci.name, ci.number, ci.address, ci.zip, ci.city, ci.country FROM companies c JOIN company_infos ci ON (c.id = ci.company_id) "

fetchCompanies :: Statement -> [Company] -> IO [Company]
fetchCompanies st acc = fetchRow st >>= maybe (return acc)
  (\[cid, eid, sid, name, number, address, zip', city, country
   ] -> fetchCompanies st $ Company {
       companyid = fromSql cid
     , companyexternalid = fromSql eid
     , companyservice = fromSql sid
     , companyinfo = CompanyInfo {
         companyname = fromSql name
       , companynumber = fromSql number
       , companyaddress = fromSql address
       , companyzip = fromSql zip'
       , companycity = fromSql city
       , companycountry = fromSql country
     }
   } : acc)
