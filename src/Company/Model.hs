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
import Data.Data
import Data.Int
import Database.HDBC
import Happstack.State
import Happstack.Server
import Happstack.Util.Common
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import DB.Classes
import DB.Derive
import DB.Fetcher2
import DB.Utils
import API.Service.Model
import Company.Tables

newtype CompanyID = CompanyID { unCompanyID :: Int64 }
  deriving (Eq, Ord, Data, Typeable)
$(newtypeDeriveConvertible ''CompanyID)
$(newtypeDeriveUnderlyingReadShow ''CompanyID)

instance FromReqURI CompanyID where
  fromReqURI = readM

newtype ExternalCompanyID = ExternalCompanyID { unExternalCompanyID :: BS.ByteString }
  deriving (Eq, Ord, Show)
$(newtypeDeriveConvertible ''ExternalCompanyID)

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
    fetchCompanies st

data GetCompany = GetCompany CompanyID
instance DBQuery GetCompany (Maybe Company) where
  dbQuery (GetCompany cid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompaniesSQL ++ "WHERE c.id = ?"
    _ <- execute st [toSql cid]
    cs <- fetchCompanies st
    oneObjectReturnedGuard cs

data GetCompanyByExternalID = GetCompanyByExternalID (Maybe ServiceID) ExternalCompanyID
instance DBQuery GetCompanyByExternalID (Maybe Company) where
  dbQuery (GetCompanyByExternalID msid ecid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompaniesSQL
      ++ "WHERE ((?::TEXT IS NULL AND c.service_id IS NULL) OR c.service_id = ?) AND c.external_id = ?"
    _ <- execute st [toSql msid, toSql msid, toSql ecid]
    cs <- fetchCompanies st
    oneObjectReturnedGuard cs

data CreateCompany = CreateCompany (Maybe ServiceID) (Maybe ExternalCompanyID)
instance DBUpdate CreateCompany Company where
  dbUpdate (CreateCompany msid mecid) = do
    wrapDB $ \conn -> runRaw conn "LOCK TABLE companies IN ACCESS EXCLUSIVE MODE"
    cid <- CompanyID <$> getUniqueID tableCompanies
    wrapDB $ \conn -> do
      _ <- run conn ("INSERT INTO companies ("
        ++ "  id"
        ++ ", external_id"
        ++ ", service_id"
        ++ ", name"
        ++ ", number"
        ++ ", address"
        ++ ", zip"
        ++ ", city"
        ++ ", country) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)") $ [
            toSql cid
          , toSql mecid
          , toSql msid
          ] ++ replicate 6 (toSql "")
      return ()
    dbQuery (GetCompany cid) >>= maybe (E.throw $ NoObject "") return

data SetCompanyInfo = SetCompanyInfo CompanyID CompanyInfo
instance DBUpdate SetCompanyInfo Bool where
  dbUpdate (SetCompanyInfo cid ci) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE companies SET"
      ++ "  name = ?"
      ++ ", number = ?"
      ++ ", address = ?"
      ++ ", zip = ?"
      ++ ", city = ?"
      ++ ", country = ?"
      ++ "  WHERE id = ?") [
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
  dbUpdate (GetOrCreateCompanyWithExternalID msid ecid) = do
    mc <- dbQuery $ GetCompanyByExternalID msid ecid
    case mc of
      Just c  -> return c
      Nothing -> dbUpdate $ CreateCompany msid $ Just ecid

-- helpers

selectCompaniesSQL :: String
selectCompaniesSQL = "SELECT"
  ++ "  c.id"
  ++ ", c.external_id"
  ++ ", c.service_id"
  ++ ", c.name"
  ++ ", c.number"
  ++ ", c.address"
  ++ ", c.zip"
  ++ ", c.city"
  ++ ", c.country"
  ++ "  FROM companies c"
  ++ " "

fetchCompanies :: Statement -> IO [Company]
fetchCompanies st = foldDB st decoder []
  where
    decoder acc cid eid sid name number address zip' city country = Company {
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
      } : acc

-- this will not be needed when we move documents to pgsql. for now it's needed
-- for document handlers - it seems that types of arguments that handlers take
-- need to be serializable. I don't know wtf, but I'll gladly dispose of these
-- instances when we're done with the migration.

newtype CompanyID_0 = CompanyID_0 Int
  deriving (Eq, Ord, Typeable)

deriving instance Typeable Company
deriving instance Typeable CompanyInfo
deriving instance Typeable ExternalCompanyID

instance Version CompanyID where
  mode = extension 2 (Proxy :: Proxy CompanyID_0)

instance Version CompanyID_0
instance Version Company
instance Version CompanyInfo
instance Version ExternalCompanyID

instance Migrate CompanyID_0 CompanyID where
  migrate (CompanyID_0 n) = CompanyID (fromIntegral n)

$(deriveSerializeFor [
    ''CompanyID
  , ''CompanyID_0
  , ''Company
  , ''CompanyInfo
  , ''ExternalCompanyID
  ])
