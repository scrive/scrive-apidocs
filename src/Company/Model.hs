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
  , AddCompanyInvite(..)
  , RemoveCompanyInvite(..)
  , GetCompanyInvite(..)
  , GetInvitesForCompany(..)
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

{- |
    A CompanyInvite is a record
    of an invitation made by a company
    to takeover an existing user.
-}
data CompanyInvite = CompanyInvite {
    invitedemail    :: BS.ByteString --who was invited
  , invitedfstname  :: BS.ByteString --the fstname they were invited as
  , invitedsndname  :: BS.ByteString --the sndname they were invited as
  , invitingcompany :: CompanyID --the company they are invited to
  } deriving (Eq, Ord, Show)

data GetCompanies = GetCompanies (Maybe ServiceID)
instance DBQuery GetCompanies [Company] where
  dbQuery (GetCompanies msid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompaniesSQL
      ++ "WHERE (?::TEXT IS NULL AND c.service_id IS NULL) OR c.service_id = ? ORDER BY c.id DESC"
    _ <- execute st [toSql msid, toSql msid]
    fetchCompanies st []

data GetCompany = GetCompany CompanyID
instance DBQuery GetCompany (Maybe Company) where
  dbQuery (GetCompany cid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompaniesSQL ++ "WHERE c.id = ?"
    _ <- execute st [toSql cid]
    cs <- fetchCompanies st []
    oneObjectReturnedGuard cs

data GetCompanyByExternalID = GetCompanyByExternalID (Maybe ServiceID) ExternalCompanyID
instance DBQuery GetCompanyByExternalID (Maybe Company) where
  dbQuery (GetCompanyByExternalID msid ecid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompaniesSQL
      ++ "WHERE ((?::TEXT IS NULL AND c.service_id IS NULL) OR c.service_id = ?) AND c.external_id = ?"
    _ <- execute st [toSql msid, toSql msid, toSql ecid]
    cs <- fetchCompanies st []
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
    dbQuery (GetCompany cid) >>= maybe (E.throw $ TooManyObjects "" 0 1) return

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

data AddCompanyInvite = AddCompanyInvite CompanyInvite
instance DBUpdate AddCompanyInvite CompanyInvite where
  dbUpdate (AddCompanyInvite CompanyInvite{
      invitedemail
    , invitedfstname
    , invitedsndname
    , invitingcompany
    }) = do
    wrapDB $ \conn -> runRaw conn "LOCK TABLE companyinvites IN ACCESS EXCLUSIVE MODE"
    wrapDB $ \conn -> do
      _ <- run conn ("DELETE FROM companyinvites "
                      ++ "WHERE (company_id = ? AND email = ?)") $
                      [ toSql invitingcompany
                      , toSql invitedemail]
      _ <- run conn ("INSERT INTO companyinvites ("
                      ++ "  email"
                      ++ ", first_name"
                      ++ ", last_name"
                      ++ ", company_id) VALUES (?, ?, ?, ?)") $
                      [ toSql invitedemail
                      , toSql invitedfstname
                      , toSql invitedsndname
                      , toSql invitingcompany]
      return ()
    dbQuery (GetCompanyInvite invitingcompany invitedemail) >>= maybe (E.throw $ NoObject "") return

data RemoveCompanyInvite = RemoveCompanyInvite CompanyID BS.ByteString
instance DBUpdate RemoveCompanyInvite () where
  dbUpdate (RemoveCompanyInvite companyid email) = do
  wrapDB $ \conn -> runRaw conn "LOCK TABLE companyinvites IN ACCESS EXCLUSIVE MODE"
  wrapDB $ \conn -> do
    _ <- run conn ("DELETE FROM companyinvites "
                    ++ "WHERE (company_id = ? AND email = ?)") $
                    [ toSql companyid
                    , toSql email]
    return ()
  dbQuery (GetCompanyInvite companyid email) >>= maybe (return ()) (const $ E.throw $ NoObject "")

data GetCompanyInvite = GetCompanyInvite CompanyID BS.ByteString
instance DBQuery GetCompanyInvite (Maybe CompanyInvite) where
  dbQuery (GetCompanyInvite companyid email) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompanyInvitesSQL
      ++ "WHERE (c.company_id = ? AND c.email = ?)"
    _ <- execute st [toSql companyid, toSql email]
    cs <- fetchCompanyInvites st []
    oneObjectReturnedGuard cs

data GetInvitesForCompany = GetInvitesForCompany CompanyID
instance DBQuery GetInvitesForCompany [CompanyInvite] where
    dbQuery (GetInvitesForCompany companyid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectCompanyInvitesSQL
      ++ "WHERE (c.company_id = ?)"
    _ <- execute st [toSql companyid]
    fetchCompanyInvites st []

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

fetchCompanies :: Statement -> [Company] -> IO [Company]
fetchCompanies st acc = fetchRow st >>= maybe (return acc) f
  where f [cid, eid, sid, name, number, address, zip', city, country
         ] = fetchCompanies st $ Company {
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
         } : acc
        f l = error $ "fetchCompanies: unexpected row: "++show l

selectCompanyInvitesSQL :: String
selectCompanyInvitesSQL = "SELECT"
  ++ "  ci.email"
  ++ ", ci.first_name"
  ++ ", ci.last_name"
  ++ ", ci.company_id"
  ++ "  FROM companyinvites ci"
  ++ " "

fetchCompanyInvites :: Statement -> [CompanyInvite] -> IO [CompanyInvite]
fetchCompanyInvites st acc = fetchRow st >>= maybe (return acc) f
  where f [email, fstname, sndname, cid
         ] = fetchCompanyInvites st $ CompanyInvite {
             invitedemail = fromSql email
           , invitedfstname = fromSql fstname
           , invitedsndname = fromSql sndname
           , invitingcompany = fromSql cid
         } : acc
        f l = error $ "fetchCompanyInvites: unexpected row: "++show l

-- this will not be needed when we move documents to pgsql. for now it's needed
-- for document handlers - it seems that types of arguments that handlers take
-- need to be serializable. I don't know wtf, but I'll gladly dispose of these
-- instances when we're done with the migration.

newtype CompanyID_0 = CompanyID_0 Int
  deriving (Eq, Ord, Typeable)

deriving instance Typeable Company
deriving instance Typeable CompanyInfo
deriving instance Typeable ExternalCompanyID
deriving instance Typeable CompanyInvite

instance Version CompanyID where
  mode = extension 2 (Proxy :: Proxy CompanyID_0)

instance Version CompanyID_0
instance Version Company
instance Version CompanyInfo
instance Version ExternalCompanyID
instance Version CompanyInvite

instance Migrate CompanyID_0 CompanyID where
  migrate (CompanyID_0 n) = CompanyID (fromIntegral n)

$(deriveSerializeFor [
    ''CompanyID
  , ''CompanyID_0
  , ''Company
  , ''CompanyInfo
  , ''ExternalCompanyID
  , ''CompanyInvite
  ])
