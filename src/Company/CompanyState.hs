{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Company.CompanyState
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  portable
--
-- Company in our system. Some users will be marked as members of company. Documents that they will create
-- will be marked as company documents.
-----------------------------------------------------------------------------
module Company.CompanyState
    ( Company(..)
    , CompanyInfo(..)
    , CompanyID(..)
    , CompanyUser(..)
    , Companies
    , GetCompany(..)
    , GetCompanyByExternalID(..)
    , SetCompanyInfo(..)
    , CreateNewCompany(..)
    , GetOrCreateCompanyWithExternalID(..)
    , ExternalCompanyID(..)
    , insertCompaniesIntoPG
) where
import API.Service.ServiceState
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Data
import Data.Maybe (isJust, fromJust, catMaybes)
import Database.HDBC
import DB.Derive
import DB.Classes
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.State
import Happstack.Util.Common
import Misc
import Numeric
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

newtype CompanyID = CompanyID { unCompanyID :: Int }
    deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''CompanyID)

deriving instance Data CompanyID

newtype ExternalCompanyID = ExternalCompanyID { unExternalCompanyID :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''ExternalCompanyID)

deriving instance Data ExternalCompanyID


newtype CompanyUser = CompanyUser { unCompanyUser :: Int }
    deriving (Eq, Ord, Typeable)

deriving instance Data CompanyUser

data Company0 = Company0
               { companyid0 :: CompanyID
               , companyexternalid0 :: ExternalCompanyID
               , companyservice0 :: Maybe ServiceID
               }
             deriving (Eq, Ord, Show, Typeable)

data Company = Company
               { companyid :: CompanyID
               , companyexternalid :: Maybe ExternalCompanyID
               , companyservice :: Maybe ServiceID
               , companyinfo :: CompanyInfo
               }
             deriving (Eq, Ord, Show)

instance Typeable Company where typeOf _ = mkTypeOf "Company"

instance Migrate Company0 Company where
    migrate (Company0 {
               companyid0
             , companyexternalid0
             , companyservice0
          }) = Company {
              companyid         = companyid0
            , companyexternalid = Just companyexternalid0
            , companyservice    = companyservice0
            , companyinfo       = CompanyInfo {
                                      companyname       = BS.empty
                                    , companynumber     = BS.empty
                                    , companyaddress    = BS.empty
                                    , companyzip        = BS.empty
                                    , companycity       = BS.empty
                                    , companycountry    = BS.empty
                                  }
          }

instance Version Company0

instance Version Company where
    mode = extension 1 (Proxy :: Proxy Company0)

data CompanyInfo = CompanyInfo
               { companyname :: BS.ByteString
               , companynumber :: BS.ByteString
               , companyaddress :: BS.ByteString
               , companyzip :: BS.ByteString
               , companycity :: BS.ByteString
               , companycountry :: BS.ByteString
               }
             deriving (Eq, Ord, Show)

instance Typeable CompanyInfo where typeOf _ = mkTypeOf "CompanyInfo"

instance Version CompanyInfo

instance Version CompanyUser
instance Version CompanyID
instance Version ExternalCompanyID

instance Show CompanyID where
    showsPrec prec (CompanyID val) = showsPrec prec val

instance Read CompanyID where
    readsPrec _prec = let make (i,v) = (CompanyID i,v)
                      in map make . readDec

instance FromReqURI CompanyID where
    fromReqURI = readM

instance Show ExternalCompanyID where
    show (ExternalCompanyID val) = BS.toString val

instance Read ExternalCompanyID where
    readsPrec _prec s =  [(ExternalCompanyID  $ BS.fromString  s,"")]

instance Show CompanyUser where
    showsPrec prec (CompanyUser val) = showsPrec prec val

instance Read CompanyUser where
    readsPrec _prec = let make (i,v) = (CompanyUser i,v)
                      in map make . readDec

type Companies = IxSet Company

instance Indexable Company where
        empty = ixSet [ ixFun (\x -> [companyid x] :: [CompanyID])
                      , ixFun (\x -> catMaybes [companyexternalid x] :: [ExternalCompanyID])
                      , ixFun (\x -> [companyservice x] :: [Maybe ServiceID])]

modifyCompany :: (Maybe ServiceID) -> CompanyID
           -> (Company -> Either String Company)
           -> Update Companies (Either String Company)
modifyCompany sid cid action = do
  companies <- ask
  case getOne (companies @= sid @= cid ) of
    Nothing -> return $ Left "no such company"
    Just company ->
        case action company of
          Left message -> return $ Left message
          Right newcompany ->
              if companyid newcompany /= cid
                 then return $ Left "No one can change company id"
              else do
                modify (updateIx cid newcompany)
                return $ Right newcompany

getCompany ::CompanyID -> Query Companies (Maybe Company)
getCompany cid = do
  companies <- ask
  return $ getOne (companies @= cid)

getCompanyByExternalID :: (Maybe ServiceID) -> ExternalCompanyID -> Query Companies (Maybe Company)
getCompanyByExternalID sid ecid = do
  companies <- ask
  return $ getOne (companies @= sid @= ecid)

setCompanyInfo :: Company -> CompanyInfo -> Update Companies (Either String Company)
setCompanyInfo Company{companyid,companyservice} newcompanyinfo = modifyCompany companyservice companyid $ \company ->
  return $ company { companyinfo = newcompanyinfo }
  
createNewCompany :: Update Companies Company
createNewCompany = do
  companies <- ask
  cid <- getUnique companies CompanyID
  let company = Company {
    companyid = cid
  , companyservice = Nothing
  , companyexternalid = Nothing
  , companyinfo = CompanyInfo {
                      companyname = BS.empty
                    , companynumber = BS.empty
                    , companyaddress = BS.empty
                    , companyzip = BS.empty
                    , companycity = BS.empty
                    , companycountry = BS.empty
                  }
  }
  modify $ insert company
  return $ company

getOrCreateCompanyWithExternalID :: (Maybe ServiceID) -> ExternalCompanyID -> Update Companies Company
getOrCreateCompanyWithExternalID sid ecid = do
    companies <- ask
    let mcompany = getOne (companies @= ecid)
    if (isJust $ mcompany)
     then return $ fromJust $ mcompany
     else do
        cid <- getUnique companies CompanyID
        let company = Company {
                      companyid = cid
                    , companyservice = sid
                    , companyexternalid = Just ecid
                    , companyinfo = CompanyInfo {
                                        companyname = BS.empty
                                      , companynumber = BS.empty
                                      , companyaddress = BS.empty
                                      , companyzip = BS.empty
                                      , companycity = BS.empty
                                      , companycountry = BS.empty
                                   }
                    }
        modify $ insert company
        return $ company

getCompanies :: (Maybe ServiceID) -> Query Companies [Company]
getCompanies sid = do
  companies <- ask
  return $ toList (companies @= sid)

getAllCompanies :: Query Companies [Company]
getAllCompanies = ask >>= return . toList

$(mkMethods ''Companies [
                       'getCompany
                     , 'getCompanyByExternalID
                     , 'setCompanyInfo
                     , 'getCompanies
                     , 'createNewCompany
                     , 'getOrCreateCompanyWithExternalID
                     , 'getAllCompanies
                        ])

$(deriveSerializeFor [ ''CompanyID
                     , ''CompanyUser
                     , ''Company
                     , ''Company0
                     , ''CompanyInfo
                     , ''ExternalCompanyID
                     ])

instance Component Companies where
  type Dependencies Companies = End
  initialValue = IxSet.empty

insertCompaniesIntoPG :: DB ()
insertCompaniesIntoPG = wrapDB $ \conn -> do
  companies <- query GetAllCompanies
  forM_ companies $ \c -> do
    _ <- run conn ("INSERT INTO companies ("
      ++ "  id"
      ++ ", external_id"
      ++ ", service_id"
      ++ ", name"
      ++ ", number"
      ++ ", address"
      ++ ", zip"
      ++ ", city"
      ++ ", country) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)") [
        toSql $ companyid c
      , toSql $ companyexternalid c
      , toSql $ companyservice c
      , toSql $ companyname $ companyinfo c
      , toSql $ companynumber $ companyinfo c
      , toSql $ companyaddress $ companyinfo c
      , toSql $ companyzip $ companyinfo c
      , toSql $ companycity $ companyinfo c
      , toSql $ companycountry $ companyinfo c
      ]
    return ()
