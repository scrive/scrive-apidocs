{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror -fno-warn-unused-binds #-}
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
    , CompanyID(..)
    , CompanyUser(..)
    , Companies
    , GetCompany(..)
    , GetCompanyByExternalID(..)
    , GetOrCreateCompanyWithExternalID(..)
) where
import API.Service.ServiceState 
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Data
import Data.Maybe (isJust, fromJust)
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.State
import Happstack.Util.Common
import Misc
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

newtype CompanyID = CompanyID { unCompanyID :: Int }
    deriving (Eq, Ord, Typeable)

deriving instance Data CompanyID 

newtype ExternalCompanyID = ExternalCompanyID { unExternalCompanyID :: BS.ByteString }
    deriving (Eq, Ord, Typeable)

deriving instance Data ExternalCompanyID


newtype CompanyUser = CompanyUser { unCompanyUser :: Int }
    deriving (Eq, Ord, Typeable)

deriving instance Data CompanyUser

data Company = Company
               { companyid :: CompanyID
               , companyexternalid :: ExternalCompanyID
               , companyservice :: Maybe ServiceID
               }
             deriving (Eq, Ord)
     
instance Typeable Company where typeOf _ = mkTypeOf "Company"
 

deriving instance Show Company

instance Version Company 
instance Version CompanyUser
instance Version CompanyID 
instance Version ExternalCompanyID 

instance Show CompanyID where
    showsPrec prec (CompanyID val) = showsPrec prec val
    
instance Read CompanyID where
    readsPrec prec = let make (i,v) = (CompanyID i,v) 
                     in map make . readsPrec prec 
                     
instance FromReqURI CompanyID where
    fromReqURI = readM                     

instance Show ExternalCompanyID where
    show (ExternalCompanyID val) = BS.toString val
    
instance Read ExternalCompanyID where
    readsPrec _prec s =  [(ExternalCompanyID  $ BS.fromString  s,"")] 

instance Show CompanyUser where
    showsPrec prec (CompanyUser val) = showsPrec prec val

instance Read CompanyUser where
    readsPrec prec = let make (i,v) = (CompanyUser i,v) 
                     in map make . readsPrec prec 

type Companies = IxSet Company

instance Indexable Company where
        empty = ixSet [ ixFun (\x -> [companyid x] :: [CompanyID])
                      , ixFun (\x -> [companyexternalid x] :: [ExternalCompanyID]) 
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

getCompany :: (Maybe ServiceID) -> CompanyID -> Query Companies (Maybe Company)
getCompany sid cid = do
  companies <- ask
  return $ getOne (companies @= sid @= cid)

getCompanyByExternalID :: (Maybe ServiceID) -> ExternalCompanyID -> Query Companies (Maybe Company)
getCompanyByExternalID sid ecid = do
  companies <- ask
  return $ getOne (companies @= sid @= ecid)
  
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
                    , companyexternalid = ecid
                    }
        modify $ insert company
        return $ company

getCompanies :: (Maybe ServiceID) -> Query Companies [Company]
getCompanies sid = do
  companies <- ask
  return $ toList (companies @= sid)


$(mkMethods ''Companies [ 
                       'getCompany
                     , 'getCompanyByExternalID  
                     , 'getCompanies   
                     , 'getOrCreateCompanyWithExternalID
                        ])

$(deriveSerializeFor [ ''CompanyID
                     , ''CompanyUser
                     , ''Company
                     , ''ExternalCompanyID
                     ])

instance Component Companies where
  type Dependencies Companies = End
  initialValue = IxSet.empty
