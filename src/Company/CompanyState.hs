{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Company.CompanyState
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  portable
--
-- Company in our system. Some users will be maked as members of company. Documents that they will create
-- will be maked as company documents.
-----------------------------------------------------------------------------
module Company.CompanyState
    ( Company(..)
    , CompanyID(..)
    , CompanyUser(..)
    , Companies(..)
    
) where
import Happstack.Data
import Happstack.State
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify,MonadState(..))
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS (unlines) 
import Happstack.Data.IxSet as IxSet
import Data.Maybe(isJust,fromJust,maybe)
import Misc
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Codec.Utils (Octet)
import Data.Digest.SHA256 (hash)
import System.Random
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Control.Applicative
import System.Time as ST
import MinutesTime as MT
import Payments.PaymentsState as Payments
import Data.Data
import Data.Maybe
import User.Password
import API.Service.ServiceState 


newtype CompanyID = CompanyID { unCompanyID :: Int }
    deriving (Eq, Ord, Typeable)

deriving instance Data CompanyID 

newtype CompanyUser = CompanyUser { unCompanyUser :: Int }
    deriving (Eq, Ord, Typeable)

deriving instance Data CompanyUser

data Company = Company
          {   companyid :: CompanyID
            , companyservice :: Maybe ServiceID
          }
    deriving (Eq, Ord)
     
instance Typeable Company where typeOf _ = mkTypeOf "Company"
 

deriving instance Show Company

instance Version Company 
instance Version CompanyUser
instance Version CompanyID 

instance Show CompanyID where
    showsPrec prec (CompanyID val) = showsPrec prec val
    
instance Read CompanyID where
    readsPrec prec = let make (i,v) = (CompanyID i,v) 
                     in map make . readsPrec prec 

instance Show CompanyUser where
    showsPrec prec (CompanyUser val) = showsPrec prec val

instance Read CompanyUser where
    readsPrec prec = let make (i,v) = (CompanyUser i,v) 
                     in map make . readsPrec prec 

type Companies = IxSet Company

instance Indexable Company where
        empty = ixSet [ ixFun (\x -> [companyid x] :: [CompanyID]) ]

modifyCompany :: CompanyID 
           -> (Company -> Either String Company) 
           -> Update Companies (Either String Company)
modifyCompany cid action = do
  companies <- ask
  case getOne (companies @= cid) of
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

getCompany :: CompanyID -> Query Companies (Maybe Company)
getCompany cid = do
  companies <- ask
  return $ getOne (companies @= cid)

getCompanies :: Query Companies [Company]
getCompanies = do
  companies <- ask
  return $ toList companies

createCompany :: (Maybe ServiceID) -> Update Companies Company
createCompany msid = do
    companies <- ask
    cid <- getUnique companies CompanyID
    let company = Company {
                    companyid = cid,
                    companyservice = msid
                    }
    modify $ insert company
    return company
    

$(mkMethods ''Companies [ 
                       'getCompany
                     , 'getCompanies   
                     , 'createCompany
                        ])

$(deriveSerializeFor [ ''CompanyID
                     , ''CompanyUser
                     , ''Company
                     ])

instance Component Companies where
  type Dependencies Companies = End
  initialValue = IxSet.empty
