{-# OPTIONS_GHC -Wall #-}
module API.Service.ServiceState
    ( ServiceID(..)
    , ServiceAdmin(..)
    , Service(..)
    , Services(..)
    , GetService(..)
    , GetServices(..)
    , CreateService(..)
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
import Data.List
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Control.Applicative
import MinutesTime
import Data.Data
import Codec.Binary.Base16 as Base16
import User.Password

newtype ServiceID = ServiceID { unServiceID :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
      
deriving instance Data ServiceID
instance Version ServiceID

instance Show ServiceID where
    showsPrec prec (ServiceID val) = showsPrec prec val

instance Read ServiceID where
    readsPrec prec = let make (i,v) = (ServiceID i,v) 
                     in map make . readsPrec prec 

instance URLAble ServiceID where
    encodeForURL = Base16.encode . BS.unpack . unServiceID   
    
instance FromReqURI ServiceID  where
   fromReqURI = (fmap (ServiceID . BS.pack)) . Base16.decode 

newtype ServiceAdmin = ServiceAdmin { unServiceAdmin :: Int }
    deriving (Eq, Ord, Typeable, Read, Show)
    
deriving instance Data ServiceAdmin
instance Version ServiceAdmin


data Service = Service
          { serviceid             :: ServiceID
          , servicepassword       :: Password
          , serviceadmin  :: ServiceAdmin
          }
            deriving (Eq, Ord)            


instance Migrate () Service where
    migrate _ =  error "No migration avaible"
    
instance Typeable Service where typeOf _ = mkTypeOf "Service"

   
instance Version Service  where
    mode = extension 1 (Proxy :: Proxy ())     


instance Indexable Service where
        empty = ixSet [ ixFun (\x -> [serviceid x] :: [ServiceID]) ]

type Services = IxSet Service


modifyService :: ServiceID 
           -> (Service -> Either String Service) 
           -> Update Services (Either String Service)
modifyService sid action = do
  services <- ask
  case getOne (services @= sid) of
    Nothing -> return $ Left "no such service"
    Just service -> 
        case action service of
          Left message -> return $ Left message
          Right newservice -> 
              if serviceid newservice /= sid
                 then return $ Left "No one can change id of service"
              else do
                modify (updateIx sid newservice)
                return $ Right newservice

getService :: ServiceID -> Query Services (Maybe Service)
getService sid = do
  services <- ask
  return $ getOne (services @= sid)

getServices :: Query Services [Service]
getServices = do
  services <- ask
  return $ toList services

createService :: ServiceID -> Password -> ServiceAdmin -> Update Services (Maybe Service)
createService sid passwd admin = do
     services <- ask
     if (isJust $ getOne (services @= sid)) 
      then return Nothing
      else do
        let srv = Service { 
                      serviceid = sid
                    , servicepassword = passwd
                    , serviceadmin = admin
                    }
        modify (updateIx sid srv)
        return $ Just srv
        

        
$(mkMethods ''Services [ 
                'getService
              , 'getServices
              , 'createService
              ])
                    
$(deriveSerializeFor [ 
                ''Service
              , ''ServiceID
              , ''ServiceAdmin
              ])

instance Component Services where
  type Dependencies Services = End
  initialValue = IxSet.empty
