{-# OPTIONS_GHC -Wall -fno-warn-orphans -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
module API.Service.ServiceState
    ( ServiceID(..)
    , ServiceAdmin(..)
    , Service(..)
    , Services
    , GetService(..)
    , GetServicesForAdmin(..)
    , GetServices(..)
    , CreateService(..)
    , UpdateService(..)
) where
import Codec.Binary.Base16 as Base16
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Data
import Data.Maybe (isJust)
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.State
import Misc
import User.Password
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

newtype ServiceID = ServiceID { unServiceID :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
      
deriving instance Data ServiceID
instance Version ServiceID

instance Show ServiceID where
    showsPrec _prec (ServiceID val) = (++) (BS.toString val)

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


data Service1 = Service1
          { serviceid1             :: ServiceID
          , servicepassword1       :: Password
          , serviceadmin1  :: ServiceAdmin
          }
            deriving (Eq, Ord, Typeable)            

data Service = Service
          { serviceid             :: ServiceID
          , servicepassword       :: Password
          , serviceadmin  :: ServiceAdmin
          , servicename :: BS.ByteString
          , servicedocumentinvitationmail :: BS.ByteString
          }
            deriving (Eq, Ord)            


instance Migrate Service1 Service where
    migrate  (Service1
          { serviceid1   
          , servicepassword1 
          , serviceadmin1 
          }) =   Service
          { serviceid = serviceid1   
          , servicepassword = servicepassword1
          , serviceadmin = serviceadmin1
          , servicename = BS.empty
          , servicedocumentinvitationmail = BS.empty
          }
    
instance Migrate () Service1 where
    migrate _ =  error "No migration avaible"
    
instance Typeable Service where typeOf _ = mkTypeOf "Service"

   
instance Version Service1  where
    mode = extension 1 (Proxy :: Proxy ())     

instance Version Service  where
    mode = extension 2 (Proxy :: Proxy Service1)    

instance Indexable Service where
        empty = ixSet [   ixFun (\x -> [serviceid x] :: [ServiceID])
                        , ixFun (\x -> [serviceadmin x] :: [ServiceAdmin])]

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

getServicesForAdmin :: ServiceAdmin ->  Query Services [Service]
getServicesForAdmin admin = do
  services <- ask
  return $ toList (services @= admin)

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
                    , servicename = BS.empty
                    , servicedocumentinvitationmail = BS.empty
                    }
        modify (updateIx sid srv)
        return $ Just srv
        
updateService:: ServiceID -> BS.ByteString -> BS.ByteString -> Update Services ()
updateService sid nicename invitationmain = do
  _ <- modifyService sid $ \s ->
    Right s { servicename = nicename 
            , servicedocumentinvitationmail = invitationmain
            }        
  return ()                    
                        
$(mkMethods ''Services [ 
                'getService
              , 'getServicesForAdmin  
              , 'getServices
              , 'createService
              , 'updateService
              ])
                    
$(deriveSerializeFor [ 
                ''Service
              , ''ServiceID
              , ''ServiceAdmin
              , ''Service1
              ])

instance Component Services where
  type Dependencies Services = End
  initialValue = IxSet.empty
