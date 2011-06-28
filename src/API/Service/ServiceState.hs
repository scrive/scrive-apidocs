module API.Service.ServiceState
    ( ServiceID(..)
    , ServiceAdmin(..)
    , ServiceLocation(..)
    , ServiceSettings(..)
    , ServiceUI(..)
    , Service(..)
    , Services
    , GetService(..)
    , GetServicesForAdmin(..)
    , GetServices(..)
    , GetServiceByLocation(..)
    , CreateService(..)
    , UpdateServiceUI(..)
    , UpdateServiceSettings(..)
    , toServiceLocation
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

newtype ServiceLocation = ServiceLocation { unServiceLocation :: BS.ByteString }
    deriving (Eq, Ord, Typeable, Read)

toServiceLocation :: String -> ServiceLocation
toServiceLocation s = let
                        (prefix,rest) = splitAt 8 s
                      in ServiceLocation $ BS.fromString $ prefix ++ ((takeWhile ((/=) '/')) rest)

instance Show ServiceLocation where
    showsPrec _prec (ServiceLocation val) = (++) (BS.toString val)

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

data Service2 = Service2
          { serviceid2                     :: ServiceID
          , servicepassword2               :: Password
          , serviceadmin2                  :: ServiceAdmin
          , servicename2 :: BS.ByteString
          , servicedocumentinvitationmail2 :: BS.ByteString
          }
            deriving (Eq, Ord, Typeable)


data Service = Service
          {   serviceid             :: ServiceID
            , servicesettings :: ServiceSettings
            , serviceui:: ServiceUI
          }
            deriving (Eq, Ord, Show)

data ServiceSettings = ServiceSettings  {
            servicepassword  :: Password
          , serviceadmin     :: ServiceAdmin
          , servicelocation  :: Maybe ServiceLocation
          , servicemailfromaddress :: Maybe BS.ByteString
    }  deriving (Eq, Ord, Typeable, Show)

data ServiceUI = ServiceUI {
            servicemailfooter :: Maybe BS.ByteString
          , servicebuttons :: Maybe (BS.ByteString,BS.ByteString) -- Two sprite files
          , servicebackground :: Maybe BS.ByteString
          , serviceoverlaybackground :: Maybe BS.ByteString
          , servicebarsbackground :: Maybe BS.ByteString
          , servicelogo :: Maybe BS.ByteString -- File with the logo

    }  deriving (Eq, Ord, Typeable, Show)


instance Migrate Service2 Service where
    migrate  ( Service2
          { serviceid2
          , servicepassword2
          , serviceadmin2
          }) = Service {
              serviceid = serviceid2
            , servicesettings = ServiceSettings {
                  servicepassword = servicepassword2
                , serviceadmin = serviceadmin2
                , servicelocation = Nothing
                , servicemailfromaddress  = Nothing }
            , serviceui = ServiceUI {
                    servicemailfooter  = Nothing
                  , servicebuttons  = Nothing
                  , servicebackground  = Nothing
                  , serviceoverlaybackground  = Nothing
                  , servicebarsbackground = Nothing
                  , servicelogo = Nothing}
          }

instance Migrate Service1 Service2 where
    migrate  (Service1
          { serviceid1
          , servicepassword1
          , serviceadmin1
          }) =   Service2
          { serviceid2 = serviceid1
          , servicepassword2 = servicepassword1
          , serviceadmin2 = serviceadmin1
          , servicename2 = BS.empty
          , servicedocumentinvitationmail2 = BS.empty
          }

instance Migrate () Service1 where
    migrate _ =  error "No migration avaible"

instance Typeable Service where typeOf _ = mkTypeOf "Service"


instance Version ServiceLocation
instance Version ServiceSettings
instance Version ServiceUI


instance Version Service1  where
    mode = extension 1 (Proxy :: Proxy ())

instance Version Service2  where
    mode = extension 2 (Proxy :: Proxy Service1)

instance Version Service  where
    mode = extension 3 (Proxy :: Proxy Service2)

instance Indexable Service where
        empty = ixSet [   ixFun (\x -> [serviceid x] :: [ServiceID])
                        , ixFun (\x -> [serviceadmin $ servicesettings x] :: [ServiceAdmin])
                        , ixFun (\x -> case (servicelocation  $ servicesettings x) of
                                          Just l -> [l]
                                          _ -> [] )]
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


getServiceByLocation :: ServiceLocation -> Query Services (Maybe Service)
getServiceByLocation location = do
  services <- ask
  return $ getOne (services @= location)


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
                    , servicesettings = ServiceSettings {
                        servicepassword =  passwd
                      , serviceadmin = admin
                      , servicelocation = Nothing
                      , servicemailfromaddress  = Nothing }
                    , serviceui = ServiceUI {
                          servicemailfooter  = Nothing
                        , servicebuttons  = Nothing
                        , servicebackground  = Nothing
                        , serviceoverlaybackground  = Nothing
                        , servicebarsbackground = Nothing
                        , servicelogo = Nothing}
                    }
        modify (updateIx sid srv)
        return $ Just srv

updateServiceUI:: ServiceID -> ServiceUI -> Update Services ()
updateServiceUI sid ui = do
  _ <- modifyService sid $ \s ->
                    Right s { serviceui = ui }
  return ()

updateServiceSettings:: ServiceID -> ServiceSettings -> Update Services ()
updateServiceSettings sid settings = do
  _ <- modifyService sid $ \s ->
                    Right s { servicesettings = settings }
  return ()


$(mkMethods ''Services [
                'getService
              , 'getServicesForAdmin
              , 'getServices
              , 'getServiceByLocation
              , 'createService
              , 'updateServiceUI
              , 'updateServiceSettings
              ])

$(deriveSerializeFor [
                ''Service
              , ''ServiceID
              , ''ServiceAdmin
              , ''ServiceLocation
              , ''Service1
              , ''Service2
              , ''ServiceSettings
              , ''ServiceUI
              ])

instance Component Services where
  type Dependencies Services = End
  initialValue = IxSet.empty
