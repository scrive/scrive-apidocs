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
    , insertServicesIntoPG
) where

import Codec.Binary.Base16 as Base16
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Data
import Data.Maybe (isJust)
import Database.HDBC
import DB.Classes
import DB.Derive
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.State
import Misc
import User.OldPassword
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BS


newtype ServiceID = ServiceID { unServiceID :: BS.ByteString }
    deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''ServiceID)

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
$(newtypeDeriveConvertible ''ServiceLocation)

toServiceLocation :: String -> ServiceLocation
toServiceLocation s = let
                        (prefix,rest) = splitAt 8 s
                      in ServiceLocation $ BS.fromString $ prefix ++ ((takeWhile ((/=) '/')) rest)

instance Show ServiceLocation where
    showsPrec _prec (ServiceLocation val) = (++) (BS.toString val)

newtype ServiceAdmin = ServiceAdmin { unServiceAdmin :: Int }
    deriving (Eq, Ord, Typeable, Read, Show)
$(newtypeDeriveConvertible ''ServiceAdmin)

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

data ServiceUI1 = ServiceUI1 {
            servicemailfooter1 :: Maybe BS.ByteString
          , servicebuttons1 :: Maybe (BS.ByteString,BS.ByteString) -- Two sprite files
          , servicebackground1 :: Maybe BS.ByteString
          , serviceoverlaybackground1 :: Maybe BS.ByteString
          , servicebarsbackground1 :: Maybe BS.ByteString
          , servicelogo1 :: Maybe BS.ByteString -- File with the logo

    }  deriving (Eq, Ord, Typeable, Show)

data ServiceUI = ServiceUI {
            servicemailfooter :: Maybe BS.ByteString
          , servicebuttons :: Maybe (BS.ByteString,BS.ByteString) -- Two sprite files
          , servicebuttonstextcolor :: Maybe BS.ByteString
          , servicebackground :: Maybe BS.ByteString
          , serviceoverlaybackground :: Maybe BS.ByteString
          , servicebarsbackground :: Maybe BS.ByteString
          , servicelogo :: Maybe BS.ByteString -- File with the logo

    }  deriving (Eq, Ord, Typeable, Show)


instance Migrate () ServiceUI1  where
    migrate _ =  error "No migration avaible"
    
instance Migrate ServiceUI1 ServiceUI where
    migrate  (ServiceUI1 {
            servicemailfooter1
          , servicebuttons1
          , servicebackground1
          , serviceoverlaybackground1
          , servicebarsbackground1 
          , servicelogo1

    }) = ServiceUI {
            servicemailfooter = servicemailfooter1
          , servicebuttons = servicebuttons1
          , servicebuttonstextcolor = Nothing
          , servicebackground = servicebackground1
          , serviceoverlaybackground = serviceoverlaybackground1
          , servicebarsbackground =  servicebarsbackground1 
          , servicelogo = servicelogo1

    }


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
                  , servicebuttonstextcolor = Nothing
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

instance Version ServiceUI1 
instance Version ServiceUI where
    mode = extension 1 (Proxy :: Proxy ServiceUI1)

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
                        , servicebuttonstextcolor = Nothing
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
              , ''ServiceUI1
              , ''ServiceUI
              ])

instance Component Services where
  type Dependencies Services = End
  initialValue = IxSet.empty

insertServicesIntoPG :: DB ()
insertServicesIntoPG = wrapDB $ \conn -> do
  services <- query GetServices
  forM_ services $ \s -> do
    let (salt, hash) = case servicepassword $ servicesettings s of
         NoPassword -> (Nothing, Nothing)
         Password salt' hash' -> (Just $ B64.encode $ BS.pack salt', Just $ B64.encode $ BS.pack hash')
    _ <- run conn ("INSERT INTO services ("
      ++ "  id"
      ++ ", password"
      ++ ", salt"
      ++ ", admin_id"
      ++ ", location"
      ++ ", email_from_address) VALUES (?, decode(?, 'base64'), decode(?, 'base64'), ?, ?, ?)") [
        toSql $ serviceid s
      , toSql hash
      , toSql salt
      , toSql $ serviceadmin $ servicesettings s
      , toSql $ servicelocation $ servicesettings s
      , toSql $ servicemailfromaddress $ servicesettings s
      ]
    _ <- run conn ("INSERT INTO service_uis ("
      ++ "  service_id"
      ++ ", mail_footer"
      ++ ", button1"
      ++ ", button2"
      ++ ", buttons_text_color"
      ++ ", background"
      ++ ", overlay_background"
      ++ ", bars_background"
      ++ ", logo) VALUES (?, ?, decode(?, 'base64'), decode(?, 'base64'), ?, ?, ?, ?, decode(?, 'base64'))") [
        toSql $ serviceid s
      , toSql $ servicemailfooter $ serviceui s
      , toSql $ (B64.encode . fst) `fmap` servicebuttons (serviceui s)
      , toSql $ (B64.encode . snd) `fmap` servicebuttons (serviceui s)
      , toSql $ servicebuttonstextcolor $ serviceui s
      , toSql $ servicebackground $ serviceui s
      , toSql $ serviceoverlaybackground $ serviceui s
      , toSql $ servicebarsbackground $ serviceui s
      , toSql $ B64.encode `fmap` servicelogo (serviceui s)
      ]
    return ()
