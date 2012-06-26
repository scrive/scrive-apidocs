module API.Service.Model (
    ServiceID(..)
  , ServiceLocation(..)
  , toServiceLocation
  , Service(..)
  , ServiceSettings(..)
  , ServiceUI(..)
  , GetServiceByLocation(..)
  , GetService(..)
  , GetServicesForAdmin(..)
  , GetServices(..)
  , CreateService(..)
  , UpdateServiceUI(..)
  , UpdateServiceSettings(..)
  ) where

import Control.Applicative
import Database.HDBC
import Happstack.Server
import qualified Codec.Binary.Base16 as B16
import qualified Data.ByteString as BS

import DB
import Misc
import User.Password
import User.UserID

newtype ServiceID = ServiceID { unServiceID :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''ServiceID)
$(newtypeDeriveUnderlyingReadShow ''ServiceID)

instance URLAble ServiceID where
  encodeForURL = B16.encode . BS.unpack . unServiceID
instance FromReqURI ServiceID  where
  fromReqURI = fmap (ServiceID . BS.pack) . B16.decode

newtype ServiceLocation = ServiceLocation { unServiceLocation :: String }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''ServiceLocation)
$(newtypeDeriveUnderlyingReadShow ''ServiceLocation)

toServiceLocation :: String -> ServiceLocation
toServiceLocation s =
  ServiceLocation $ prefix ++ takeWhile ((/=) '/') rest
  where
    (prefix, rest) = splitAt 8 s

data Service = Service {
    serviceid       :: ServiceID
  , servicesettings :: ServiceSettings
  , serviceui       :: ServiceUI
  } deriving (Eq, Ord, Show)

data ServiceSettings = ServiceSettings {
    servicepassword        :: Maybe Password
  , serviceadmin           :: UserID
  , servicelocation        :: Maybe ServiceLocation
  , servicemailfromaddress :: Maybe String
  } deriving (Eq, Ord, Show)

data ServiceUI = ServiceUI {
    servicemailfooter        :: Maybe String
  , servicebuttons           :: Maybe (Binary, Binary) -- Two sprite files
  , servicebuttonstextcolor  :: Maybe String
  , servicebackground        :: Maybe String
  , serviceoverlaybackground :: Maybe String
  , servicebarsbackground    :: Maybe String
  , servicelogo              :: Maybe Binary -- File with the logo
  } deriving (Eq, Ord, Show)

data GetServiceByLocation = GetServiceByLocation ServiceLocation
instance MonadDB m => DBQuery m GetServiceByLocation (Maybe Service) where
  query (GetServiceByLocation loc) = do
    kPrepare $ selectServicesSQL ++ "WHERE s.location = ?"
    _ <- kExecute [toSql loc]
    fetchServices >>= oneObjectReturnedGuard

data GetService = GetService ServiceID
instance MonadDB m => DBQuery m GetService (Maybe Service) where
  query (GetService sid) = do
    kPrepare $ selectServicesSQL ++ "WHERE s.id = ?"
    _ <- kExecute [toSql sid]
    fetchServices >>= oneObjectReturnedGuard

data GetServicesForAdmin = GetServicesForAdmin UserID
instance MonadDB m => DBQuery m GetServicesForAdmin [Service] where
  query (GetServicesForAdmin aid) = do
    kPrepare $ selectServicesSQL ++ "WHERE s.admin_id = ? ORDER BY s.id DESC"
    _ <- kExecute [toSql aid]
    fetchServices

data GetServices = GetServices
instance MonadDB m => DBQuery m GetServices [Service] where
  query GetServices = do
    kPrepare $ selectServicesSQL ++ "ORDER BY s.id DESC"
    _ <- kExecute []
    fetchServices

data CreateService = CreateService ServiceID (Maybe Password) UserID
instance MonadDB m => DBUpdate m CreateService (Maybe Service) where
  update (CreateService sid pwd aid) = do
    _ <- kRunRaw "LOCK TABLE services IN ACCESS EXCLUSIVE MODE"
    exists <- checkIfServiceExists sid
    if exists
      then return Nothing
      else do
        kPrepare $ "INSERT INTO services ("
          ++ "  id"
          ++ ", password"
          ++ ", salt"
          ++ ", admin_id) VALUES (?, ?, ?, ?)"
        _ <- kExecute [
            toSql sid
          , toSql $ pwdHash <$> pwd
          , toSql $ pwdSalt <$> pwd
          , toSql aid
          ]
        query $ GetService sid

data UpdateServiceUI = UpdateServiceUI ServiceID ServiceUI
instance MonadDB m => DBUpdate m UpdateServiceUI Bool where
  update (UpdateServiceUI sid sui) = do
    kPrepare $ "UPDATE services SET"
        ++ "  mail_footer = ?"
        ++ ", button1 = ?"
        ++ ", button2 = ?"
        ++ ", buttons_text_color = ?"
        ++ ", background = ?"
        ++ ", overlay_background = ?"
        ++ ", bars_background = ?"
        ++ ", logo = ?"
        ++ "  WHERE id = ?"
    kExecute01 [
        toSql $ servicemailfooter sui
      , toSql $ fst <$> servicebuttons sui
      , toSql $ snd <$> servicebuttons sui
      , toSql $ servicebuttonstextcolor sui
      , toSql $ servicebackground sui
      , toSql $ serviceoverlaybackground sui
      , toSql $ servicebarsbackground sui
      , toSql $ servicelogo sui
      , toSql sid
      ]

data UpdateServiceSettings = UpdateServiceSettings ServiceID ServiceSettings
instance MonadDB m => DBUpdate m UpdateServiceSettings Bool where
  update (UpdateServiceSettings sid ss) = do
    kPrepare $ "UPDATE services SET"
        ++ "  password = ?"
        ++ ", salt = ?"
        ++ ", admin_id = ?"
        ++ ", location = ?"
        ++ ", email_from_address = ?"
        ++ "  WHERE id = ?"
    kExecute01 [
        toSql $ pwdHash <$> servicepassword ss
      , toSql $ pwdSalt <$> servicepassword ss
      , toSql $ serviceadmin ss
      , toSql $ servicelocation ss
      , toSql $ servicemailfromaddress ss
      , toSql sid
      ]

-- helpers

checkIfServiceExists :: MonadDB m => ServiceID -> DBEnv m Bool
checkIfServiceExists uid = checkIfAnyReturned
  $ SQL "SELECT 1 FROM services WHERE id = ?" [toSql uid]

selectServicesSQL :: String
selectServicesSQL = "SELECT"
  ++ "  s.id"
  ++ ", s.password"
  ++ ", s.salt"
  ++ ", s.admin_id"
  ++ ", s.location"
  ++ ", s.email_from_address"
  ++ ", s.mail_footer"
  ++ ", s.button1"
  ++ ", s.button2"
  ++ ", s.buttons_text_color"
  ++ ", s.background"
  ++ ", s.overlay_background"
  ++ ", s.bars_background"
  ++ ", s.logo"
  ++ "  FROM services s"
  ++ " "

fetchServices :: MonadDB m => DBEnv m [Service]
fetchServices = foldDB decoder []
  where
    decoder acc sid password salt admin_id location email_from_address
      mail_footer button1 button2 buttons_text_color background
      overlay_background bars_background logo = Service {
          serviceid = sid
        , servicesettings = ServiceSettings {
            servicepassword = maybePassword (password, salt)
          , serviceadmin = admin_id
          , servicelocation = location
          , servicemailfromaddress = email_from_address
          }
        , serviceui = ServiceUI {
            servicemailfooter = mail_footer
          , servicebuttons = pairMaybe button1 button2
          , servicebuttonstextcolor = buttons_text_color
          , servicebackground = background
          , serviceoverlaybackground = overlay_background
          , servicebarsbackground = bars_background
          , servicelogo = logo
          }
        } : acc
