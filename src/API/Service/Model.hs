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
import Data.Data
import Database.HDBC
import Happstack.Server
import Happstack.State
import qualified Codec.Binary.Base16 as B16
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

import DB.Classes
import DB.Derive
import DB.Fetcher2
import DB.Utils
import DB.Types
import Misc
import User.Password
import User.UserID

newtype ServiceID = ServiceID { unServiceID :: BS.ByteString }
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''ServiceID)
$(newtypeDeriveUnderlyingReadShow ''ServiceID)

$(deriveSerialize ''ServiceID)
instance Version ServiceID

instance URLAble ServiceID where
  encodeForURL = B16.encode . BS.unpack . unServiceID
instance FromReqURI ServiceID  where
  fromReqURI = fmap (ServiceID . BS.pack) . B16.decode

newtype ServiceLocation = ServiceLocation { unServiceLocation :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''ServiceLocation)
$(newtypeDeriveUnderlyingReadShow ''ServiceLocation)

toServiceLocation :: String -> ServiceLocation
toServiceLocation s =
  ServiceLocation $ BS.fromString $ prefix ++ takeWhile ((/=) '/') rest
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
  , servicemailfromaddress :: Maybe BS.ByteString
  } deriving (Eq, Ord, Show)

data ServiceUI = ServiceUI {
    servicemailfooter        :: Maybe BS.ByteString
  , servicebuttons           :: Maybe (Binary, Binary) -- Two sprite files
  , servicebuttonstextcolor  :: Maybe BS.ByteString
  , servicebackground        :: Maybe BS.ByteString
  , serviceoverlaybackground :: Maybe BS.ByteString
  , servicebarsbackground    :: Maybe BS.ByteString
  , servicelogo              :: Maybe Binary -- File with the logo
  } deriving (Eq, Ord, Show)

data GetServiceByLocation = GetServiceByLocation ServiceLocation
instance DBQuery GetServiceByLocation (Maybe Service) where
  dbQuery (GetServiceByLocation loc) = do
    ss <- wrapDB $ \conn -> do
      st <- prepare conn $ selectServicesSQL ++ "WHERE s.location = ?"
      _ <- execute st [toSql loc]
      fetchServices st
    oneObjectReturnedGuard ss

data GetService = GetService ServiceID
instance DBQuery GetService (Maybe Service) where
  dbQuery (GetService sid) = do
    ss <- wrapDB $ \conn -> do
      st <- prepare conn $ selectServicesSQL ++ "WHERE s.id = ?"
      _ <- execute st [toSql sid]
      fetchServices st
    oneObjectReturnedGuard ss

data GetServicesForAdmin = GetServicesForAdmin UserID
instance DBQuery GetServicesForAdmin [Service] where
  dbQuery (GetServicesForAdmin aid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectServicesSQL ++ "WHERE s.admin_id = ? ORDER BY s.id DESC"
    _ <- execute st [toSql aid]
    fetchServices st

data GetServices = GetServices
instance DBQuery GetServices [Service] where
  dbQuery GetServices = wrapDB $ \conn -> do
    st <- prepare conn $ selectServicesSQL ++ "ORDER BY s.id DESC"
    _ <- executeRaw st
    fetchServices st

data CreateService = CreateService ServiceID (Maybe Password) UserID
instance DBUpdate CreateService (Maybe Service) where
  dbUpdate (CreateService sid pwd aid) = do
    exists <- checkIfServiceExists sid
    if exists
      then return Nothing
      else do
        wrapDB $ \conn -> do
          _ <- run conn ("INSERT INTO services ("
            ++ "  id"
            ++ ", password"
            ++ ", salt"
            ++ ", admin_id) VALUES (?, decode(?, 'base64'), decode(?, 'base64'), ?)") [
                toSql sid
              , toSql $ pwdHash <$> pwd
              , toSql $ pwdSalt <$> pwd
              , toSql aid
              ]
          return ()
        dbQuery $ GetService sid

data UpdateServiceUI = UpdateServiceUI ServiceID ServiceUI
instance DBUpdate UpdateServiceUI Bool where
  dbUpdate (UpdateServiceUI sid sui) = do
    r <- wrapDB $ \conn -> do
      run conn ("UPDATE services SET"
        ++ "  mail_footer = ?"
        ++ ", button1 = decode(?, 'base64')"
        ++ ", button2 = decode(?, 'base64')"
        ++ ", buttons_text_color = ?"
        ++ ", background = ?"
        ++ ", overlay_background = ?"
        ++ ", bars_background = ?"
        ++ ", logo = decode(?, 'base64')"
        ++ "  WHERE id = ?") [
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
    oneRowAffectedGuard r

data UpdateServiceSettings = UpdateServiceSettings ServiceID ServiceSettings
instance DBUpdate UpdateServiceSettings Bool where
  dbUpdate (UpdateServiceSettings sid ss) = do
    r <- wrapDB $ \conn -> do
      run conn ("UPDATE services SET"
        ++ "  password = decode(?, 'base64')"
        ++ ", salt = decode(?, 'base64')"
        ++ ", admin_id = ?"
        ++ ", location = ?"
        ++ ", email_from_address = ?"
        ++ "  WHERE id = ?") [
          toSql $ pwdHash <$> servicepassword ss
        , toSql $ pwdSalt <$> servicepassword ss
        , toSql $ serviceadmin ss
        , toSql $ servicelocation ss
        , toSql $ servicemailfromaddress ss
        , toSql sid
        ]
    oneRowAffectedGuard r

-- helpers

checkIfServiceExists :: ServiceID -> DB Bool
checkIfServiceExists uid = wrapDB $ \conn -> do
  quickQuery' conn "SELECT 1 FROM services WHERE id = ?" [toSql uid]
    >>= checkIfOneObjectReturned

selectServicesSQL :: String
selectServicesSQL = "SELECT"
  ++ "  s.id"
  ++ ", encode(s.password, 'base64')"
  ++ ", encode(s.salt, 'base64')"
  ++ ", s.admin_id"
  ++ ", s.location"
  ++ ", s.email_from_address"
  ++ ", s.mail_footer"
  ++ ", encode(s.button1, 'base64')"
  ++ ", encode(s.button2, 'base64')"
  ++ ", s.buttons_text_color"
  ++ ", s.background"
  ++ ", s.overlay_background"
  ++ ", s.bars_background"
  ++ ", encode(s.logo, 'base64')"
  ++ "  FROM services s"
  ++ " "

fetchServices :: Statement -> IO [Service]
fetchServices st = foldDB st decoder []
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
