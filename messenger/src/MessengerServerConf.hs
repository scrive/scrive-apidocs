module MessengerServerConf (
    MessengerServerConf(..)
  , SenderConfig(..)
  ) where

import Data.Word
import Utils.Default
import qualified Data.ByteString as BS
import Data.Unjson
import Data.Maybe
import Control.Applicative

data MessengerServerConf = MessengerServerConf
  { mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: BS.ByteString
  , mscMasterSender    :: SenderConfig
  } deriving (Read, Show)

unjsonMessengerServerConf :: UnjsonDef MessengerServerConf
unjsonMessengerServerConf = objectOf $ pure MessengerServerConf
  <*> (pure (,)
         <*> fieldBy "bind_ip"
            (fst . mscHttpBindAddress)
            "IP to listen on, defaults to 0.0.0.0"
            unjsonIPv4AsWord32
         <*> field "bind_port"
            (snd . mscHttpBindAddress)
            "Port to listen on")
  <*> fieldBy "database"
      mscDBConfig
      "Database connection string"
      unjsonAeson
  <*> field "master_sender"
      mscMasterSender
      "Master sender"

instance Unjson MessengerServerConf where
  unjsonDef = unjsonMessengerServerConf

data SenderConfig = GlobalMouthSender
  {
    gmSenderUser      :: String
  , gmSenderPassword  :: String
  , gmURL             :: String           -- "https://gw3.mcm.globalmouth.com:8443/api/mcm"
  }
  | LocalSender
  { localDirectory     :: FilePath
  , localOpenCommand   :: Maybe String
  } deriving (Read, Show)

mkSenderConfig :: Maybe String
               -> Maybe String
               -> Maybe String
               -> Maybe String
               -> Maybe String
               -> SenderConfig
mkSenderConfig (Just gmURL') gmSenderUser' gmSenderPassword' _localDirectory' _localOpenCommand' =
  GlobalMouthSender { gmURL = gmURL',
                      gmSenderUser = fromMaybe "" gmSenderUser',
                      gmSenderPassword = fromMaybe "" gmSenderPassword' }

mkSenderConfig _serviceName' _smtpAddr' _smtpUser' localDirectory' localOpenCommand' =
  LocalSender { localDirectory = fromMaybe "" localDirectory',
                localOpenCommand = localOpenCommand' }

unjsonSenderConfig :: UnjsonDef SenderConfig
unjsonSenderConfig = objectOf $ pure mkSenderConfig
  <*> fieldOpt "url"
     (\s -> case s of
         GlobalMouthSender v _ _ -> Just v
         _ -> Nothing)
     "SMTP address to contact"
  <*> fieldOpt "username"
     (\s -> case s of
         GlobalMouthSender _ v _ -> Just v
         _ -> Nothing)
      "Username for GlobalMouth service"
  <*> fieldOpt "password"
     (\s -> case s of
         GlobalMouthSender _ _ v -> Just v
         _ -> Nothing)
      "Password for GlobalMouth service"
  <*> fieldOpt "dir"
     (\s -> case s of
         LocalSender v _ -> Just v
         _ -> Nothing)
      "Local directory to save 'eml' files to (ignored if SMTP)"
  <*> fieldOpt "open"
     (\s -> case s of
         LocalSender _ v -> v
         _ -> Nothing)
      "Local open command to open 'eml' files ('/usr/bin/open', 'gnome-open', 'kde-open') (ignored if SMTP)"

instance Unjson SenderConfig where
  unjsonDef = unjsonSenderConfig


instance HasDefaultValue MessengerServerConf where
  defaultValue = MessengerServerConf {
      mscHttpBindAddress = (0x7f000001, 6668)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscMasterSender = LocalSender {
        localDirectory = "/tmp"
      , localOpenCommand = Nothing
    }
  }
