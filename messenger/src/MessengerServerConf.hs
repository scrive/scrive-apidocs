module MessengerServerConf (
    MessengerServerConf(..)
  , SenderConfig(..)
  , unjsonMessengerServerConf
  ) where

import Data.Word
import Utils.Default
import qualified Data.ByteString as BS
import Data.Unjson
import Control.Applicative
import Data.Data

data MessengerServerConf = MessengerServerConf
  { mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: BS.ByteString
  , mscMasterSender    :: SenderConfig
  } deriving (Eq, Ord, Read, Show)

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
  } deriving (Eq, Ord, Read, Show, Typeable, Data)


unjsonSenderConfig :: UnjsonDef SenderConfig
unjsonSenderConfig = DisjointUnjsonDef "type"
                     [("global_mouth", unjsonIsConstrByName "GlobalMouthSender",
                       pure GlobalMouthSender
                       <*> field "username"
                       gmSenderUser
                       "Username for GlobalMouth service"
                       <*> field "password"
                       gmSenderPassword
                       "Password for GlobalMouth service"
                       <*> field "url"
                       gmURL
                       "GlobalMouth address to contact")
                     ,("local", unjsonIsConstrByName "LocalSender",
                       pure LocalSender
                       <*> field "dir"
                       localDirectory
                       "Local directory to save 'sms' files"
                       <*> fieldOpt "open"
                       localOpenCommand
                       "Local open command to open 'eml' files ('/usr/bin/open', 'gnome-open', 'kde-open')")]

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
