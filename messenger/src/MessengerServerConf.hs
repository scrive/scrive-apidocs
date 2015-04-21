module MessengerServerConf (
    MessengerServerConf(..)
  , SenderConfig(..)
  , unjsonMessengerServerConf
  ) where

import Data.Default
import Data.Unjson
import Data.Word
import qualified Data.ByteString as BS

import KontraPrelude
import Log.Configuration
import Utils.Default
import Utils.TH

data MessengerServerConf = MessengerServerConf {
  mscHttpBindAddress :: !(Word32, Word16)
, mscDBConfig        :: !BS.ByteString
, mscLogConfig       :: !LogConfig
, mscMasterSender    :: !SenderConfig
} deriving (Eq, Ord, Show)

unjsonMessengerServerConf :: UnjsonDef MessengerServerConf
unjsonMessengerServerConf = objectOf $ MessengerServerConf
  <$> ((,)
    <$> fieldBy "bind_ip"
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
  <*> field "logging"
      mscLogConfig
      "Logging configuration"
  <*> field "master_sender"
      mscMasterSender
      "Master sender"

instance Unjson MessengerServerConf where
  unjsonDef = unjsonMessengerServerConf

data SenderConfig = GlobalMouthSender {
gmSenderUser       :: !String
, gmSenderPassword :: !String
, gmURL            :: !String -- "https://gw3.mcm.globalmouth.com:8443/api/mcm"
} | LocalSender {
  localDirectory   :: !FilePath
, localOpenCommand :: !(Maybe String)
} deriving (Eq, Ord, Show)

instance Unjson SenderConfig where
  unjsonDef = disjointUnionOf "type" [
      ("global_mouth", $(isConstr 'GlobalMouthSender), GlobalMouthSender
        <$> field "username"
            gmSenderUser
            "Username for GlobalMouth service"
        <*> field "password"
            gmSenderPassword
            "Password for GlobalMouth service"
        <*> field "url"
            gmURL
            "GlobalMouth address to contact"
      )
    , ("local", $(isConstr 'LocalSender), LocalSender
        <$> field "dir"
            localDirectory
            "Local directory to save 'sms' files"
        <*> fieldOpt "open"
            localOpenCommand
            "Local open command to open 'eml' files ('/usr/bin/open', 'gnome-open', 'kde-open')"
      )
    ]

instance HasDefaultValue MessengerServerConf where
  defaultValue = MessengerServerConf {
      mscHttpBindAddress = (0x7f000001, 6668)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscLogConfig = def
    , mscMasterSender = LocalSender {
        localDirectory = "/tmp"
      , localOpenCommand = Nothing
    }
  }
