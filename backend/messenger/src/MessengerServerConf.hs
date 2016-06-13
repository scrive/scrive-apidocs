module MessengerServerConf (
    MessengerServerConf(..)
  , sendersConfigFromMessengerConf
  , SendersConfig(..)
  , SenderConfig(..)
  , unjsonMessengerServerConf
  ) where

import Data.Default
import Data.Text (Text)
import Data.Unjson
import Data.Word

import KontraPrelude
import Log.Configuration
import SMS.Data
import Utils.TH

data MessengerServerConf = MessengerServerConf {
  mscHttpBindAddress :: !(Word32, Word16)
, mscDBConfig        :: !Text
, mscLogConfig       :: !LogConfig
, mscSenderDefault   :: !SenderConfig
, mscSenderTelia     :: !SenderConfig
} deriving (Eq, Show)

newtype SendersConfig = SendersConfig (SMSProvider -> SenderConfig)

sendersConfigFromMessengerConf :: MessengerServerConf -> SendersConfig
sendersConfigFromMessengerConf MessengerServerConf{..} = SendersConfig
  (\p -> case p of
    SMSDefault -> mscSenderDefault
    SMSTeliaCallGuide -> mscSenderTelia
  )

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
  <*> field "sender_default"
      mscSenderDefault
      "Default Sender configuration"
  <*> field "sender_telia"
      mscSenderTelia
      "Telia Sender configuration"

instance Unjson MessengerServerConf where
  unjsonDef = unjsonMessengerServerConf

data SenderConfig = GlobalMouthSender {
gmSenderUser       :: !String
, gmSenderPassword :: !String
, gmURL            :: !String -- "https://gw3.mcm.globalmouth.com:8443/api/mcm"
} | MbloxSender {
  mbToken          :: !String
, mbURL            :: !String -- "https://api.mblox.com/xms/v1/{username}/batches"
} | TeliaCallGuideSender {
  tcgSenderUrl      :: !String -- "https://sms.ccs.teliasonera.com/smsplus/smsextended"
, tcgSenderUser     :: !String
, tcgSenderPassword :: !String
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
    , ("mblox", $(isConstr 'MbloxSender), MbloxSender
        <$> field "token"
            mbToken
            "Mblox api token"
        <*> field "url"
            mbURL
            "Mblox url, with username embedded"
      )
    , ("telia_callguide", $(isConstr 'TeliaCallGuideSender), TeliaCallGuideSender
        <$> field "url"
            tcgSenderUrl
            "URL for Telia CallGuide service"
        <*> field "username"
            tcgSenderUser
            "Username for Telia CallGuide service"
        <*> field "password"
            tcgSenderPassword
            "Password for Telia CallGuide service"
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

instance Default MessengerServerConf where
  def = MessengerServerConf {
      mscHttpBindAddress = (0x7f000001, 6668)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscLogConfig = def
    , mscSenderDefault = LocalSender {
        localDirectory = "/tmp/default"
      , localOpenCommand = Nothing
    }
    , mscSenderTelia = LocalSender {
        localDirectory = "/tmp/telia"
      , localOpenCommand = Nothing
    }
  }
