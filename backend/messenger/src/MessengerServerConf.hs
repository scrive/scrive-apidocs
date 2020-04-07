{-# LANGUAGE TemplateHaskell #-}
module MessengerServerConf (
    MessengerServerConf(..)
  , sendersConfigFromMessengerConf
  , SendersConfig(..)
  , SenderConfig(..)
  , unjsonMessengerServerConf
  ) where

import Data.Text (Text)
import Data.Unjson
import Data.Word

import Log.Configuration
import Monitoring
import SMS.Types
import Utils.TH

data MessengerServerConf = MessengerServerConf
  { messengerHttpBindAddress  :: !(Word32, Word16)
  , messengerDBConfig         :: !Text
  , messengerMaxDBConnections :: !Int
  , messengerLogConfig        :: !LogConfig
  , messengerSenderDefault    :: !SenderConfig
  , messengerSenderTelia      :: !SenderConfig
  , messengerMonitoringConfig :: !(Maybe MonitoringConf)
  } deriving (Eq, Show)

newtype SendersConfig = SendersConfig (SMSProvider -> SenderConfig)

sendersConfigFromMessengerConf :: MessengerServerConf -> SendersConfig
sendersConfigFromMessengerConf MessengerServerConf {..} = SendersConfig $ \case
  SMSDefault        -> messengerSenderDefault
  SMSTeliaCallGuide -> messengerSenderTelia

unjsonMessengerServerConf :: UnjsonDef MessengerServerConf
unjsonMessengerServerConf =
  objectOf
    $   MessengerServerConf
    <$> (   (,)
        <$> fieldBy "bind_ip"
                    (fst . messengerHttpBindAddress)
                    "IP to listen on, defaults to 0.0.0.0"
                    unjsonIPv4AsWord32
        <*> field "bind_port" (snd . messengerHttpBindAddress) "Port to listen on"
        )
    <*> field "database"           messengerDBConfig         "Database connection string"
    <*> field "max_db_connections" messengerMaxDBConnections "Database connections limit"
    <*> field "logging"            messengerLogConfig        "Logging configuration"
    <*> field "sender_default" messengerSenderDefault "Default Sender configuration"
    <*> field "sender_telia"       messengerSenderTelia      "Telia Sender configuration"
    <*> fieldOpt "monitoring"
                 messengerMonitoringConfig
                 "Configuration of the ekg-statsd-based monitoring."

instance Unjson MessengerServerConf where
  unjsonDef = unjsonMessengerServerConf

data SenderConfig = MbloxSender
  { mbToken          :: !String
  , mbURL            :: !String
  -- ^ "https://api.mblox.com/xms/v1/{username}/batches"
  } | TeliaCallGuideSender
  { tcgSenderUrl      :: !String
  -- ^ "https://sms.ace.teliacompany.com/smsplus/smsextended"
  , tcgSenderUser     :: !String
  , tcgSenderPassword :: !String
  } | LocalSender
  { localDirectory   :: !FilePath
  , localOpenCommand :: !(Maybe String)
  } deriving (Eq, Ord, Show)

instance Unjson SenderConfig where
  unjsonDef = disjointUnionOf
    "type"
    [ ( "mblox"
      , $(isConstr 'MbloxSender)
      , MbloxSender <$> field "token" mbToken "Mblox api token" <*> field
        "url"
        mbURL
        "Mblox url, with username embedded"
      )
    , ( "telia_callguide"
      , $(isConstr 'TeliaCallGuideSender)
      , TeliaCallGuideSender
      <$> field "url"      tcgSenderUrl      "URL for Telia CallGuide service"
      <*> field "username" tcgSenderUser     "Username for Telia CallGuide service"
      <*> field "password" tcgSenderPassword "Password for Telia CallGuide service"
      )
    , ( "local"
      , $(isConstr 'LocalSender)
      , LocalSender
      <$> field "dir" localDirectory "Local directory to save 'sms' files"
      <*> fieldOpt
            "open"
            localOpenCommand
            "Local open command to open 'eml' files \
            \('/usr/bin/open', 'gnome-open', 'kde-open')"
      )
    ]
