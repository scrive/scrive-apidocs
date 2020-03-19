{-# LANGUAGE TemplateHaskell #-}
module MailingServerConf (
    MailingServerConf(..)
  , SenderConfig(..)
  , SMTPUser(..)
  , SMTPDedicatedUser(..)
  , CallbackValidationKeys(..)
  , unjsonMailingServerConf
  ) where

import Data.Unjson
import Data.Word

import Database.Redis.Configuration
import FileStorage.Amazon.Config
import Log.Configuration
import Mails.Types
import Monitoring
import Utils.TH

data MailingServerConf = MailingServerConf
  { mailerHttpBindAddress    :: !(Word32, Word16)
  , mailerDBConfig           :: !Text
  , mailerMaxDBConnections   :: !Int
  , mailerRedisCacheConfig   :: !(Maybe RedisConfig)
  , mailerLocalFileCacheSize :: !Int
  , mailerLogConfig          :: !LogConfig
  , mailerMasterSender       :: !SenderConfig
  , mailerSlaveSender        :: !(Maybe SenderConfig)
  , mailerAmazonConfig       :: !AmazonConfig
  , mailerTestReceivers      :: ![Address]
  , mailerMonitoringConfig   :: !(Maybe MonitoringConf)
  } deriving (Eq, Show)

-- | SMTP callback key authentication will be used to receive callbacks
-- Right now it's used only for SocketLabs

data CallbackValidationKeys = CallbackValidationKeys
  { callbackValidationSecretKey :: !Text
  , callbackValidationValidationKey :: !Text
  } deriving (Eq, Ord, Show)

unjsonCallbackValidationKeys :: UnjsonDef CallbackValidationKeys
unjsonCallbackValidationKeys =
  objectOf
    $   CallbackValidationKeys
    <$> field "secret_key"
              callbackValidationSecretKey
              "Secret key for callback validation"
    <*> field "validation_key"
              callbackValidationValidationKey
              "Validation key for callback validation"


data SMTPUser = SMTPUser
  { smtpAccount  :: !Text
  , smtpPassword :: !Text
  , callbackValidationKeys :: !(Maybe CallbackValidationKeys)
  } deriving (Eq, Ord, Show)

unjsonSMTPUser :: UnjsonDef SMTPUser
unjsonSMTPUser =
  objectOf
    $   SMTPUser
    <$> field "smtp_account"  smtpAccount  "SMTP account name"
    <*> field "smtp_password" smtpPassword "SMTP account password"
    <*> fieldOptBy "callback_validation"
                   callbackValidationKeys
                   "Callback validation keys connected with this account"
                   unjsonCallbackValidationKeys

-- | SMTP user that is dedicated only to email
-- where from address matched given address.
data SMTPDedicatedUser = SMTPDedicatedUser
  { smtpFromDedicatedAddress :: !Text
  , smtpDedicatedUser        :: !SMTPUser
  } deriving (Eq, Ord, Show)

unjsonSMTPDedicatedUser :: UnjsonDef SMTPDedicatedUser
unjsonSMTPDedicatedUser =
  objectOf
    $   SMTPDedicatedUser
    <$> field "from_address"
              smtpFromDedicatedAddress
              "'From:' address for for which this credentials should be used"
    <*> fieldBy "user" smtpDedicatedUser "SMTP account credentials" unjsonSMTPUser

unjsonMailingServerConf :: UnjsonDef MailingServerConf
unjsonMailingServerConf =
  objectOf
    $   MailingServerConf
    <$> (   (,)
        <$> fieldBy "bind_ip"
                    (fst . mailerHttpBindAddress)
                    "IP to listen on, defaults to 0.0.0.0"
                    unjsonIPv4AsWord32
        <*> field "bind_port" (snd . mailerHttpBindAddress) "Port to listen on"
        )
    <*> field "database"           mailerDBConfig         "Database connection string"
    <*> field "max_db_connections" mailerMaxDBConnections "Database connections limit"
    <*> fieldOpt "redis_cache" mailerRedisCacheConfig "Redis cache configuration"
    <*> field "local_file_cache_size"
              mailerLocalFileCacheSize
              "Local file cache size in bytes"
    <*> field "logging"       mailerLogConfig    "Logging configuration"
    <*> field "master_sender" mailerMasterSender "Master sender"
    <*> fieldOpt "slave_sender" mailerSlaveSender "Slave sender"
    <*> field "amazon"         mailerAmazonConfig  "Amazon configuration"
    <*> field "test_receivers" mailerTestReceivers "Email addresses for testing services"
    <*> fieldOpt "monitoring"
                 mailerMonitoringConfig
                 "Configuration of the ekg-statsd-based monitoring."

instance Unjson MailingServerConf where
  unjsonDef = unjsonMailingServerConf

data SenderConfig = SMTPSender {
  serviceName        :: !Text
, smtpAddr           :: !Text
, smtpUser           :: !SMTPUser
, smtpDedicatedUsers :: ![SMTPDedicatedUser]
} | LocalSender {
  localDirectory     :: !FilePath
, localOpenCommand   :: !(Maybe Text)
} | NullSender
  deriving (Eq, Ord, Show)

instance Unjson SenderConfig where
  unjsonDef = disjointUnionOf
    "type"
    [ ( "smtp"
      , $(isConstr 'SMTPSender)
      , SMTPSender
      <$> field "name"      serviceName "Name of this sender service"
      <*> field "smtp_addr" smtpAddr    "SMTP address to contact"
      <*> fieldBy "user"
                  smtpUser
                  "SMTP account credentials for default SMTP service"
                  unjsonSMTPUser
      <*> fieldBy
            "dedicated_users"
            smtpDedicatedUsers
            "SMTP accounts credentials for SMTP services with dedicated 'From:' addresses"
            (arrayOf unjsonSMTPDedicatedUser)
      )
    , ( "local"
      , $(isConstr 'LocalSender)
      , LocalSender
      <$> field "dir" localDirectory "Local directory to save 'eml' files"
      <*> fieldOpt
            "open"
            localOpenCommand
            "Local open command to open 'eml' files ('/usr/bin/open', 'gnome-open', 'kde-open')"
      )
    , ("null", (== NullSender), pure NullSender)
    ]
