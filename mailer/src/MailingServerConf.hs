module MailingServerConf (
    MailingServerConf(..)
  , SenderConfig(..)
  , SMTPUser(..)
  , SMTPDedicatedUser(..)
  , unjsonMailingServerConf
  ) where

import Data.Default
import Data.Unjson
import Data.Word
import qualified Data.ByteString as BS

import KontraPrelude
import Log.Configuration
import Mails.Data
import Utils.Default
import Utils.TH

data MailingServerConf = MailingServerConf {
  mscHttpBindAddress :: !(Word32, Word16)
, mscDBConfig        :: !BS.ByteString
, mscLogConfig       :: !LogConfig
, mscMasterSender    :: !SenderConfig
, mscSlaveSender     :: !(Maybe SenderConfig)
, mscAmazonConfig    :: !(Maybe (String, String, String))
, testReceivers      :: ![Address]
} deriving (Eq, Ord, Show)

data SMTPUser = SMTPUser {
  smtpAccount  :: !String
, smtpPassword :: !String
} deriving (Eq, Ord, Show)

unjsonSMTPUser :: UnjsonDef SMTPUser
unjsonSMTPUser = objectOf $ SMTPUser
  <$> field "smtp_account"
      smtpAccount
      "SMTP account name"
  <*> field "smtp_password"
      smtpPassword
      "SMTP account password"

-- | SMTP user that is dedicated only to email
-- where from address matched given address.
data SMTPDedicatedUser = SMTPDedicatedUser {
  smtpFromDedicatedAddress :: !String
, smtpDedicatedUser        :: !SMTPUser
} deriving (Eq, Ord, Show)

unjsonSMTPDedicatedUser :: UnjsonDef SMTPDedicatedUser
unjsonSMTPDedicatedUser = objectOf $ SMTPDedicatedUser
  <$> field "from_address"
      smtpFromDedicatedAddress
      "'From:' address for for which this credentials should be used"
  <*> fieldBy "user"
      smtpDedicatedUser
      "SMTP account credentials"
      unjsonSMTPUser

unjsonMailingServerConf :: UnjsonDef MailingServerConf
unjsonMailingServerConf = objectOf $ MailingServerConf
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
  <*> fieldOpt "slave_sender"
      mscSlaveSender
      "Slave sender"
  <*> fieldOptBy "amazon"
      mscAmazonConfig
      "Amazon configuration"
      (objectOf $ (,,)
        <$> field "bucket"
            (\(x,_,_) -> x)
            "In which bucket to store new files"
        <*> field "access_key"
            (\(_,x,_) -> x)
            "Amazon access key"
        <*> field "secret_key"
            (\(_,_,x) -> x)
            "Amazon secret key")
  <*> field "test_receivers"
      testReceivers
      "Email addresses for testing services"

instance Unjson MailingServerConf where
  unjsonDef = unjsonMailingServerConf

data SenderConfig = SMTPSender {
  serviceName        :: !String
, smtpAddr           :: !String
, smtpUser           :: !SMTPUser
, smtpDedicatedUsers :: ![SMTPDedicatedUser]
} | LocalSender {
  localDirectory     :: !FilePath
, localOpenCommand   :: !(Maybe String)
} | NullSender
  deriving (Eq, Ord, Show)

instance Unjson SenderConfig where
  unjsonDef = disjointUnionOf "type" [
      ("smtp", $(isConstr 'SMTPSender), SMTPSender
        <$> field "name"
            serviceName
            "Name of this sender service"
        <*> field "smtp_addr"
            smtpAddr
            "SMTP address to contact"
        <*> fieldBy "user"
            smtpUser
            "SMTP account credentials for default SMTP service"
            unjsonSMTPUser
        <*> fieldBy "dedicated_users"
            smtpDedicatedUsers
            "SMTP accounts credentials for SMTP services with dedicated 'From:' addresses"
            (arrayOf unjsonSMTPDedicatedUser)
      )
    , ("local", $(isConstr 'LocalSender), LocalSender
      <$> field "dir"
          localDirectory
          "Local directory to save 'eml' files"
      <*> fieldOpt "open"
          localOpenCommand
          "Local open command to open 'eml' files ('/usr/bin/open', 'gnome-open', 'kde-open')")
    , ("null", (== NullSender), pure NullSender)
    ]

-- SMTPSender {
--     serviceName = "SendGrid"
--   , smtpAddr = "smtps://smtp.sendgrid.net"
--   , smtpUser= "duzyrak@gmail.com"
--   , smtpPassword = "zimowisko"
-- }

instance HasDefaultValue MailingServerConf where
  defaultValue = MailingServerConf {
      mscHttpBindAddress = (0x7f000001, 6666)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscLogConfig = def
    , mscMasterSender = LocalSender {
        localDirectory = "/tmp"
      , localOpenCommand = Nothing
    }
    , mscSlaveSender = Nothing
    , mscAmazonConfig = Nothing
    , testReceivers = [
        Address {
          addrName = "test"
        , addrEmail = "your@email.scrive.com"
      }
    ]
  }
