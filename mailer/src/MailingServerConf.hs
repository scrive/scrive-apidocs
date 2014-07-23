module MailingServerConf (
    MailingServerConf(..)
  , SenderConfig(..)
  , SMTPUser(..)
  , SMTPDedicatedUser(..)
  ) where

import Data.Word
import Utils.Default
import Mails.Data
import qualified Data.ByteString as BS
import Data.Unjson
import Control.Applicative
import Data.Data

data MailingServerConf = MailingServerConf {
    mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: BS.ByteString
  , mscMasterSender    :: SenderConfig
  , mscSlaveSender     :: Maybe SenderConfig
  , mscAmazonConfig    :: Maybe (String, String, String)
  , testReceivers      :: [Address]
  } deriving (Read, Show)


unjsonMailingServerConf :: UnjsonDef MailingServerConf
unjsonMailingServerConf = objectOf $ pure MailingServerConf
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
      (unjsonAeson)
  <*> field "master_sender"
      mscMasterSender
      "Master sender"
  <*> fieldOpt "slave_sender"
      mscSlaveSender
      "Slave sender"
  <*> fieldOptBy "amazon"
      mscAmazonConfig
      "Amazon configuration"
      (objectOf $ pure (,,)
       <*> field "bucket"
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
    serviceName        :: String
  , smtpAddr           :: String
  , smtpUser           :: SMTPUser
  , smtpDedicatedUsers :: [SMTPDedicatedUser]
  }
  | LocalSender {
    localDirectory     :: FilePath
  , localOpenCommand   :: Maybe String
  }
  | NullSender
  deriving (Read, Show, Typeable, Data)

unjsonSenderConfig :: UnjsonDef SenderConfig
unjsonSenderConfig = DisjointUnjsonDef "type"
                     [("smtp", unjsonIsConstrByName "SMTPSender",
                       pure SMTPSender
                               <*> field "name"
                                   serviceName
                                   "Name of this sender service"
                               <*> field "smtp_addr"
                                   smtpAddr
                                   "SMTP address to contact"
                               <*> field "username"
                                   smtpUser
                                   "Username for SMTP service"
                               <*> field "password"
                                   smtpPassword
                                   "Password for SMTP service")
                     ,("local", unjsonIsConstrByName "LocalSender",
                       pure LocalSender
                               <*> field "dir"
                                   localDirectory
                                   "Local directory to save 'eml' files"
                               <*> fieldOpt "open"
                                   localOpenCommand
                                   "Local open command to open 'eml' files ('/usr/bin/open', 'gnome-open', 'kde-open')")
                     ,("null", unjsonIsConstrByName "NullSender",
                       pure NullSender)]

instance Unjson SenderConfig where
  unjsonDef = unjsonSenderConfig

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
    , mscMasterSender = LocalSender {
        localDirectory = "/tmp"
      , localOpenCommand = Nothing
    }
    , mscSlaveSender = Nothing
    , mscAmazonConfig = Nothing
    , testReceivers   = [Address { addrName = "test",   addrEmail = "your@email.scrive.com" }]
  }
