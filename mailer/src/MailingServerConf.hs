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

data MailingServerConf = MailingServerConf {
    mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: BS.ByteString
  , mscMasterSender    :: SenderConfig
  , mscSlaveSender     :: Maybe SenderConfig
  , mscAmazonConfig    :: Maybe (String, String, String)
  , testReceivers     :: [Address]
  } deriving (Read, Show)

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
  deriving (Read, Show)

data SMTPUser = SMTPUser {
    smtpAccount  :: String
  , smtpPassword :: String
}  deriving (Read, Show)


-- SMTP user that is dedicated only to email where from address matched given address 
data SMTPDedicatedUser = SMTPDedicatedUser {
    smtpFromDedicatedAddress :: String
  , smtpDedicatedUser    :: SMTPUser
} deriving (Read, Show)


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
