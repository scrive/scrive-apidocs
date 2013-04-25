module MailingServerConf (
    MailingServerConf(..)
  , SenderConfig(..)
  ) where

import Data.Word
import Configuration
import Mails.Data

data MailingServerConf = MailingServerConf {
    mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: String
  , mscMasterSender    :: SenderConfig
  , mscSlaveSender     :: Maybe SenderConfig
  , mscAmazonConfig    :: Maybe (String, String, String)
  , testReceivers     :: [Address]
  } deriving (Read, Show)

data SenderConfig = SMTPSender {
    serviceName        :: String
  , smtpAddr           :: String
  , smtpUser           :: String
  , smtpPassword       :: String
  }
  | LocalSender {
    localDirectory     :: FilePath
  , localOpenCommand   :: Maybe String
  } deriving (Read, Show)

-- SMTPSender {
--     serviceName = "SendGrid"
--   , smtpAddr = "smtps://smtp.sendgrid.net"
--   , smtpUser= "duzyrak@gmail.com"
--   , smtpPassword = "zimowisko"
-- }

instance Configuration MailingServerConf where
  confDefault = MailingServerConf {
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
  confOptions = []
  confVerify _ = return $ Right ()
