module MessengerServerConf (
    MessengerServerConf(..)
  , SenderConfig(..)
  ) where

import Data.Word
import Configuration

data MessengerServerConf = MessengerServerConf
  { mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: String
  , mscMasterSender    :: SenderConfig
  , mscSlaveSender     :: Maybe SenderConfig
  } deriving (Read, Show)

data SenderConfig = SMSSender
  { serviceName        :: String
  , smtpAddr           :: String
  , smtpUser           :: String
  , smtpPassword       :: String
  }
  | LocalSender
  { localDirectory     :: FilePath
  , localOpenCommand   :: Maybe String
  } deriving (Read, Show)

-- SMTPSender {
--     serviceName = "SendGrid"
--   , smtpAddr = "smtps://smtp.sendgrid.net"
--   , smtpUser= "duzyrak@gmail.com"
--   , smtpPassword = "zimowisko"
-- }

instance Configuration MessengerServerConf where
  confDefault = MessengerServerConf {
      mscHttpBindAddress = (0x7f000001, 6668)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscMasterSender = LocalSender {
        localDirectory = "/tmp"
      , localOpenCommand = Nothing
    }
    , mscSlaveSender = Nothing
  }
  confOptions = []
  confVerify _ = return $ Right ()
