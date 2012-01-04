module MailingServerConf (
    MailingServerConf(..)
  , MailsConfig(..)
  ) where

import Data.Word
import Configuration

data MailingServerConf = MailingServerConf {
    mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: String
  , mscMailsConfig     :: MailsConfig
  } deriving (Read, Show)

data MailsConfig = MailsSendgrid {
    sendgridSMTP         :: String
  , sendgridRestAPI      :: String
  , sendgridUser         :: String
  , sendgridPassword     :: String
  }
  | MailsSendmail
  | MailsLocal {
    localDirectory       :: FilePath
  , localOpenCommand     :: Maybe String
  } deriving (Read, Show)

-- MailsSendgrid  {
--     sendgridSMTP = "smtps://smtp.sendgrid.net"
--   , sendgridRestAPI = "https://sendgrid.com/api"
--   , sendgridUser= "duzyrak@gmail.com"
--   , sendgridPassword = "zimowisko"
-- }

instance Configuration MailingServerConf where
  confDefault = MailingServerConf {
      mscHttpBindAddress = (0x7f000001, 6666)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscMailsConfig = MailsLocal {
        localDirectory = "/tmp"
      , localOpenCommand = Nothing
    }
  }
  confOptions = []
  confVerify _ = return $ Right ()
