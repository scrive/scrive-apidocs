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
  | MailsLocalOpen
    deriving (Read, Show)

instance Configuration MailingServerConf where
  confDefault = MailingServerConf {
      mscHttpBindAddress = (0x7f000001, 6666)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscMailsConfig = MailsLocalOpen
  }
  confOptions = []
  confVerify _ = return $ Right ()
