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
    ourInfoEmail         :: String
  , ourInfoEmailNiceName :: String
  , sendgridSMTP         :: String
  , sendgridRestAPI      :: String
  , sendgridUser         :: String
  , sendgridPassword     :: String
  }
  | MailsSendmail {
    ourInfoEmail         :: String
  , ourInfoEmailNiceName :: String
  }
  | MailsLocalOpen {
    ourInfoEmail         :: String
  , ourInfoEmailNiceName :: String
  } deriving (Read, Show)

instance Configuration MailingServerConf where
  confDefault = MailingServerConf {
      mscHttpBindAddress = (0x7f000001, 6666)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscMailsConfig = MailsLocalOpen {
        ourInfoEmail          = "development-system@skrivapa.se"
      , ourInfoEmailNiceName  = "Development"
      }
    }
  confOptions = []
  confVerify _ = return $ Right ()
