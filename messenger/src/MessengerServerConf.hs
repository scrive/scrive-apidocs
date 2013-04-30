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
  } deriving (Read, Show)

data SenderConfig = SMSSender
  { serviceName        :: String
  , smsSenderUser      :: String
  , smsSenderPassword  :: String
  }
  | LocalSender
  { localDirectory     :: FilePath
  , localOpenCommand   :: Maybe String
  } deriving (Read, Show)

instance Configuration MessengerServerConf where
  confDefault = MessengerServerConf {
      mscHttpBindAddress = (0x7f000001, 6668)
    , mscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
    , mscMasterSender = LocalSender {
        localDirectory = "/tmp"
      , localOpenCommand = Nothing
    }
  }
  confOptions = []
  confVerify _ = return $ Right ()
