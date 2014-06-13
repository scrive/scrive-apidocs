module MessengerServerConf (
    MessengerServerConf(..)
  , SenderConfig(..)
  ) where

import Data.Word
import Configuration
import qualified Data.ByteString as BS

data MessengerServerConf = MessengerServerConf
  { mscHttpBindAddress :: (Word32, Word16)
  , mscDBConfig        :: BS.ByteString
  , mscMasterSender    :: SenderConfig
  } deriving (Read, Show)

data SenderConfig = GlobalMouthSender
  {
    gmSenderUser      :: String
  , gmSenderPassword  :: String
  , gmURL             :: String           -- "https://gw3.mcm.globalmouth.com:8443/api/mcm"
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
