module LiveDocxConf (
    LiveDocxConf(..)
  ) where

import Configuration

data LiveDocxConf = LiveDocxConf {
    url :: String
  , username :: String
  , password :: String
} deriving (Eq, Ord, Read, Show)

instance Configuration LiveDocxConf where
  confDefault = LiveDocxConf {
      url = "https://api.livedocx.com/1.2/mailmerge.asmx"
    , username = "unittests"
    , password = "unitt3stsarenecessary"
  }
  confOptions = []
  confVerify _ = return $ Right ()
