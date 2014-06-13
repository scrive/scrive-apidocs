module LiveDocxConf (
    LiveDocxConf(..)
  ) where

import Configuration

data LiveDocxConf = LiveDocxConf {
    url :: String
  , serviceURL :: String
  , username :: String
  , password :: String
} deriving (Eq, Ord, Read, Show)

instance Configuration LiveDocxConf where
  confDefault = LiveDocxConf {
      url = "https://api.livedocx.com/1.2/mailmerge.asmx"
    , serviceURL = "http://api.livedocx.com/2.1/mailmerge/"
    , username = "unittests"
    , password = "unitt3stsarenecessary"
  }
