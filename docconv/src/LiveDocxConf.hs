module LiveDocxConf (
    LiveDocxConf(..)
  ) where

import Utils.Default

data LiveDocxConf = LiveDocxConf {
    url :: String
  , serviceURL :: String
  , username :: String
  , password :: String
} deriving (Eq, Ord, Read, Show)

instance HasDefaultValue LiveDocxConf where
  defaultValue = LiveDocxConf {
      url = "https://api.livedocx.com/1.2/mailmerge.asmx"
    , serviceURL = "http://api.livedocx.com/2.1/mailmerge/"
    , username = "unittests"
    , password = "unitt3stsarenecessary"
  }
