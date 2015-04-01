module LiveDocxConf (
    LiveDocxConf(..)
  ) where

import Data.Unjson

import KontraPrelude
import Utils.Default

data LiveDocxConf = LiveDocxConf {
    url :: String
  , serviceURL :: String
  , username :: String
  , password :: String
} deriving (Eq, Ord, Show)

unjsonLiveDocxConf :: UnjsonDef LiveDocxConf
unjsonLiveDocxConf = objectOf $ pure LiveDocxConf
  <*> field "url"
      url
      "URL"
  <*> field "service_url"
      serviceURL
      "Service URL"
  <*> field "username"
      username
      "Username"
  <*> field "password"
      password
      "Password"

instance Unjson LiveDocxConf where
  unjsonDef = unjsonLiveDocxConf

instance HasDefaultValue LiveDocxConf where
  defaultValue = LiveDocxConf {
      url = "https://api.livedocx.com/1.2/mailmerge.asmx"
    , serviceURL = "http://api.livedocx.com/2.1/mailmerge/"
    , username = "unittests"
    , password = "unitt3stsarenecessary"
  }
