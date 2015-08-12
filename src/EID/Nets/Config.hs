module EID.Nets.Config where

import Data.Text
import Data.Unjson

import KontraPrelude

data NetsConfig = NetsConfig {
    netsMerchantIdentifier     :: Text
  , netsMerchantPassword       :: Text
  , netsIdentifyUrl            :: Text
  , netsAssertionUrl           :: Text
} deriving (Eq, Ord, Show)

instance Unjson NetsConfig where
  unjsonDef = objectOf $ NetsConfig
    <$> field "mid"
        netsMerchantIdentifier
        "Nets merchant identifier"
    <*> field "password"
        netsMerchantPassword
        "Nets merchant password"
    <*> field "identifyUrl"
        netsIdentifyUrl
        "Nets identify url"
    <*> field "assertionUrl"
        netsAssertionUrl
        "Nets assertion url"
