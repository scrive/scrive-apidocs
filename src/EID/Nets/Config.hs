module EID.Nets.Config where

import Data.Unjson
import qualified Data.Text as T

import KontraPrelude

data NetsConfig = NetsConfig {
    netsMerchantIdentifier     :: T.Text
  , netsMerchantPassword       :: T.Text
  , netsIdentifyUrl            :: T.Text
  , netsAssertionUrl           :: T.Text
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
