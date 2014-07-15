module Payments.Config (RecurlyConfig(..)) where

import Control.Applicative
import Data.Unjson

data RecurlyConfig = RecurlyConfig { recurlySubdomain  :: String
                                   , recurlyAPIKey     :: String
                                   , recurlyPrivateKey :: String
                                   }
                   deriving (Show, Read, Eq, Ord)

unjsonRecurlyConfig :: UnjsonDef RecurlyConfig
unjsonRecurlyConfig = objectOf $ pure RecurlyConfig
  <*> field "subdomain"
      recurlySubdomain
      "Recurly subdomain"
  <*> field "api_key"
      recurlyAPIKey
      "Recurly API key"
  <*> field "private_key"
      recurlyPrivateKey
      "Recurly private key"

instance Unjson RecurlyConfig where
  unjsonDef = unjsonRecurlyConfig
