module Payments.Config (RecurlyConfig(..)) where

import Data.Unjson

import KontraPrelude

data RecurlyConfig = RecurlyConfig { recurlySubdomain  :: String
                                   , recurlyAPIKey     :: String
                                   , recurlyPrivateKey :: String
                                   }
                   deriving (Show, Eq, Ord)

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
