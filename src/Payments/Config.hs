module Payments.Config (RecurlyConfig(..)) where

data RecurlyConfig = RecurlyConfig { recurlySubdomain  :: String
                                   , recurlyAPIKey     :: String
                                   , recurlyPrivateKey :: String
                                   }
                   deriving (Show, Read, Eq, Ord)