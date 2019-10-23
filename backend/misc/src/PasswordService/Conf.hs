module PasswordService.Conf (
    PasswordServiceConf(..)
  , defaultPasswordService
  ) where

import Data.Unjson

data PasswordServiceConf = PasswordServiceConf {
    passwordServiceUrl :: String
} deriving (Show, Eq, Ord)

unjsonPasswordServiceConf :: UnjsonDef PasswordServiceConf
unjsonPasswordServiceConf = objectOf $ pure PasswordServiceConf <*> field
  "password_service_url"
  passwordServiceUrl
  "PasswordService url"

defaultPasswordService :: PasswordServiceConf
defaultPasswordService = PasswordServiceConf "https://api.pwnedpasswords.com"

instance Unjson PasswordServiceConf where
  unjsonDef = unjsonPasswordServiceConf
