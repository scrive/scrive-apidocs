{-# OPTIONS_GHC -fno-warn-orphans #-}

module User.Password.Internal where

import Data.Function
import qualified Crypto.Scrypt as Scrypt
import qualified Data.ByteString as BS

import KontraPrelude

instance Ord Scrypt.EncryptedPass where
  compare = compare `on` Scrypt.getEncryptedPass

-- | An encrypted password. Two password schemes are supported:
-- current and legacy.
data Password = Password       { pwdEncPass    :: !Scrypt.EncryptedPass
                               , pwdSHA256Salt :: !BS.ByteString }
                -- ^ Current password scheme, SHA256 + scrypt.

              | LegacyPassword { pwdHash'      :: !BS.ByteString
                               , pwdSalt'      :: !BS.ByteString }
                -- ^ Legacy password scheme, single round SHA256.
              deriving (Eq, Ord)

instance Show Password where
  show Password       {} = "Password (SHA256 + scrypt, hash and salt hidden)"
  show LegacyPassword {} = "Password (legacy SHA256, hash and salt hidden)"
