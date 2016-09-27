module User.Password where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Digest.SHA256 as D

import Crypto.RNG (CryptoRNG, randomBytes)
import Crypto.RNG.Utils
import KontraPrelude

data Password = Password {
    pwdHash :: !BS.ByteString
  , pwdSalt :: !BS.ByteString
  } deriving (Eq, Ord)

instance Show Password where
  show _ = "Password (hash and salt are not visible for logging)"

createPassword :: CryptoRNG m => String -> m Password
createPassword password = do
  salt <- randomBytes 10
  return Password {
      pwdHash = hashPassword password salt
    , pwdSalt = salt
  }

hashPassword :: String -> BS.ByteString -> BS.ByteString
hashPassword password = BS.pack . D.hash . BS.unpack . (`BS.append` BSU.fromString password)

verifyPassword :: Maybe Password -> String -> Bool
verifyPassword Nothing _ = False
verifyPassword (Just Password{pwdHash, pwdSalt}) password =
  pwdHash == hashPassword password pwdSalt

maybePassword :: (Maybe BS.ByteString, Maybe BS.ByteString) -> Maybe Password
maybePassword (Just hash, Just salt) = Just Password { pwdHash = hash, pwdSalt = salt }
maybePassword _ = Nothing

randomPassword :: CryptoRNG m => m String
randomPassword = randomString 32 (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])
