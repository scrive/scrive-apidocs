module User.Password where

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO)
import Crypto.RNG(CryptoRNG, randomBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Digest.SHA256 as D

import DB.Binary
import Crypto.RNG.Utils

data Password = Password {
    pwdHash :: Binary
  , pwdSalt :: Binary
  } deriving (Eq, Ord, Show)

createPassword :: (MonadIO m, CryptoRNG m) => String -> m Password
createPassword password = do
  salt <- Binary `liftM` randomBytes 10
  return Password {
      pwdHash = hashPassword password salt
    , pwdSalt = salt
  }

hashPassword :: String -> Binary -> Binary
hashPassword password = binApp (BS.pack . D.hash . BS.unpack . (`BS.append` BSU.fromString password))

verifyPassword :: Maybe Password -> String -> Bool
verifyPassword Nothing _ = False
verifyPassword (Just Password{pwdHash, pwdSalt}) password =
  pwdHash == hashPassword password pwdSalt

maybePassword :: (Maybe Binary, Maybe Binary) -> Maybe Password
maybePassword (Just hash, Just salt) = Just Password { pwdHash = hash, pwdSalt = salt }
maybePassword _ = Nothing

randomPassword :: (MonadIO m, CryptoRNG m) => m String
randomPassword = randomString 8 (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])
