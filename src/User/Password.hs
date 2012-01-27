module User.Password where

import Control.Monad.IO.Class
import Crypto.GlobalRandom(genBytesIO)
import qualified Data.ByteString as BS
import qualified Data.Digest.SHA256 as D

import DB.Types

data Password = Password {
    pwdHash :: Binary
  , pwdSalt :: Binary
  } deriving (Eq, Ord, Show)

createPassword :: MonadIO m => BS.ByteString -> m Password
createPassword password = liftIO $ do
  salt <- Binary `fmap` genBytesIO 10
  return Password {
      pwdHash = hashPassword password salt
    , pwdSalt = salt
  }

hashPassword :: BS.ByteString -> Binary -> Binary
hashPassword password salt =
  Binary . BS.pack . D.hash . BS.unpack $ unBinary salt `BS.append` password

verifyPassword :: Maybe Password -> BS.ByteString -> Bool
verifyPassword Nothing _ = False
verifyPassword (Just Password{pwdHash, pwdSalt}) password =
  pwdHash == hashPassword password pwdSalt

maybePassword :: (Maybe Binary, Maybe Binary) -> Maybe Password
maybePassword (Just hash, Just salt) = Just Password { pwdHash = hash, pwdSalt = salt }
maybePassword _ = Nothing
