module User.Password ( Password
                     , PasswordAlgorithm(..)
                     , pwdAlgorithmToInt16
                     , int16ToPwdAlgorithm
                     , pwdHash
                     , pwdSalt
                     , pwdAlgorithm
                     , createPassword
                     , mkPassword
                     , maybeMkPassword
                     , maybeVerifyPassword
                     , randomPassword
                     , randomPasswordString
                     , verifyPassword ) where

import Crypto.RNG (CryptoRNG, randomBytes)
import Crypto.RNG.Utils
import Data.Int
import qualified Crypto.Hash as H
import qualified Crypto.Scrypt as Scrypt
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import User.Password.Internal

-- | Return the hash of the encrypted password. This returns both the hash
-- , the salt, and the scrypt params in a single bytestring, separated by '|'.
-- See 'Crypto.Scrypt.EncryptedPass'. This form is useful for storing in a
-- database.
pwdHash :: Password -> BS.ByteString
pwdHash Password { pwdEncPass } = Scrypt.getEncryptedPass pwdEncPass

-- | Return the salt used for the SHA256 hashing step.
pwdSalt :: Password -> BS.ByteString
pwdSalt Password { pwdSHA256Salt } = pwdSHA256Salt

-- | Version of the password hashing scheme used.
data PasswordAlgorithm = PasswordAlgorithmScrypt
                      deriving (Eq, Ord, Show)

-- | Return the version of the password hashing scheme.
pwdAlgorithm :: Password -> PasswordAlgorithm
pwdAlgorithm Password{} = PasswordAlgorithmScrypt

pwdAlgorithmToInt16 :: PasswordAlgorithm -> Int16
pwdAlgorithmToInt16 PasswordAlgorithmScrypt = 1

int16ToPwdAlgorithm :: Int16 -> PasswordAlgorithm
int16ToPwdAlgorithm _ = PasswordAlgorithmScrypt

-- | Scrypt parameters used for hashing. Default scrypt parameters are
-- N = 14, r = 8, p = 1, we use slightly larger N and p. N=15 gives us
-- a 32M memory cost, p=2 then doubles the CPU cost. This means ~180
-- ms password hashing time on a 2016-level CPU, which I think is okay
-- for our purposes.
--
-- Alternatives I've considered:
--
-- * N = 16, r = 8, p = 1  -- time cost is ~the same, memory cost is 64M
--                            per pass, which I think is a bit too much.
-- * N = 15, r = 8, p = 1  -- time cost is halved to ~100 ms, which is actually
--                            the recommended time cost by the author of scrypt.
--                            I chose to be a little bit more
--                            conservative / future-proof.
--
-- See
-- <https://pthree.org/2016/06/29/further-investigation-into-scrypt-and-argon2-password-hashing/>
-- and <https://blog.filippo.io/the-scrypt-parameters/> for more
-- details.
kontrakcjaScryptParams :: Scrypt.ScryptParams
kontrakcjaScryptParams =
  fromJust $ -- OK to crash here
             Scrypt.scryptParams 15 8 2

-- | Encrypt a plain-text password.
createPassword :: CryptoRNG m => Text -> m Password
createPassword password = do
  saltSHA256 <- randomBytes 10
  let hash = hashPasswordSHA256 password saltSHA256
  saltScrypt <- randomBytes 32
  return Password
    { pwdEncPass    = Scrypt.encryptPass kontrakcjaScryptParams
                                         (Scrypt.Salt saltScrypt)
                                         (Scrypt.Pass hash)
    , pwdSHA256Salt = saltSHA256
    }

-- | Deserialise a password from a DB row record.
mkPassword :: BS.ByteString -> BS.ByteString -> PasswordAlgorithm -> Password
mkPassword hash salt PasswordAlgorithmScrypt = Password (Scrypt.EncryptedPass hash) salt

-- | Hash a password using the legacy scheme with the provided salt.
hashPasswordSHA256 :: Text -> BS.ByteString -> BS.ByteString
hashPasswordSHA256 password salt =
  BA.convert $ H.hashWith H.SHA256 (salt `BS.append` TE.encodeUtf8 password)

-- | Verify a provided password string against an encrypted 'Password'.
verifyPassword :: Password -> Text -> Bool
verifyPassword Password { pwdEncPass, pwdSHA256Salt } password =
  Scrypt.verifyPass' (Scrypt.Pass $ hashPasswordSHA256 password pwdSHA256Salt) pwdEncPass

-- | Like 'verifyPassword', but the argument is wrapped in Maybe.
maybeVerifyPassword :: Maybe Password -> Text -> Bool
maybeVerifyPassword Nothing     _    = False
maybeVerifyPassword (Just hash) pass = verifyPassword hash pass

-- | Like 'mkPassword', but everything is wrapped in Maybe. If the
-- password strength parameter is Nothing, it defaults to legacy.
maybeMkPassword
  :: Maybe BS.ByteString
  -> Maybe BS.ByteString
  -> Maybe PasswordAlgorithm
  -> Maybe Password
maybeMkPassword mHash mSalt mStrength =
  mkPassword <$> mHash <*> mSalt <*> (mStrength <|> Just PasswordAlgorithmScrypt)

randomPassword :: CryptoRNG m => m Password
randomPassword = randomPasswordString >>= createPassword

randomPasswordString :: CryptoRNG m => m Text
randomPasswordString = T.pack <$> randomString
  32
  (['0' .. '9'] <> ['A' .. 'Z'] <> ['a' .. 'z'] <> ".,?!\'\":;@#$%^&*()")
