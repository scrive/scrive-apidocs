module User.Password ( Password
                     , PasswordStrength(..)
                     , pwdStrengthToInt16
                     , int16ToPwdStrength
                     , pwdHash
                     , pwdSalt
                     , pwdStrength
                     , createPassword
                     , createLegacyPassword
                     , strengthenPassword
                     , mkPassword
                     , maybeMkPassword
                     , maybeVerifyPassword
                     , randomPassword
                     , randomPasswordString
                     , verifyPassword ) where

import Crypto.RNG (CryptoRNG, randomBytes)
import Crypto.RNG.Utils
import Data.Function
import Data.Int
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Scrypt as Scrypt
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU

import KontraPrelude
import User.Password.Internal

-- | Return the hash of the encrypted password. For non-legacy
-- passwords, this returns both the hash, the salt, and the scrypt
-- params in a single bytestring, separated by '|'. See
-- 'Crypto.Scrypt.EncryptedPass'. This form is useful for storing in a
-- database.
pwdHash :: Password -> BS.ByteString
pwdHash Password{pwdEncPass}     = Scrypt.getEncryptedPass pwdEncPass
pwdHash LegacyPassword{pwdHash'} = pwdHash'

-- | Return the salt used for the SHA256 hashing step.
pwdSalt :: Password -> BS.ByteString
pwdSalt Password{pwdSHA256Salt}  = pwdSHA256Salt
pwdSalt LegacyPassword{pwdSalt'} = pwdSalt'

-- | Version of the password hashing scheme used.
data PasswordStrength = PasswordStrengthLegacy
                      | PasswordStrengthCurrent
                      deriving (Eq, Show)

instance Ord PasswordStrength where
  compare PasswordStrengthLegacy  PasswordStrengthCurrent = LT
  compare PasswordStrengthCurrent PasswordStrengthLegacy  = GT
  compare PasswordStrengthLegacy  PasswordStrengthLegacy  = EQ
  compare PasswordStrengthCurrent PasswordStrengthCurrent = EQ

-- | Return the version of the password hashing scheme.
pwdStrength :: Password -> PasswordStrength
pwdStrength Password{}       = PasswordStrengthCurrent
pwdStrength LegacyPassword{} = PasswordStrengthLegacy

pwdStrengthToInt16 :: PasswordStrength -> Int16
pwdStrengthToInt16 PasswordStrengthLegacy  = 0
pwdStrengthToInt16 PasswordStrengthCurrent = 1

int16ToPwdStrength :: Int16 -> PasswordStrength
int16ToPwdStrength n | n <= 0    = PasswordStrengthLegacy
                     | otherwise = PasswordStrengthCurrent

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
kontrakcjaScryptParams = fromJust $ -- OK to crash here
                         Scrypt.scryptParams 15 8 2


-- | Encrypt a plain-text password.
createPassword :: CryptoRNG m => String -> m Password
createPassword = createLegacyPassword >=> strengthenPassword

-- | Encrypt a plain-text password using the legacy SHA-256 scheme.
createLegacyPassword :: CryptoRNG m => String -> m Password
createLegacyPassword password = do
  saltSHA256 <- randomBytes 10
  return LegacyPassword {
    pwdHash' = hashPasswordSHA256 password saltSHA256,
    pwdSalt' = saltSHA256
    }

-- | Deserialise a password from a DB row record.
mkPassword :: BS.ByteString -> BS.ByteString -> PasswordStrength -> Password
mkPassword hash salt PasswordStrengthLegacy  =
  LegacyPassword hash salt
mkPassword hash salt PasswordStrengthCurrent =
  Password (Scrypt.EncryptedPass hash) salt

-- | Convert a legacy SHA-256 password to a SHA-256 + scrypt one.
strengthenPassword :: CryptoRNG m => Password -> m Password
strengthenPassword p@Password{} = return p
strengthenPassword LegacyPassword{..} = do
  saltScrypt <- randomBytes 32
  return Password {
    pwdEncPass    = Scrypt.encryptPass kontrakcjaScryptParams
                    (Scrypt.Salt saltScrypt)
                    (Scrypt.Pass pwdHash'),
    pwdSHA256Salt = pwdSalt'
    }

-- | Hash a password using the legacy scheme with the provided salt.
hashPasswordSHA256 :: String -> BS.ByteString -> BS.ByteString
hashPasswordSHA256 password salt =
  SHA256.hash (salt `BS.append` BSU.fromString password)

-- | Verify a provided password string against an encrypted 'Password'.
verifyPassword :: Password -> String -> Bool
verifyPassword Password{pwdEncPass, pwdSHA256Salt} password =
  Scrypt.verifyPass'
  (Scrypt.Pass $ hashPasswordSHA256 password pwdSHA256Salt)
  pwdEncPass
verifyPassword LegacyPassword{pwdHash', pwdSalt'} password =
  pwdHash' == hashPasswordSHA256 password pwdSalt'

-- | Like 'verifyPassword', but the argument is wrapped in Maybe.
maybeVerifyPassword :: Maybe Password -> String -> Bool
maybeVerifyPassword Nothing _        = False
maybeVerifyPassword (Just hash) pass = verifyPassword hash pass

-- | Like 'mkPassword', but everything is wrapped in Maybe. If the
-- password strength parameter is Nothing, it defaults to legacy.
maybeMkPassword ::
  (Maybe BS.ByteString, Maybe BS.ByteString, Maybe PasswordStrength)
  -> Maybe Password
maybeMkPassword (mHash, mSalt, mStrength) =
  mkPassword <$> mHash <*> mSalt
  <*> (mStrength <|> Just PasswordStrengthLegacy)

randomPassword :: CryptoRNG m => m Password
randomPassword = randomPasswordString >>= createPassword

randomPasswordString :: CryptoRNG m => m String
randomPasswordString = randomString 32
  (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++
   ".,?!\'\":;@#$%^&*()")
