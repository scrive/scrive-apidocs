{-# OPTIONS_GHC -fno-warn-orphans #-}
module User.Password ( Password(..) -- internals exported for testing
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
import qualified Crypto.Scrypt as Scrypt
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Digest.SHA256 as D

import KontraPrelude

instance Ord Scrypt.EncryptedPass where
  compare = compare `on` Scrypt.getEncryptedPass

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

pwdHash :: Password -> BS.ByteString
pwdHash Password{pwdEncPass}     = Scrypt.getEncryptedPass pwdEncPass
pwdHash LegacyPassword{pwdHash'} = pwdHash'

pwdSalt :: Password -> BS.ByteString
pwdSalt Password{pwdSHA256Salt}  = pwdSHA256Salt
pwdSalt LegacyPassword{pwdSalt'} = pwdSalt'

-- | Version of the password hashing algorithm used. 0 = legacy
-- SHA-256, 1 = scrypt.
pwdStrength :: Password -> Int16
pwdStrength Password{}       = 1
pwdStrength LegacyPassword{} = 0

-- | Default scrypt parameters are N = 14, r = 8, p = 1, we use
-- slightly larger N and p. N=15 gives us a 32M memory cost, p=2 then
-- doubles the CPU cost. This means ~180 ms password hashing time on a
-- 2016-level CPU, which I think is okay for our purposes.
--
-- Alternatives I've considered:
--
-- * N = 16, r = 8, p = 1  -- time cost is ~the same, memory cost is 64M per pass,
--                            which I think is a bit too much.
-- * N = 15, r = 8, p = 1  -- time cost is halved to ~100 ms, which is actually the
--                            recommended time cost by the author of scrypt. I
--                            chose to be a little bit more
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

-- | Deserialise a password from a DB row.
mkPassword :: BS.ByteString -> BS.ByteString -> Int16 -> Password
mkPassword hash salt strength = case strength of
  0 -> LegacyPassword hash salt
  1 -> Password (Scrypt.EncryptedPass hash) salt
  _ -> $unexpectedError "Password strength must be either 0 or 1!"

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

hashPasswordSHA256 :: String -> BS.ByteString -> BS.ByteString
hashPasswordSHA256 password = BS.pack . D.hash . BS.unpack
                              . (`BS.append` BSU.fromString password)

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

-- | Like 'mkPassword', but everything is wrapped in Maybe.
maybeMkPassword :: (Maybe BS.ByteString, Maybe BS.ByteString, Maybe Int16)
                -> Maybe Password
maybeMkPassword (mHash, mSalt, mStrength) = do
  hash     <- mHash
  salt     <- mSalt
  strength <- mStrength
  return $ mkPassword hash salt strength

randomPassword :: CryptoRNG m => m Password
randomPassword = randomPasswordString >>= createPassword

randomPasswordString :: CryptoRNG m => m String
randomPasswordString = randomString 32
  (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++
   ".,?!\'\":;@#$%^&*()")
