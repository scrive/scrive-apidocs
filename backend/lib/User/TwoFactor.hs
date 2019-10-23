module User.TwoFactor (
    createTOTPKey
  , verifyTOTPCode
  , makeQRFromURLEmailAndKey
  ) where

import Crypto.Hash.Algorithms (SHA1(..))
import Crypto.RNG (CryptoRNG, randomBytes)
import Data.OTP (totpCheck)
import Data.Time (UTCTime)
import Data.Word (Word32)
import qualified Codec.Binary.Base32 as B32
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as TE

import User.Email (Email(..))
import Util.QRCode (QRCode, encodeQR)

createTOTPKey :: CryptoRNG m => m BS.ByteString
createTOTPKey = randomBytes 15

-- | Check the validity of a TOTP code for a given time.
--
-- Recommended values from RFC 6238:
--
-- * 5.2 Validation and Time-Step Size:
--     A validation system SHOULD typically set a policy for an acceptable OTP
--     transmission delay window for validation.
--     We RECOMMEND that at most one time step is allowed as the network delay.
--
-- In "5.3 Resynchronization", they suggest keeping track how "out of sync"
-- the client is. With most devices automatically set up to use NTP these days,
-- we will just go with 2 time-steps in either direction.
-- A liberal interpretation of the RFC recommendation.
--
-- ALso, the Google Authenticator app only supports 30 second windows and 6
-- digit codes, so we use that.
verifyTOTPCode :: BS.ByteString -> UTCTime -> Word32 -> Bool
verifyTOTPCode totpkey time code = totpCheck SHA1 totpkey (2, 2) time 30 6 code

makeQRFromURLEmailAndKey :: String -> Email -> BS.ByteString -> IO QRCode
makeQRFromURLEmailAndKey url email key =
  encodeQR . BC.unpack $ makeURIFromKey (BC.pack url) email key

makeURIFromKey :: BS.ByteString -> Email -> BS.ByteString -> BS.ByteString
makeURIFromKey url email key =
  -- URI Format is not specified in RFC, the de-facto standard is the one used
  -- by Google Authenticator:
  -- https://github.com/google/google-authenticator/wiki/Key-Uri-Format
  -- Example URI with all parameters:
  -- otpauth://totp/ACME%20Co:john.doe@email.com?secret=HXDMVJECJJWSRB3HWIZR4IFUGFTMXBOZ&issuer=ACME%20Co&algorithm=SHA1&digits=6&period=30
  "otpauth://totp/Scrive%20("
    `BS.append` url
    `BS.append` "):"
  -- Note: We add server to avoid conflicts, see
  -- https://github.com/google/google-authenticator/wiki/Conflicting-Accounts
    `BS.append` (TE.encodeUtf8 $ unEmail email)
    `BS.append` "?secret="
    `BS.append` base32Key
    `BS.append` "&issuer=Scrive&algorithm=SHA1&digits=6&period=30"
  where base32Key = B32.encode key
