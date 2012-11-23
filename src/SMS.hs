module SMS (
    SMS(..)
  , sms
  , scheduleSMS
  ) where

import Control.Applicative
import Data.Char
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSU

import Crypto.RNG
import DB
import MinutesTime
import SMS.Model
import qualified Log

data SMS = SMS {
    smsOriginator :: String -- ^ Message sender (at most 11 alphanumeric characters)
  , smsMSISDN     :: String -- ^ Number of recipient in international form (+NNXXYYYYYYY)
  , smsBody       :: String -- ^ Message body
  } deriving (Eq, Ord, Show)

-- | Empty SMS with 'Scrive' originator
sms :: SMS
sms = SMS {
    smsOriginator = "Scrive"
  , smsMSISDN = ""
  , smsBody = ""
  }

-- | Schedule SMS sendout. Note that body/originator needs
-- to be converted to latin1 as sms provider supports latin1 messages only.
-- Transliterate is used since eg. polish characters are not supported by
-- latin1, but we still want messages containing such characters to be sent
-- successfully.
scheduleSMS :: (CryptoRNG m, MonadDB m) => SMS -> m Bool
scheduleSMS msg@SMS{..} = case (validOriginator smsOriginator, validNumber smsMSISDN) of
  (True, True) -> do
    now <- getMinutesTime
    token <- random
    sid <- dbUpdate $ CreateSMS token (toLatin smsOriginator) smsMSISDN (toLatin smsBody) now
    Log.debug $ "SMS " ++ show msg ++ " with id #" ++ show sid ++ " scheduled for sendout"
    return True
  res -> do
    Log.error $ "SMS " ++ show msg ++ " discarded as (originator_ok, msisdn_ok) = " ++ show res
    return False
  where
    validOriginator = (&&) <$> (<= 11) . length <*> all ((||) <$> isAlphaNum <*> isSpace)

    validNumber ('+':rest) = length rest == 11 && all isDigit rest
    validNumber _ = False

    toLatin = BSC.unpack
            . IConv.convertFuzzy IConv.Transliterate "utf8" "latin1"
            . BSU.fromString
