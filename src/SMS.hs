module SMS (
    SMS(..)
  , sms
  , scheduleSMS
  ) where

import Crypto.RNG
import DB
import SMS.Model
import qualified Log
import Doc.SignatoryLinkID

data SMS = SMS {
    smsOriginator :: String -- ^ Message sender (at most 11 alphanumeric characters)
  , smsMSISDN     :: String -- ^ Number of recipient in international form (+NNXXYYYYYYY)
  , smsBody       :: String -- ^ Message body
  , smsSignatoryLinkID       :: Maybe SignatoryLinkID -- ^ Message body
  } deriving (Eq, Ord, Show)

-- | Empty SMS with 'Scrive' originator
sms :: SMS
sms = SMS {
    smsOriginator = "Scrive"
  , smsMSISDN = ""
  , smsBody = ""
  , smsSignatoryLinkID = Nothing
  }

-- | Schedule SMS sendout. Note that body/originator needs
-- to be converted to latin1 as sms provider supports latin1 messages only.
-- Transliterate is used since eg. polish characters are not supported by
-- latin1, but we still want messages containing such characters to be sent
-- successfully.
scheduleSMS :: (CryptoRNG m, MonadDB m) => SMS -> m Bool
scheduleSMS msg@SMS{..} = do
    now <- getMinutesTime
    token <- random
    sid <- dbUpdate $ CreateSMS token smsOriginator smsMSISDN smsBody smsSignatoryLinkID now
    Log.debug $ "SMS " ++ show msg ++ " with id #" ++ show sid ++ " scheduled for sendout"
    return True
