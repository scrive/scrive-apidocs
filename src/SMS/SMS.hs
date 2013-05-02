module SMS.SMS (
    SMS(..)
  , scheduleSMS
  ) where

import DB
import SMS.Model
import qualified Log
import MessageData

data SMS = SMS {
    smsMSISDN     :: String -- ^ Number of recipient in international form (+NNXXYYYYYYY)
  , smsData       :: MessageData -- ^ Message body
  , smsBody       :: String -- ^ Message body
  } deriving (Eq, Ord, Show)


-- | Schedule SMS sendout. Note that body/originator needs
-- to be converted to latin1 as sms provider supports latin1 messages only.
-- Transliterate is used since eg. polish characters are not supported by
-- latin1, but we still want messages containing such characters to be sent
-- successfully.
scheduleSMS :: MonadDB m => SMS -> m ()
scheduleSMS msg@SMS{..} = do
    now <- getMinutesTime
    sid <- dbUpdate $ CreateSMS "Scrive" smsMSISDN smsBody (show smsData) now
    Log.debug $ "SMS " ++ show msg ++ " with id #" ++ show sid ++ " scheduled for sendout"
    return ()
