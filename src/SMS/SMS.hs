module SMS.SMS (
    SMS(..)
  , scheduleSMS
  ) where

import Control.Monad.Catch
import Data.Char
import Data.String.Utils

import DB
import MessageData
import MinutesTime
import SMS.Model
import qualified Log

data SMS = SMS {
    smsMSISDN     :: String -- ^ Number of recipient in international form (+NNXXYYYYYYY)
  , smsData       :: MessageData -- ^ Message body
  , smsBody       :: String -- ^ Message body
  , smsOriginator :: String -- ^ SMS originator/sender name
  } deriving (Eq, Ord, Show)

-- | Schedule SMS sendout. Note that body/originator needs
-- to be converted to latin1 as sms provider supports latin1 messages only.
-- Transliterate is used since eg. polish characters are not supported by
-- latin1, but we still want messages containing such characters to be sent
-- successfully.
scheduleSMS :: (Log.MonadLog m, MonadDB m, MonadThrow m) => SMS -> m ()
scheduleSMS msg@SMS{..} = do
    if (smsMSISDN /= "")
       then do
        now <- currentTime
        sid <- dbUpdate $ CreateSMS (fixOriginator smsOriginator) (fixPhoneNumber smsMSISDN) smsBody (show smsData) now
        Log.mixlog_ $ "SMS " ++ show msg ++ " with id #" ++ show sid ++ " scheduled for sendout"
        return ()
       else do
        Log.attention_ $ "SMS:  trying to send SMS, but not mobile number was defined. This should not happend after mid II.2014. Till then we need to support it"
        return ()


fixPhoneNumber :: String -> String
fixPhoneNumber = filter goodChars
  where
    goodChars ' ' = False
    goodChars '-' = False
    goodChars '(' = False
    goodChars ')' = False
    goodChars _   = True


fixOriginator :: String -> String
fixOriginator s = notEmpty $ map fixChars $ take 11 s
  where
   fixChars c = if (isAlphaNum c ||isSpace c)
                 then c
                 else ' '
   notEmpty s' = case (strip s') of
                   "" -> "Scrive"
                   v  -> v
