module SMS.SMS (
    SMS(..)
  , KontraInfoForSMS(..)
  , AddKontraInfoForSMS(..)
  , GetKontraInfoForSMS(..)
  , scheduleSMS
  ) where

import Control.Monad.Catch
import Data.Char
import Data.String.Utils
import Log

import Chargeable.Model
import DB
import Doc.DocStateData
import Log.Identifier
import SMS.Data (SMSProvider(..))
import SMS.KontraInfoForSMS
import SMS.Model

data SMS = SMS {
    smsMSISDN     :: String -- ^ Number of recipient in international form (+NNXXYYYYYYY)
  , kontraInfoForSMS :: Maybe KontraInfoForSMS -- ^ Connection between this message and and some entity in kontrakcja
  , smsBody       :: String -- ^ Message body
  , smsOriginator :: String -- ^ SMS originator/sender name
  , smsProvider   :: SMSProvider -- ^ SMS provider type
  } deriving (Eq, Ord, Show)

-- | Schedule SMS sendout. Note that body/originator needs
-- to be converted to latin1 as sms provider supports latin1 messages only.
-- Transliterate is used since eg. polish characters are not supported by
-- latin1, but we still want messages containing such characters to be sent
-- successfully.
scheduleSMS :: (MonadLog m, MonadDB m, MonadThrow m) => Document -> SMS -> m ()
scheduleSMS doc SMS{..} = do
  when (null smsMSISDN) $ do
    unexpectedError "no mobile phone number defined"
  sid <- dbUpdate $ CreateSMS smsProvider (fixOriginator smsOriginator) smsMSISDN smsBody
  -- charge company of the author of the document for the smses
  dbUpdate $ ChargeCompanyForSMS (documentid doc) smsProvider sms_count
  case kontraInfoForSMS of
    Nothing -> return ()
    Just kifs -> void $ dbUpdate $ AddKontraInfoForSMS sid kifs
  logInfo "SMS scheduled for sendout" $ object [
      identifier_ $ documentid doc
    , identifier_ sid
    , "sms_msisdn" .= smsMSISDN
    , "sms_info" .= (logObject_ <$> kontraInfoForSMS)
    , "sms_body" .= smsBody
    , "sms_originator" .= smsOriginator
    , "sms_provider" .= show smsProvider
    ]
  where
    -- Count the real smses; if the message length is less than
    -- 160 characters, it's 1 sms. Otherwise it's split into
    -- multiple parts. Headers are attached to each part to be
    -- able to reconstruct the sms, therefore in this case it's
    -- only 153 characters per sms (GSM encoding). Note that we
    -- are not using multibyte encoding for sms, therefore using
    -- length on a String here makes sense.
    -- Source: https://en.wikipedia.org/wiki/Concatenated_SMS
    sms_count = case fromIntegral $ length smsBody of
      len | len > 160 -> case len `divMod` 153 of
                           (count, 0) -> count
                           (count, _) -> count + 1
      _ -> 1

fixOriginator :: String -> String
fixOriginator s = notEmpty $ map fixChars $ take 11 s
  where
   fixChars c = if (isAlphaNum c ||isSpace c)
                 then c
                 else ' '
   notEmpty s' = case (strip s') of
                   "" -> "Scrive"
                   v  -> v
