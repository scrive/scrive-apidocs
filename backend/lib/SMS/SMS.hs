module SMS.SMS (
    SMS(..)
  , KontraInfoForSMS(..)
  , AddKontraInfoForSMS(..)
  , scheduleSMS
  ) where

import Control.Monad.Catch
import Data.Char
import Data.List.Extra
import Data.Set (Set, fromList, member)
import Log
import qualified Data.Text as T

import Chargeable
import DB
import Doc.DocStateData
import EventStream.Class
import Log.Identifier
import SMS.KontraInfoForSMS
import SMS.Model
import SMS.Types (SMSProvider(..))

data SMS = SMS {
    smsMSISDN     :: Text -- ^ Number of recipient in international form (+NNXXYYYYYYY)
  , kontraInfoForSMS :: Maybe KontraInfoForSMS -- ^ Connection between this message and and some entity in kontrakcja
  , smsBody       :: Text -- ^ Message body
  , smsOriginator :: Text -- ^ SMS originator/sender name
  , smsProvider   :: SMSProvider -- ^ SMS provider type
  } deriving (Eq, Ord, Show)

-- | Schedule SMS sendout. The SMS sendout provider takes care of detecting the
-- need to use UCS2 or GSM7 and the details of conversion, but unfortunately we
-- also need to distinguish between these formats for billing reasons.
scheduleSMS
  :: (MonadLog m, MonadDB m, MonadThrow m, MonadCatch m, MonadEventStream m)
  => Document
  -> SMS
  -> m ()
scheduleSMS doc SMS {..} = do
  when (T.null smsMSISDN) $ do
    unexpectedError "no mobile phone number defined"
  sid <- dbUpdate $ CreateSMS smsProvider
                              (T.pack . fixOriginator $ T.unpack smsOriginator)
                              smsMSISDN
                              smsBody
  -- charge company of the author of the document for the smses
  chargeForItemSimple (mapSMSToChargeableItem smsProvider) (documentid doc) smsCount
  case kontraInfoForSMS of
    Nothing   -> return ()
    Just kifs -> void . dbUpdate $ AddKontraInfoForSMS sid kifs
  logInfo "SMS scheduled for sendout" $ object
    [ identifier $ documentid doc
    , identifier sid
    , "sms_msisdn" .= smsMSISDN
    , "sms_info" .= (logObject_ <$> kontraInfoForSMS)
    , "sms_body" .= smsBody
    , "sms_originator" .= smsOriginator
    , "sms_provider" .= show smsProvider
    ]
  where
    -- Count the real smses; GSM encoding (UCS2 encoding numbers in
    -- parenthesis): if the message length is less than or equal to 160 (70)
    -- characters, it's 1 sms. Otherwise it's split into multiple parts. Headers
    -- are attached to each part to be able to reconstruct the sms, therefore in
    -- this case the maximum length is only 153 (67) characters per sms. The way
    -- we distinguish a UCS2 message from a GSM7 is that if all characters in
    -- the message body is in the string defined in `gsm7PermissibleChars`, its
    -- GSM7 - otherwise it's UCS2.
    --
    -- Note that currently the API endpoint for Telia CallGuide uses only GSM7
    -- by means of transliteration and simple omission.
    --
    -- Sources:
    -- - https://en.wikipedia.org/wiki/Concatenated_SMS
    -- - https://en.wikipedia.org/wiki/Universal_Coded_Character_Set
    smsBodyLength = fromIntegral . T.length $ smsBody
    smsCount      = if smsProvider == SMSTeliaCallGuide || isGSM7PermissibleString smsBody
      then countSMSes 160 153
      else countSMSes 70 67
    countSMSes maxForOne maxForMultiple
      | smsBodyLength > maxForOne = case smsBodyLength `divMod` maxForMultiple of
        (count, 0) -> count
        (count, _) -> count + 1
      | otherwise = 1

    mapSMSToChargeableItem :: SMSProvider -> ChargeableItem
    mapSMSToChargeableItem SMSDefault        = CISMS
    mapSMSToChargeableItem SMSTeliaCallGuide = CISMSTelia

isGSM7PermissibleString :: Text -> Bool
isGSM7PermissibleString = T.all (`member` gsm7PermissibleChars)
  where
    gsm7PermissibleChars :: Set Char
    gsm7PermissibleChars = fromList
      (  "@Δ0¡P¿p£_!1AQaq$Φ\"2BRbr¥Γ#3CScsèΛ¤4DTdtéΩ%5EUeuùΠ"
      <> "&6FVfvìΨ'7GWgwòΣ(8HXhxÇΘ)9IYiyΞ*:JZjzØ+;KÄkäøÆ,<LÖ"
      <> "löæ-=MÑmñÅß.>NÜnüåÉ/?O§oà \r\n\x1B"
      )

fixOriginator :: String -> String
fixOriginator s = notEmpty . map fixChars $ take 11 s
  where
    fixChars c = if isAlphaNum c || isSpace c then c else ' '
    notEmpty s' = case trim s' of
      "" -> "Scrive"
      v  -> v
