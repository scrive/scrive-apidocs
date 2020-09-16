module Doc.SMSPin.Model (
    SMSPinType(..)
  , authKindToPinType
  , GetOrCreateSignatoryPin(..)
  , VerifySignatoryPin(..)
  , VerifySignatoryPinStatus(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Crypto.RNG
import Data.Aeson.Types
import Data.Int
import Data.String
import Data.Time
import Log
import Text.StringTemplates.Templates (TemplatesMonad)
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import DB
import Doc.DocumentMonad
import Doc.SignatoryLinkID
import Doc.Types.SignatoryLink
import EvidenceLog.Model
import Util.Actor

data SMSPinType
  = SMSPinToSign
  | SMSPinToView
  | SMSPinToViewArchived
  deriving (Eq, Show)

authKindToPinType :: AuthenticationKind -> SMSPinType
authKindToPinType AuthenticationToView         = SMSPinToView
authKindToPinType AuthenticationToViewArchived = SMSPinToViewArchived

instance PQFormat SMSPinType where
  pqFormat = pqFormat @Int32

instance FromSQL SMSPinType where
  type PQBase SMSPinType = PQBase Int32
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int32 of
      1 -> return SMSPinToSign
      2 -> return SMSPinToView
      3 -> return SMSPinToViewArchived
      _ -> throwM RangeError { reRange = [(1, 3)], reValue = n }

instance ToSQL SMSPinType where
  type PQDest SMSPinType = PQDest Int32
  toSQL SMSPinToSign         = toSQL (1 :: Int32)
  toSQL SMSPinToView         = toSQL (2 :: Int32)
  toSQL SMSPinToViewArchived = toSQL (3 :: Int32)

data VerifySignatoryPin = VerifySignatoryPin Text SMSPinType SignatoryLink Text Actor

data VerifySignatoryPinStatus
  = SignatoryPinCorrect -- ^ Signatory SMS PIN matches.
  | SignatoryPinIncorrect -- ^ Signatory SMS PIN does not match.
  | SignatoryPinOutdated -- ^ No valid signatory SMS PIN is available.
  | SignatoryPinExhausted -- ^ To many invalid attempts were used.
  deriving (Eq, Show)


instance (DocumentMonad m, MonadLog m, MonadThrow m, MonadTime m, TemplatesMonad m) => DBUpdate m VerifySignatoryPin VerifySignatoryPinStatus where
  dbUpdate (VerifySignatoryPin pin pintype sl phone actor) = do
    runQuery_ . sqlSelect "signatory_sms_pins" $ do
      sqlResult "pin"
      sqlResult "generated_at"
      sqlResult "attempts"
      sqlWhereEq "signatory_link_id" slid
      sqlWhereEq "phone_number"      phone
      sqlWhereEq "pin_type"          pintype
    mpin               <- fetchMaybe identity
    (status, attempts) <- case mpin of
      Just (pin', generated_at, attempts) -> do
        now <- currentTime
        let res = checkPin pin' now generated_at attempts
        logInfoSms pin' generated_at attempts res
        setAttempts $ if res == SignatoryPinCorrect then 0 else safeSucc attempts
        return (res, attempts)
      Nothing -> do
        logInfoSms Null Null 0 SignatoryPinOutdated
        return (SignatoryPinOutdated, 0)
    unless (status == SignatoryPinCorrect)
      . void
      . dbUpdate
      $ InsertEvidenceEventWithAffectedSignatoryAndMsg
          SMSPinVerificationFailed
          (do
            F.value "phone" phone
            F.value "pin_attempts" attempts
          )
          (Just sl)
          (Just . fromString $ show status)
          actor
    return status
    where
      slid = signatorylinkid sl

      checkPin pin' now generated_at attempts
        | isPinOutdated now generated_at = SignatoryPinOutdated
        | -- Order matters here. Always check attempts before pin equality.
          attempts >= maxAttempts = SignatoryPinExhausted
        | pin == pin'             = SignatoryPinCorrect
        | otherwise               = SignatoryPinIncorrect

      safeSucc n = if n == maxBound then maxBound else succ n

      setAttempts attempts = do
        logInfo "Updating SMS PIN attempt counter" $ object ["attempts" .= attempts]
        runQuery_ . sqlUpdate "signatory_sms_pins" $ do
          sqlSet "attempts" attempts
          sqlWhereEq "signatory_link_id" slid
          sqlWhereEq "phone_number"      phone
          sqlWhereEq "pin_type"          pintype

      maxAttempts :: Int32
      maxAttempts = 3

      logInfoSms
        :: (MonadLog m, ToJSON a, ToJSON b)
        => a
        -> b
        -> Int32
        -> VerifySignatoryPinStatus
        -> m ()
      logInfoSms expected_pin generated_at attempts status =
        logInfo "SMS PIN verification" $ object
          [ "pin" .= pin
          , "expected_pin" .= expected_pin
          , "status" .= show status
          , "attempts" .= attempts
          , "generated_at" .= generated_at
          ]

data GetOrCreateSignatoryPin = GetOrCreateSignatoryPin SMSPinType SignatoryLinkID Text

instance (MonadLog m, MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m) => DBUpdate m GetOrCreateSignatoryPin Text where
  dbUpdate (GetOrCreateSignatoryPin pintype slid phone) = do
    runQuery_ . sqlSelect "signatory_sms_pins" $ do
      sqlResult "pin"
      sqlResult "generated_at"
      sqlResult "attempts"
      sqlWhereEq "signatory_link_id" slid
      sqlWhereEq "phone_number"      phone
      sqlWhereEq "pin_type"          pintype
    now         <- currentTime
    current_pin <- fetchMaybe identity
    let (mpin, generated_at, attempts :: Int32) = case current_pin of
          Just (pin, generated_at', attempts')
            | -- Only reset attempts and validity when PIN is outdated.
              isPinOutdated now generated_at' -> (Nothing, now, 0)
            | -- Keep current PIN code and attempts when PIN is valid.
              otherwise -> (Just pin, generated_at', attempts')
          Nothing -> (Nothing, now, 0)
    (new_pin, log_message) <- case mpin of
      Just pin -> pure (pin, "Using existing PIN code")
      Nothing  -> do
        pin <- show <$> randomR (1000, 9999)
        pure (pin, "Generating new PIN code")
    logInfo log_message $ object
      [ "pin" .= new_pin
      , "pin_type" .= show pintype
      , "generated_at" .= show generated_at
      , "attempts" .= show attempts
      ]
    runQuery_ . sqlInsert "signatory_sms_pins" $ do
      sqlSet "pin"               new_pin
      sqlSet "generated_at"      generated_at
      sqlSet "attempts"          attempts
      sqlSet "signatory_link_id" slid
      sqlSet "phone_number"      phone
      sqlSet "pin_type"          pintype
      sqlOnConflictOnColumns ["signatory_link_id", "phone_number", "pin_type"]
        . sqlUpdate ""
        $ do
            sqlSet "pin"          new_pin
            sqlSet "generated_at" generated_at
            sqlSet "attempts"     attempts
    pure $ T.pack new_pin


isPinOutdated :: UTCTime -> UTCTime -> Bool
isPinOutdated now generated_at = now `diffUTCTime` generated_at > validityWindow
  where
    validityWindow :: NominalDiffTime
    -- PIN is valid for 60 minutes from its generation.
    validityWindow = 60 * 60
