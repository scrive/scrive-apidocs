module Doc.SMSPin.Model (
    SMSPinType(..)
  , authKindToPinType
  , GetSignatoryPin(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Crypto.RNG
import Data.Int
import Data.Time
import Log
import qualified Data.Text as T

import DB
import Doc.SignatoryLinkID
import Doc.Types.SignatoryLink

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

data GetSignatoryPin = GetSignatoryPin SMSPinType SignatoryLinkID Text

instance (MonadLog m, MonadDB m, MonadThrow m, MonadTime m, CryptoRNG m) => DBQuery m GetSignatoryPin Text where
  dbQuery (GetSignatoryPin pintype slid phone) = do
    runQuery_ . sqlSelect "signatory_sms_pins" $ do
      sqlResult "pin"
      sqlResult "generated_at"
      sqlWhereEq "signatory_link_id" slid
      sqlWhereEq "phone_number"      phone
      sqlWhereEq "pin_type"          pintype
    mpin <- fetchMaybe identity
    now  <- currentTime
    case mpin of
      Just (pin, generated_at) -> if now `diffUTCTime` generated_at <= validityWindow
        then return pin
        else do
          newPin <- show <$> randomR (1000, 9999)
          logInfo "Generating new pin" $ object
            ["new pin" .= newPin, "reason" .= ("previous one was too old" :: String)]
          runQuery_ . sqlUpdate "signatory_sms_pins" $ do
            sqlSet "pin"          newPin
            sqlSet "generated_at" now
            sqlWhereEq "signatory_link_id" slid
            sqlWhereEq "phone_number"      phone
            sqlWhereEq "pin_type"          pintype
          return $ T.pack newPin
      Nothing -> do
        newPin <- show <$> randomR (1000, 9999)
        logInfo "Generating new pin"
          $ object ["new pin" .= newPin, "reason" .= ("no previous pin" :: String)]
        runQuery_ . sqlInsert "signatory_sms_pins" $ do
          sqlSet "pin"               newPin
          sqlSet "generated_at"      now
          sqlSet "signatory_link_id" slid
          sqlSet "phone_number"      phone
          sqlSet "pin_type"          pintype
        return $ T.pack newPin
    where
      validityWindow :: NominalDiffTime
      -- PIN is valid for 60 minutes from its generation.
      validityWindow = 60 * 60
