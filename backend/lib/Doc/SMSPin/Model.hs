module Doc.SMSPin.Model (
    SMSPinType(..)
  , GetSignatoryPin(..)
  ) where

import Control.Monad.Catch
import Crypto.RNG
import Data.Int

import DB
import Doc.SignatoryLinkID

data SMSPinType = SMSPinToSign | SMSPinToView deriving (Eq, Show)

instance PQFormat SMSPinType where
  pqFormat = pqFormat @Int32

instance FromSQL SMSPinType where
  type PQBase SMSPinType = PQBase Int32
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int32 of
      1 -> return SMSPinToSign
      2 -> return SMSPinToView
      _ -> throwM RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL SMSPinType where
  type PQDest SMSPinType = PQDest Int32
  toSQL SMSPinToSign     = toSQL (1::Int32)
  toSQL SMSPinToView     = toSQL (2::Int32)

data GetSignatoryPin = GetSignatoryPin SMSPinType SignatoryLinkID String


instance (MonadDB m, MonadThrow m, CryptoRNG m) => DBQuery m GetSignatoryPin String where
  query (GetSignatoryPin pintype slid phone) = do
    runQuery_ . sqlSelect "signatory_sms_pins" $ do
      sqlResult "pin"
      sqlWhereEq "signatory_link_id" slid
      sqlWhereEq "phone_number" phone
      sqlWhereEq "pin_type" pintype
    mpin <- fetchMaybe runIdentity
    case mpin of
         Just pin -> return pin
         Nothing -> do
            pin' <- fmap show $ randomR (1000,9999)
            runQuery_ $ sqlInsert "signatory_sms_pins" $ do
              sqlSet "pin_type" $ pintype
              sqlSet "signatory_link_id" slid
              sqlSet "phone_number" phone
              sqlSet "pin" $ pin'
            return pin'
