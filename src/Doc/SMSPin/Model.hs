module Doc.SMSPin.Model (
    GetSignatoryPin(..)
  ) where

import Control.Monad.Catch

import Crypto.RNG
import DB
import Doc.SignatoryLinkID

data GetSignatoryPin = GetSignatoryPin SignatoryLinkID String
instance (MonadDB m, MonadThrow m, CryptoRNG m) => DBQuery m GetSignatoryPin String where
  query (GetSignatoryPin slid phone) = do
    runQuery_ . sqlSelect "signatory_sms_pins" $ do
      sqlResult "pin"
      sqlWhereEq "signatory_link_id" slid
      sqlWhereEq "phone_number" phone
    mpin <- fetchMaybe runIdentity
    case mpin of
         Just pin -> return pin
         Nothing -> do
            pin' <- fmap show $ randomR (1000,9999)
            runQuery_ $ sqlInsert "signatory_sms_pins" $ do
              sqlSet "signatory_link_id" slid
              sqlSet "phone_number" phone
              sqlSet "pin" $ pin'
            return pin'
