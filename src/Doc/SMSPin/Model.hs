module Doc.SMSPin.Model (
    GetSignatoryPin(..)
  ) where

import Crypto.RNG
import DB
import Doc.SignatoryLinkID
import KontraMonad

data GetSignatoryPin = GetSignatoryPin SignatoryLinkID String
instance (KontraMonad m, MonadDB m,CryptoRNG m) => DBQuery m GetSignatoryPin String where
  query (GetSignatoryPin slid phone) = do
    runQuery_ . sqlSelect "signatory_sms_pins" $ do
      sqlResult "pin"
      sqlWhereEq "signatory_link_id" slid
      sqlWhereEq "phone_number" phone
    mpin <- fetchMaybe unSingle
    case mpin of
         Just pin -> return pin
         Nothing -> do
            pin' <- fmap show $ randomR (100000,999999)
            runQuery_ $ sqlInsert "signatory_sms_pins" $ do
              sqlSet "signatory_link_id" slid
              sqlSet "phone_number" phone
              sqlSet "pin" $ pin'
            return pin'
