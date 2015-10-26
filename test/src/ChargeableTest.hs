module ChargeableTest where

import Data.Int
import Test.Framework

import Company.Model
import CompanyAccounts.Model
import DB hiding (query, update)
import KontraPrelude
import MessageData
import SMS.Data (SMSProvider(..))
import SMS.SMS
import TestingUtil
import TestKontra as T

chargeableTest :: TestEnvSt -> Test
chargeableTest env = testGroup "Chargeable Items" [
    testThat "SMSes for default provider are counted properly" env test_smsCounting_default
  , testThat "SMSes for Telia CallGuide are counted properly" env test_smsCounting_telia
  ]

test_smsCounting_default :: TestEnv ()
test_smsCounting_default = do
  company <- addNewCompany
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  True <- dbUpdate $ SetUserCompany (userid user) (companyid company)
  doc <- addRandomDocument (randomDocumentAllowsDefault user)
  let sms = SMS {
        smsMSISDN = "+48666666666"
      , smsData = None
      , smsBody = ""
      , smsOriginator = "Scrive"
      , smsProvider = SMSDefault
      }

  scheduleSMS doc sms { smsBody = "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "One SMS was counted" (1::Int32)

  runSQL_ clearChargeable

  scheduleSMS doc sms { smsBody = intercalate ", " $ replicate 25 "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "Two SMSes were counted" (2::Int32)
  where
    clearChargeable = "DELETE FROM chargeable_items WHERE type = 1"
    selectQuantity  = "SELECT quantity FROM chargeable_items WHERE type = 1"

test_smsCounting_telia :: TestEnv ()
test_smsCounting_telia = do
  company <- addNewCompany
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  True <- dbUpdate $ SetUserCompany (userid user) (companyid company)
  doc <- addRandomDocument (randomDocumentAllowsDefault user)
  let sms = SMS {
        smsMSISDN = "+48666666666"
      , smsData = None
      , smsBody = ""
      , smsOriginator = "Scrive"
      , smsProvider = SMSTeliaCallGuide
      }

  scheduleSMS doc sms { smsBody = "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "One SMS was counted" (1::Int32)

  runSQL_ clearChargeable

  scheduleSMS doc sms { smsBody = intercalate ", " $ replicate 25 "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "Two SMSes were counted" (2::Int32)
  where
    clearChargeable = "DELETE FROM chargeable_items WHERE type = 5"
    selectQuantity  = "SELECT quantity FROM chargeable_items WHERE type = 5"
