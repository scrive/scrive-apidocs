module ChargeableTest where

import Data.Int
import Data.List
import Test.Framework

import Company.Model
import CompanyAccounts.Model
import DB hiding (query, update)
import MessageData
import SMS.SMS
import TestingUtil
import TestKontra as T

chargeableTest :: TestEnvSt -> Test
chargeableTest env = testGroup "Chargeable Items" [
    testThat "SMSes are counted properly" env test_smsCounting
  ]

test_smsCounting :: TestEnv ()
test_smsCounting = do
  company <- addNewCompany
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  True <- dbUpdate $ SetUserCompany (userid user) (companyid company)
  doc <- addRandomDocument (randomDocumentAllowsDefault user)
  let sms = SMS {
        smsMSISDN = "+48666666666"
      , smsData = None
      , smsBody = ""
      , smsOriginator = "Scrive"
      }

  scheduleSMS doc sms { smsBody = "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "One SMS was counted" (1::Int32)

  runSQL_ clearChargeable

  scheduleSMS doc sms { smsBody = intercalate ", " $ replicate 25 "Test sms" }
  runSQL_ selectQuantity
  fetchOne runIdentity >>= assertEqual "Two SMSes were counted" (2::Int32)
  where
    clearChargeable = "DELETE FROM chargeable_items"
    selectQuantity  = "SELECT quantity FROM chargeable_items"
