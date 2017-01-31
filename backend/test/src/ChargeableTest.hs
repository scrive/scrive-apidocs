module ChargeableTest where

import Control.Monad.Trans
import Data.Int
import Happstack.Server
import Test.Framework
import qualified Data.ByteString as BS

import Company.Model
import CompanyAccounts.Model
import Context
import DB hiding (query, update)
import Doc.API.V1.Calls
import Doc.API.V2.Calls
import Doc.DocStateData
import Doc.DocumentMonad (withDocument)
import Doc.Model
import KontraPrelude
import MessageData
import SMS.Data (SMSProvider(..))
import SMS.SMS
import TestingUtil
import TestKontra as T
import Util.Actor
import Util.SignatoryLinkUtils

chargeableTest :: TestEnvSt -> Test
chargeableTest env = testGroup "Chargeable Items" [
    testThat "SMSes for default provider are counted properly" env test_smsCounting_default
  , testThat "SMSes for Telia CallGuide are counted properly" env test_smsCounting_telia
  , testThat "Starting a document with V1/V2 adds a chargeable item" env test_startDocumentCharging
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

test_startDocumentCharging :: TestEnv ()
test_startDocumentCharging = do
  company <- addNewCompany
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  True <- dbUpdate $ SetUserCompany (userid user) (companyid company)
  ctxWithUser <- (\c -> c { ctxmaybeuser = Just user })<$> mkContext def

  did1 <- newDocumentReadyToStart user
  req1 <- mkRequest POST []
  (_, _) <- runTestKontra req1 ctxWithUser $ apiCallV1Ready $ did1
  runSQL_ $ "SELECT count(*) FROM chargeable_items WHERE type = 6 AND quantity = 1 AND company_id = " <?> (companyid company)
  fetchOne runIdentity >>= assertEqual "Company was charged for starting one document" (1::Int64)

  did2 <- newDocumentReadyToStart user
  req2 <- mkRequest POST []
  (_, _) <- runTestKontra req2 ctxWithUser $ docApiV2Start $ did2
  runSQL_ $ "SELECT count(*) FROM chargeable_items WHERE type = 6 AND quantity = 1 AND company_id = " <?> (companyid company)
  fetchOne runIdentity >>= assertEqual "Company was charged for starting other document" (2::Int64)

  where
    newDocumentReadyToStart user = do
      let filename = inTestDir "pdfs/simple.pdf"
      filecontent <- liftIO $ BS.readFile filename
      file <- addNewFile filename filecontent
      doc <- addRandomDocumentWithAuthorAndConditionAndFile
            user
            (\d -> documentstatus d == Preparation
                     && (case documenttype d of
                          Signable -> True
                          _ -> False)
                     && all ((==) EmailDelivery . signatorylinkdeliverymethod) (documentsignatorylinks d))
            file

      True <- withDocument doc $ do
        randomUpdate $ ResetSignatoryDetails ([
              (def {   signatoryfields = (signatoryfields $ fromJust $ getAuthorSigLink doc)
                              , signatoryisauthor = True
                              , signatoryispartner = False
                              , maybesignatory = Just $ userid user })
            , (def {   signatorysignorder = SignOrder 1
                              , signatoryisauthor = False
                              , signatoryispartner = True
                              , signatoryfields = [
                                  fieldForTests (NameFI (NameOrder 1)) "Fred"
                                , fieldForTests (NameFI (NameOrder 2)) "Frog"
                                , fieldForTests EmailFI "fred@frog.com"
                                ]})
          ]) (systemActor $ documentctime doc)
      return $ documentid doc
