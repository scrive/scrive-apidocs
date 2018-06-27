module AdministrationTest (administrationTests) where

import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON

import Administration.AdministrationControl
import Administration.Invoicing
import Chargeable.Model
import Context
import DB
import Doc.DocInfo
import Doc.DocStateData
import MinutesTime
import TestingUtil
import TestKontra as T
import User.Email
import UserGroup.Data
import UserGroup.Data.PaymentPlan
import UserGroup.Model
import UserGroupAccountsTest
import Util.CSVUtil (CSV(..))

administrationTests :: TestEnvSt -> Test
administrationTests env = testGroup "AdministrationControl" [
                             testThat "Searching for companies in adminonly works" env test_jsonCompanies
                           , testThat "InvoicingReport doesn't trigger exception in a simple case" env test_invoicingReport
                          ]

test_jsonCompanies :: TestEnv ()
test_jsonCompanies = do
  (_adminuser1, _ug1) <- addNewAdminUserAndUserGroup "Anna" "Android" "anna@android.com"
  (adminuser2, ug2) <- addNewAdminUserAndUserGroup "Jet" "Li" "jet.li@example.com"
  Just _standarduser2 <- addNewUserToUserGroup "Bob" "Blue" "jony@blue.com" (get ugID ug2)
  _ <- dbUpdate . UserGroupUpdate . set ugInvoicing (Invoice OnePlan) $ ug2

  ctx <- (set ctxmaybeuser     (Just adminuser2) .
          set ctxadminaccounts [Email "jet.li@example.com"]) <$> mkContext def

  req2 <- mkRequest GET [ ("nonFree", inText "true")
                       , ("limit", inText "10")
                       , ("offset", inText "0")]
  (rsp, _) <- runTestKontra req2 ctx jsonCompanies
  let JSObject rspJSON = rsp
      Just (JSArray companies) = lookup "companies" $ fromJSObject rspJSON
  assertEqual "Searching for non-free companies works" 1 (length companies)

  req3 <- mkRequest GET [ ("allCompanies", inText "true")
                       , ("limit", inText "10")
                       , ("offset", inText "0")]
  (rsp2, _) <- runTestKontra req3 ctx jsonCompanies
  let JSObject rspJSON2 = rsp2
      Just (JSArray companies2) = lookup "companies" $ fromJSObject rspJSON2
  assertEqual "Searching for all companies works" 2 (length companies2)

test_invoicingReport:: TestEnv ()
test_invoicingReport = do
  (u1, ug1) <- addNewAdminUserAndUserGroup "Anna" "A1" "a1@android.com"
  (_, ug2) <- addNewAdminUserAndUserGroup "Anna" "A2" "a2@android.com"
  (_, ug3) <- addNewAdminUserAndUserGroup "Bob" "B1" "b1@example.com"
  _ <- addNewUserToUserGroup "Bob" "B2" "b2@blue.com" (get ugID ug3)
  _ <- dbUpdate $ UserGroupUpdate $ set ugInvoicing (Invoice OnePlan) $ ug1
  _ <- dbUpdate $ UserGroupUpdate $ set ugInvoicing (Invoice TeamPlan) $ ug2
  _ <- dbUpdate $ UserGroupUpdate $ set ugInvoicing (Invoice EnterprisePlan) $ ug3

  did1 <- addRandomDocumentWithAuthorAndCondition u1 (isClosed && isSignable)
  _ <- dbUpdate $ ChargeUserGroupForClosingDocument $ documentid did1

  ct <- currentTime
  csv <- dbQuery $ InvoicingReport $ 1 `daysAfter` ct
  assertBool "There are some rows in invoicing csv if there are some companies in DB" (length (csvContent csv) > 0)

