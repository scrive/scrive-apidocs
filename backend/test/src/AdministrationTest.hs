module AdministrationTest (administrationTests) where

import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON

import Administration.AdministrationControl
import DB
import TestingUtil
import TestKontra as T
import User.Email
import User.Lang (defaultLang)
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan

administrationTests :: TestEnvSt -> Test
administrationTests env = testGroup
  "AdministrationControl"
  [testThat "Searching for companies in adminonly works" env test_jsonCompanies]

test_jsonCompanies :: TestEnv ()
test_jsonCompanies = do
  (_adminuser1, _ug1) <- addNewAdminUserAndUserGroup "Anna" "Android" "anna@android.com"
  (adminuser2 , ug2 ) <- addNewAdminUserAndUserGroup "Jet" "Li" "jet.li@example.com"
  Just _standarduser2 <- addNewUserToUserGroup "Bob" "Blue" "jony@blue.com" (ugID ug2)
  void $ dbUpdate . UserGroupUpdate . set #ugInvoicing (Invoice OnePlan) $ ug2

  ctx <-
    (set #maybeUser (Just adminuser2) . set #adminAccounts [Email "jet.li@example.com"])
      <$> mkContext defaultLang

  req2 <- mkRequest
    GET
    [("nonFree", inText "true"), ("limit", inText "10"), ("offset", inText "0")]
  (rsp, _) <- runTestKontra req2 ctx jsonCompanies
  let JSObject rspJSON             = rsp
      Just     (JSArray companies) = lookup "companies" $ fromJSObject rspJSON
  assertEqual "Searching for non-free companies works" 1 (length companies)

  req3 <- mkRequest
    GET
    [("allCompanies", inText "true"), ("limit", inText "10"), ("offset", inText "0")]
  (rsp2, _) <- runTestKontra req3 ctx jsonCompanies
  let JSObject rspJSON2             = rsp2
      Just     (JSArray companies2) = lookup "companies" $ fromJSObject rspJSON2
  assertEqual "Searching for all companies works" 2 (length companies2)
