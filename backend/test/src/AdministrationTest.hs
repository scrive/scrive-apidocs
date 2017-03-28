module AdministrationTest (administrationTests) where

import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON

import Administration.AdministrationControl
import Company.Model
import CompanyAccountsTest
import Context
import DB hiding (query, update)
import KontraPrelude
import TestingUtil
import TestKontra as T
import User.Email

administrationTests :: TestEnvSt -> Test
administrationTests env = testGroup "AdministrationControl" [
                           testThat "Searching for companies in adminonly works" env test_jsonCompanies
                          ]

test_jsonCompanies :: TestEnv ()
test_jsonCompanies = do
  (_adminuser1, _company1) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  (adminuser2, company2) <- addNewAdminUserAndCompany "Jet" "Li" "jet.li@example.com"
  Just _standarduser2 <- addNewCompanyUser "Bob" "Blue" "jony@blue.com" (companyid company2)
  _ <- dbUpdate $ SetCompanyPaymentPlan (companyid company2) OnePlan

  ctx <- (\c -> c { ctxmaybeuser = Just adminuser2
                , ctxadminaccounts = [Email "jet.li@example.com"] }) <$> mkContext def

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
