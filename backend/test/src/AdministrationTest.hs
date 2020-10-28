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
import User.Types.SignupMethod
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan

administrationTests :: TestEnvSt -> Test
administrationTests env = testGroup
  "AdministrationControl"
  [testThat "Searching for companies in adminonly works" env test_jsonCompanies]

test_jsonCompanies :: TestEnv ()
test_jsonCompanies = do
  _adminuser1 <- instantiateUser $ randomUserTemplate { firstName      = return "Anna"
                                                      , lastName       = return "Android"
                                                      , email = return "anna@android.com"
                                                      , isCompanyAdmin = True
                                                      , signupMethod   = CompanyInvitation
                                                      }
  ug2        <- instantiateRandomFreeUserGroup
  adminuser2 <- instantiateUser $ randomUserTemplate { firstName = return "Jet"
                                                     , lastName = return "Li"
                                                     , email = return "jet.li@example.com"
                                                     , groupID = return $ ug2 ^. #id
                                                     , isCompanyAdmin = True
                                                     , signupMethod = CompanyInvitation
                                                     }
  _standarduser2 <- instantiateUser $ randomUserTemplate { groupID = return $ ug2 ^. #id }
  void . dbUpdate . UserGroupUpdate . set #invoicing (Invoice EnterprisePlan) $ ug2

  ctx <-
    set #maybeUser (Just adminuser2)
    .   set #adminAccounts [Email "jet.li@example.com"]
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
