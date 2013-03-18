module CompanyStateTest (companyStateTests) where

import Control.Monad
import Test.Framework

import Company.Model
import DB
import TestingUtil
import TestKontra

companyStateTests :: TestEnvSt -> Test
companyStateTests env = testGroup "CompanyState" [
    testThat "CreateCompany works" env test_createCompany
  , testThat "GetCompanies works" env test_getCompanies
  , testThat "GetCompany works" env test_getCompany
  , testThat "GetCompanyByExternalID works" env test_getCompanyByExternalID
  , testThat "SetCompanyInfo works" env test_setCompanyInfo
  , testThat "GetOrCreateCompanyWithExternalID works" env test_getOrCreateCompanyWithExternalID
  , testThat "UpdateCompanyUI works" env test_updateCompanyUI
  ]

test_createCompany :: TestEnv ()
test_createCompany = do
  forM_ ["", "external_id"] addTestCompany
  assertSuccess

test_getCompanies :: TestEnv ()
test_getCompanies = do
  companies <- forM ["", "external_id"] addTestCompany
  result <- dbQuery $ GetCompanies [] [] 0 (-1)
  assertBool "GetCompanies returned correct result" $ and $ map (`elem` result) companies

test_getCompany :: TestEnv ()
test_getCompany = do
  Company{companyid = cid} <- addTestCompany ""
  Just company <- dbQuery $ GetCompany cid
  assertBool "GetCompany returned correct result" $ companyid company == cid

test_getCompanyByExternalID :: TestEnv ()
test_getCompanyByExternalID = do
  let seid = "external_id"
      eid = ExternalCompanyID seid
  Company{companyid = cid} <- addTestCompany seid
  Just company <- dbQuery $ GetCompanyByExternalID eid
  assertBool "GetCompanyByExternalID returned correct result" $ companyid company == cid

test_setCompanyInfo :: TestEnv ()
test_setCompanyInfo = do
  Company{companyid = cid, companyinfo} <- addTestCompany ""
  let ci = companyinfo {
      companyname = "name"
    , companynumber = "number"
    , companyaddress = "address"
    , companyzip = "zip"
    , companycity = "city"
    , companycountry = "country"
  }
  res <- dbUpdate $ SetCompanyInfo cid ci
  assertBool "CompanyInfo updated correctly" res
  Just Company{companyinfo = newci} <- dbQuery $ GetCompany cid
  assertBool "Returned CompanyInfo is correct" $ ci == newci

test_updateCompanyUI :: TestEnv ()
test_updateCompanyUI = do
  Company{companyid = cid, companyui} <- addTestCompany ""
  let cui = companyui {
    companyemailheaderfont = Just "Helvetica Neue, Arial, sans-serif"
  , companyemailfont = Just "Helvetica Neue, Arial, sans-serif"
  , companyemailbordercolour = Just "#dee4ed"
  , companyemailbuttoncolour = Just "215"
  , companyemailemailbackgroundcolour = Just "#0f0"
  , companyemailbackgroundcolour = Just "blue"
  , companyemailtextcolour = Just "green"
  , companyemaillogo = Nothing
  , companysignviewlogo = Nothing
  , companysignviewtextcolour = Nothing
  , companysignviewtextfont = Nothing
  , companysignviewbarscolour = Nothing
  , companysignviewbarstextcolour = Nothing
  , companysignviewbackgroundcolour = Nothing
  }
  res <- dbUpdate $ UpdateCompanyUI cid cui
  assertBool "CompanyUI updated correctly" res
  Just Company{companyui = newcui} <- dbQuery $ GetCompany cid
  assertEqual "Returned CompanyUI is correct" cui newcui

test_getOrCreateCompanyWithExternalID :: TestEnv ()
test_getOrCreateCompanyWithExternalID = do
  let eid = ExternalCompanyID "external_id"
  Company{companyid = cid} <- dbUpdate $ GetOrCreateCompanyWithExternalID eid
  company <- dbUpdate $ GetOrCreateCompanyWithExternalID eid
  assertBool "GetOrCreateCompanyWithExternalID returned the same company it created before" $ companyid company == cid

addTestCompany :: String -> TestEnv Company
addTestCompany seid = dbUpdate $ CreateCompany eid
  where
    eid = case seid of
      "" -> Nothing
      _  -> Just $ ExternalCompanyID seid
