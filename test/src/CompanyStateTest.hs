module CompanyStateTest (companyStateTests) where

import Control.Monad
import Test.Framework

import Company.Model
import DB.Classes
import TestingUtil

companyStateTests :: DBEnv -> Test
companyStateTests conn = testGroup "CompanyState" [
    testThat "CreateCompany works" conn test_createCompany
  , testThat "GetCompanies works" conn test_getCompanies
  , testThat "GetCompany works" conn test_getCompany
  , testThat "GetCompanyByExternalID works" conn test_getCompanyByExternalID
  , testThat "SetCompanyInfo works" conn test_setCompanyInfo
  , testThat "GetOrCreateCompanyWithExternalID works" conn test_getOrCreateCompanyWithExternalID
  , testThat "UpdateCompanyUI works" conn test_updateCompanyUI
  ]

test_createCompany :: DB ()
test_createCompany = do
  forM_ ["", "external_id"] addTestCompany
  assertSuccess

test_getCompanies :: DB ()
test_getCompanies = do
  companies <- forM ["", "external_id"] addTestCompany
  result <- dbQuery $ GetCompanies Nothing
  assertBool "GetCompanies returned correct result" $ and $ map (`elem` result) companies

test_getCompany :: DB ()
test_getCompany = do
  Company{companyid = cid} <- addTestCompany ""
  Just company <- dbQuery $ GetCompany cid
  assertBool "GetCompany returned correct result" $ companyid company == cid

test_getCompanyByExternalID :: DB ()
test_getCompanyByExternalID = do
  let seid = "external_id"
      eid = ExternalCompanyID seid
  Company{companyid = cid} <- addTestCompany seid
  Just company <- dbQuery $ GetCompanyByExternalID Nothing eid
  assertBool "GetCompanyByExternalID returned correct result" $ companyid company == cid

test_setCompanyInfo :: DB ()
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

test_updateCompanyUI :: DB ()
test_updateCompanyUI = do
  Company{companyid = cid, companyui} <- addTestCompany ""
  let cui = companyui {
    companybarsbackground = Just "blue"
  , companybarstextcolour = Just "green"
  , companylogo = Nothing
  }
  res <- dbUpdate $ UpdateCompanyUI cid cui
  assertBool "CompanyUI updated correctly" res
  Just Company{companyui = newcui} <- dbQuery $ GetCompany cid
  assertEqual "Returned CompanyUI is correct" cui newcui

test_getOrCreateCompanyWithExternalID :: DB ()
test_getOrCreateCompanyWithExternalID = do
  let eid = ExternalCompanyID "external_id"
  Company{companyid = cid} <- dbUpdate $ GetOrCreateCompanyWithExternalID Nothing eid
  company <- dbUpdate $ GetOrCreateCompanyWithExternalID Nothing eid
  assertBool "GetOrCreateCompanyWithExternalID returned the same company it created before" $ companyid company == cid

addTestCompany :: String -> DB Company
addTestCompany seid = dbUpdate $ CreateCompany Nothing eid
  where
    eid = case seid of
      "" -> Nothing
      _  -> Just $ ExternalCompanyID seid
