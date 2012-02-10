module CompanyStateTest (companyStateTests) where

import Control.Monad
import Test.Framework
import qualified Data.ByteString.UTF8 as BS

import Company.Model
import DB.Classes
import TestingUtil

companyStateTests :: DBEnv -> Test
companyStateTests env = testGroup "CompanyState" [
    testThat "CreateCompany works" env test_createCompany
  , testThat "GetCompanies works" env test_getCompanies
  , testThat "GetCompany works" env test_getCompany
  , testThat "GetCompanyByExternalID works" env test_getCompanyByExternalID
  , testThat "SetCompanyInfo works" env test_setCompanyInfo
  , testThat "GetOrCreateCompanyWithExternalID works" env test_getOrCreateCompanyWithExternalID
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
      eid = ExternalCompanyID $ BS.fromString seid
  Company{companyid = cid} <- addTestCompany seid
  Just company <- dbQuery $ GetCompanyByExternalID Nothing eid
  assertBool "GetCompanyByExternalID returned correct result" $ companyid company == cid

test_setCompanyInfo :: DB ()
test_setCompanyInfo = do
  Company{companyid = cid, companyinfo} <- addTestCompany ""
  let ci = companyinfo {
      companyname = BS.fromString "name"
    , companynumber = BS.fromString "number"
    , companyaddress = BS.fromString "address"
    , companyzip = BS.fromString "zip"
    , companycity = BS.fromString "city"
    , companycountry = BS.fromString "country"
  }
  res <- dbUpdate $ SetCompanyInfo cid ci
  assertBool "CompanyInfo updated correctly" res
  Just Company{companyinfo = newci} <- dbQuery $ GetCompany cid
  assertBool "Returned CompanyInfo is correct" $ ci == newci

test_getOrCreateCompanyWithExternalID :: DB ()
test_getOrCreateCompanyWithExternalID = do
  let eid = ExternalCompanyID $ BS.fromString "external_id"
  Company{companyid = cid} <- dbUpdate $ GetOrCreateCompanyWithExternalID Nothing eid
  company <- dbUpdate $ GetOrCreateCompanyWithExternalID Nothing eid
  assertBool "GetOrCreateCompanyWithExternalID returned the same company it created before" $ companyid company == cid

addTestCompany :: String -> DB Company
addTestCompany seid = dbUpdate $ CreateCompany Nothing eid
  where
    eid = case seid of
      "" -> Nothing
      _  -> Just $ ExternalCompanyID $ BS.fromString seid
