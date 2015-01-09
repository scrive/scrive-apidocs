module CompanyStateTest (companyStateTests) where

import Control.Monad
import Test.Framework

import Company.CompanyUI
import Company.Model
import DB
import TestingUtil
import TestKontra

companyStateTests :: TestEnvSt -> Test
companyStateTests env = testGroup "CompanyState" [
    testThat "CreateCompany works" env test_createCompany
  , testThat "GetCompanies works" env test_getCompanies
  , testThat "GetCompany works" env test_getCompany
  , testThat "SetCompanyInfo works" env test_setCompanyInfo
  , testThat "UpdateCompanyUI works" env test_updateCompanyUI
  ]

test_createCompany :: TestEnv ()
test_createCompany = do
  _ <- dbUpdate $ CreateCompany
  assertSuccess

test_getCompanies :: TestEnv ()
test_getCompanies = do
  companies <- forM [(),()] $ \_ -> dbUpdate $ CreateCompany
  result <- dbQuery $ GetCompanies [] [] 0 (-1)
  assertBool "GetCompanies returned correct result" $ and $ map (`elem` result) companies

test_getCompany :: TestEnv ()
test_getCompany = do
  Company{companyid = cid} <- dbUpdate $ CreateCompany
  Just company <- dbQuery $ GetCompany cid
  assertBool "GetCompany returned correct result" $ companyid company == cid

test_setCompanyInfo :: TestEnv ()
test_setCompanyInfo = do
  Company{companyid = cid, companyinfo} <- dbUpdate $ CreateCompany
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
  Company{companyid = cid} <- dbUpdate $ CreateCompany
  let cui = CompanyUI {
      companyuicompanyid = cid
    , companyMailTheme = Nothing
    , companySignviewTheme = Nothing
    , companyServiceTheme = Nothing
    , companyBrowserTitle = Nothing
    , companySmsOriginator = Nothing
    , companyFavicon = Nothing
  }
  res <- dbUpdate $ SetCompanyUI cid cui
  assertBool "CompanyUI updated correctly" res
  newcui2 <- dbQuery $ GetCompanyUI cid
  assertEqual "Returned CompanyUI is correct" cui newcui2
