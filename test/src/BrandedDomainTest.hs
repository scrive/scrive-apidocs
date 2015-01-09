module BrandedDomainTest (brandedDomainTests) where

import Test.Framework

import BrandedDomain.Model
import Company.Model
import DB
import TestingUtil
import TestKontra
import User.Model
import Utils.Default

brandedDomainTests :: TestEnvSt -> Test
brandedDomainTests env = testGroup "CompanyState" [
    testThat "test_brandedDomainCreateUpdate" env test_brandedDomainCreateUpdate,
    testThat "test_brandedDomainAssociatedDomain" env test_brandedDomainAssociatedDomain,
    testThat "test_brandedDomainAmbiguous" env test_brandedDomainAmbiguous
  ]

test_brandedDomainCreateUpdate :: TestEnv ()
test_brandedDomainCreateUpdate = do
  bdid <- dbUpdate $ NewBrandedDomain

  bd <- dbQuery $ GetBrandedDomainByID bdid
  let nbd = bd { bdUrl = "http://localhost:8000"
               }
  dbUpdate $ UpdateBrandedDomain nbd

  xbd <- dbQuery $ GetBrandedDomainByID bdid

  assertEqual "Braded domain round trips to database" nbd xbd

  wbd <- dbQuery $ GetBrandedDomainByURL (bdUrl nbd)

  assertEqual "GetBrandedDomainByURL works" nbd wbd

test_brandedDomainAssociatedDomain :: TestEnv ()
test_brandedDomainAssociatedDomain = do
  company <- addNewCompany
  bdid <- dbUpdate $ NewBrandedDomain
  bd <- dbQuery $ GetBrandedDomainByID bdid
  let nbd = bd { bdUrl = "http://localhost:8000"
               }
  dbUpdate $ UpdateBrandedDomain nbd

  pwd <- createPassword "admin"

  Just user <- dbUpdate $ AddUser ("Andrzej", "Rybczak") "andrzej@scrive.com" (Just pwd) (companyid company,True) defaultValue bdid

  wbd <- dbQuery $ GetBrandedDomainByUserID (userid user)

  assertEqual "GetBrandedDomainByUserID works" nbd wbd

test_brandedDomainAmbiguous :: TestEnv ()
test_brandedDomainAmbiguous = do
  mainbd <- dbQuery $ GetMainBrandedDomain
  _bdid0 <- dbUpdate $ NewBrandedDomain
  wbd0 <- dbQuery $ GetBrandedDomainByURL ("http://url")

  assertEqual "GetBrandedDomainByURL with no url does not match" mainbd wbd0

  bdid1 <- dbUpdate $ NewBrandedDomain
  bd1 <- dbQuery $ GetBrandedDomainByID bdid1
  let nbd1 = bd1 { bdUrl = "http://localhost:8000"
               }
  dbUpdate $ UpdateBrandedDomain nbd1

  bdid2 <- dbUpdate $ NewBrandedDomain
  bd2 <- dbQuery $ GetBrandedDomainByID bdid2
  let nbd2 = bd2 { bdUrl = "http://localhost"
               }
  dbUpdate $ UpdateBrandedDomain nbd2


  wbd1 <- dbQuery $ GetBrandedDomainByURL (bdUrl nbd1)
  wbd2 <- dbQuery $ GetBrandedDomainByURL (bdUrl nbd2)
  wbd3 <- dbQuery $ GetBrandedDomainByURL ("")

  assertEqual "GetBrandedDomainByURL works" nbd1 wbd1
  assertEqual "GetBrandedDomainByURL works" nbd2 wbd2
  assertEqual "GetBrandedDomainByURL works" mainbd wbd3
