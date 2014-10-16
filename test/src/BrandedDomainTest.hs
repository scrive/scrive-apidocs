module BrandedDomainTest (brandedDomainTests) where

import Test.Framework

import BrandedDomain.BrandedDomain
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

  Just bd <- dbQuery $ GetBrandedDomainByID bdid
  let nbd = bd { bdurl = "http://localhost:8000"
               }
  dbUpdate $ UpdateBrandedDomain nbd

  Just xbd <- dbQuery $ GetBrandedDomainByID bdid

  assertEqual "Braded domain round trips to database" nbd xbd

  mwbd <- dbQuery $ GetBrandedDomainByURL (bdurl nbd)

  assertEqual "GetBrandedDomainByURL works" (Just nbd) mwbd

test_brandedDomainAssociatedDomain :: TestEnv ()
test_brandedDomainAssociatedDomain = do
  company <- addNewCompany
  bdid <- dbUpdate $ NewBrandedDomain
  Just bd <- dbQuery $ GetBrandedDomainByID bdid
  let nbd = bd { bdurl = "http://localhost:8000"
               }
  dbUpdate $ UpdateBrandedDomain nbd

  pwd <- createPassword "admin"

  Just user <- dbUpdate $ AddUser ("Andrzej", "Rybczak") "andrzej@scrive.com" (Just pwd) (companyid company,True) defaultValue (Just bdid)

  mwbd <- dbQuery $ GetBrandedDomainByUserID (userid user)

  assertEqual "GetBrandedDomainByUserID works" (Just nbd) mwbd

test_brandedDomainAmbiguous :: TestEnv ()
test_brandedDomainAmbiguous = do
  _bdid0 <- dbUpdate $ NewBrandedDomain
  mwbd0 <- dbQuery $ GetBrandedDomainByURL ("http://url")

  assertEqual "GetBrandedDomainByURL with no url does not match" Nothing mwbd0

  bdid1 <- dbUpdate $ NewBrandedDomain
  Just bd1 <- dbQuery $ GetBrandedDomainByID bdid1
  let nbd1 = bd1 { bdurl = "http://localhost:8000"
               }
  dbUpdate $ UpdateBrandedDomain nbd1

  bdid2 <- dbUpdate $ NewBrandedDomain
  Just bd2 <- dbQuery $ GetBrandedDomainByID bdid2
  let nbd2 = bd2 { bdurl = "http://localhost"
               }
  dbUpdate $ UpdateBrandedDomain nbd2


  mwbd1 <- dbQuery $ GetBrandedDomainByURL (bdurl nbd1)
  mwbd2 <- dbQuery $ GetBrandedDomainByURL (bdurl nbd2)
  mwbd3 <- dbQuery $ GetBrandedDomainByURL ("")

  assertEqual "GetBrandedDomainByURL works" (Just nbd1) mwbd1
  assertEqual "GetBrandedDomainByURL works" (Just nbd2) mwbd2
  assertEqual "GetBrandedDomainByURL works" Nothing mwbd3
