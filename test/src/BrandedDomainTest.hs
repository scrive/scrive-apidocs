module BrandedDomainTest (brandedDomainTests) where

--import Control.Monad
import Test.Framework

import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Company.Model
import User.Model
--import BrandedDomain.BrandedDomainID
import DB
import TestingUtil
import TestKontra
import Utils.Default

brandedDomainTests :: TestEnvSt -> Test
brandedDomainTests env = testGroup "CompanyState" [
    testThat "test_brandedDomainCreateUpdate" env test_brandedDomainCreateUpdate,
    testThat "test_brandedDomainAssociatedDomain" env test_brandedDomainAssociatedDomain
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

  Just user <- dbUpdate $ AddUser ("Andrzej", "Rybczak") "andrzej@scrive.com" (Just pwd) (companyid company,True) defaultValue (Just "http://localhost:8000")

  mwbd <- dbQuery $ GetBrandedDomainByUserID (userid user)

  assertEqual "GetBrandedDomainByUserID works" (Just nbd) mwbd
