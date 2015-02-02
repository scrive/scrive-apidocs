module BrandedDomainTest (brandedDomainTests) where

import Test.Framework

import BrandedDomain.Model
import Company.Model
import DB
import TestingUtil
import TestKontra
import Theme.Model
import User.Model
import Utils.Default

brandedDomainTests :: TestEnvSt -> Test
brandedDomainTests env = testGroup "BrandedDomainsTest" [
    testThat "test_brandedDomainCreateUpdate" env test_brandedDomainCreateUpdate,
    testThat "test_brandedDomainAssociatedDomain" env test_brandedDomainAssociatedDomain,
    testThat "test_brandedDomainAmbiguous" env test_brandedDomainAmbiguous,
    testThat "test_brandedDomainCanChangeThemeOrSettingsOfMainDomain" env test_brandedDomainCanChangeThemeOrSettingsOfMainDomain
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


test_brandedDomainCanChangeThemeOrSettingsOfMainDomain :: TestEnv ()
test_brandedDomainCanChangeThemeOrSettingsOfMainDomain = do
  mainbd1 <- dbQuery $ GetMainBrandedDomain
  _ <- dbUpdate $ UpdateBrandedDomain mainbd1 {bdMailTheme = bdLoginTheme mainbd1}
  mainbd2 <- dbQuery $ GetMainBrandedDomain
  assertEqual "Can change main domain " mainbd1 mainbd2

  mailTheme1 <- dbQuery $ GetTheme (bdMailTheme mainbd1)
  False <-  dbUpdate $ UpdateThemeForDomain (bdid mainbd1) $ mailTheme1  {themeBrandTextColor = "#222345"}
  mailTheme2 <- dbQuery $ GetTheme (bdMailTheme mainbd1)
  assertEqual "Can change mail theme of main domain " mailTheme1 mailTheme2

  loginTheme1 <- dbQuery $ GetTheme (bdLoginTheme mainbd1)
  False <- dbUpdate $ UpdateThemeForDomain (bdid mainbd1) $ loginTheme1 {themeBrandColor = "#123456"}
  loginTheme2 <- dbQuery $ GetTheme (bdLoginTheme mainbd1)
  assertEqual "Can change login theme of main domain " loginTheme1 loginTheme2

  serviceTheme1 <- dbQuery $ GetTheme (bdServiceTheme mainbd1)
  False <-  dbUpdate $ UpdateThemeForDomain (bdid mainbd1) $ serviceTheme1 {themeName = "New name"}
  serviceTheme2 <- dbQuery $ GetTheme (bdServiceTheme mainbd1)
  assertEqual "Can change service theme of main domain " serviceTheme1 serviceTheme2

  signviewTheme1 <- dbQuery $ GetTheme (bdSignviewTheme mainbd1)
  False <-dbUpdate $ UpdateThemeForDomain (bdid mainbd1) signviewTheme1
  signviewTheme2 <- dbQuery $ GetTheme (bdSignviewTheme mainbd1)
  assertEqual "Can change signview theme of main domain " signviewTheme1 signviewTheme2

  _ <-dbUpdate $ DeleteThemeOwnedByDomain (bdid mainbd1) (bdSignviewTheme mainbd1)
  signviewTheme3 <- dbQuery $ GetTheme (bdSignviewTheme mainbd1)
  assertEqual "Can delete theme of main domain  " signviewTheme1 signviewTheme3
