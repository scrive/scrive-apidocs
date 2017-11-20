module FeatureFlagsTest (featureFlagsTest) where

import Test.Framework
import Test.QuickCheck

import Company.Model
import DB
import FeatureFlags.Model
import KontraPrelude
import TestingUtil
import TestKontra

featureFlagsTest :: TestEnvSt -> Test
featureFlagsTest env = testGroup "FeatureFlags" [
  testThat "UpdateFeatureFlags updates state in DB"  env testUpdateFeatureFlagsWorks,
  testThat "Feature flags for new company have good defaults"  env testNewCompanyFeatureFlagDefaults
  ]

testUpdateFeatureFlagsWorks :: TestEnv ()
testUpdateFeatureFlagsWorks = do
  cid <- companyid <$> addNewCompany
  replicateM_ 100 $  do
    (ff :: FeatureFlags) <- rand 10 arbitrary
    True <- dbUpdate $ UpdateFeatureFlags cid ff
    ff' <- dbQuery $ GetFeatureFlags cid
    assertEqual "Updating feature flags works" ff ff'

testNewCompanyFeatureFlagDefaults :: TestEnv ()
testNewCompanyFeatureFlagDefaults  = do
  company <- addNewCompany
  ff <- dbQuery $ GetFeatureFlags (companyid company)
  assertEqual "New company can use templates" True (ffCanUseTemplates ff)
  assertEqual "New company can use branding" True (ffCanUseBranding ff)
  assertEqual "New company can use author attachments" True (ffCanUseAuthorAttachments ff)
  assertEqual "New company can use signatory attachments" True (ffCanUseSignatoryAttachments ff)
  assertEqual "New company can use mass sendout" True (ffCanUseMassSendout ff)
  assertEqual "New company can use sms invitations" True (ffCanUseSMSInvitations ff)
  assertEqual "New company can use sms confirmations" True (ffCanUseSMSConfirmations ff)
  assertEqual "New company can use dk authentication to view" True (ffCanUseDKAuthenticationToView ff)
  assertEqual "New company can use no authentication to view" True (ffCanUseNOAuthenticationToView ff)
  assertEqual "New company can use se authentication to view" True (ffCanUseSEAuthenticationToView ff)
  assertEqual "New company can use se authentication to sign" True (ffCanUseSEAuthenticationToSign ff)
  assertEqual "New company can use sms pin authentication to sing" True (ffCanUseSMSPinAuthenticationToSign ff)
