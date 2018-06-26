module FeatureFlagsTest (featureFlagsTest) where

import Test.Framework
import Test.QuickCheck

import DB
import FeatureFlags.Model
import TestingUtil
import TestKontra
import UserGroup.Data
import UserGroup.Model

featureFlagsTest :: TestEnvSt -> Test
featureFlagsTest env = testGroup "FeatureFlags" [
  testThat "UpdateFeatureFlags updates state in DB"  env testUpdateFeatureFlagsWorks,
  testThat "Feature flags for new company have good defaults"  env testNewCompanyFeatureFlagDefaults
  ]

testUpdateFeatureFlagsWorks :: TestEnv ()
testUpdateFeatureFlagsWorks = do
  ugid <- (get ugID) <$> addNewUserGroup
  replicateM_ 100 $  do
    (ff :: FeatureFlags) <- rand 10 arbitrary
    True <- dbUpdate $ UpdateFeatureFlags ugid ff
    ff' <- dbQuery $ GetFeatureFlags ugid
    assertEqual "Updating feature flags works" ff ff'
    companyff <- dbQuery . GetFeatureFlagsForCompany . unsafeUserGroupIDToCompanyID $ ugid
    assertEqual "Updating FF synchronizes with company FF" ff' companyff

testNewCompanyFeatureFlagDefaults :: TestEnv ()
testNewCompanyFeatureFlagDefaults  = do
  ug <- addNewUserGroup
  ff <- dbQuery $ GetFeatureFlags (get ugID ug)
  assertEqual "New company can use templates" True (ffCanUseTemplates ff)
  assertEqual "New company can use branding" True (ffCanUseBranding ff)
  assertEqual "New company can use author attachments" True (ffCanUseAuthorAttachments ff)
  assertEqual "New company can use signatory attachments" True (ffCanUseSignatoryAttachments ff)
  assertEqual "New company can use mass sendout" True (ffCanUseMassSendout ff)
  assertEqual "New company can use sms invitations" True (ffCanUseSMSInvitations ff)
  assertEqual "New company can use sms confirmations" True (ffCanUseSMSConfirmations ff)
  assertEqual "New company can use dk authentication to view" False (ffCanUseDKAuthenticationToView ff)
  assertEqual "New company can use dk authentication to sign" False (ffCanUseDKAuthenticationToSign ff)
  assertEqual "New company can use no authentication to view" False (ffCanUseNOAuthenticationToView ff)
  assertEqual "New company can use no authentication to sign" False (ffCanUseNOAuthenticationToSign ff)
  assertEqual "New company can use se authentication to view" False (ffCanUseSEAuthenticationToView ff)
  assertEqual "New company can use se authentication to sign" False (ffCanUseSEAuthenticationToSign ff)
  assertEqual "New company can use sms pin authentication to view" True (ffCanUseSMSPinAuthenticationToView ff)
  assertEqual "New company can use sms pin authentication to sign" True (ffCanUseSMSPinAuthenticationToSign ff)
