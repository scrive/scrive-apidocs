module FeatureFlagsTest (featureFlagsTest) where

import Test.Framework
import Test.QuickCheck

import DB
import FeatureFlags.Model
import TestingUtil
import TestKontra
import UserGroup.Model
import UserGroup.Types

featureFlagsTest :: TestEnvSt -> Test
featureFlagsTest env = testGroup
  "FeatureFlags"
  [ testThat "UpdateFeatureFlags updates state in DB" env testUpdateFeatureFlagsWorks
  , testThat "Feature flags for new company have good defaults"
             env
             testNewCompanyFeatureFlagDefaults
  ]

testUpdateFeatureFlagsWorks :: TestEnv ()
testUpdateFeatureFlagsWorks = do
  ug <- addNewUserGroup
  replicateM_ 100 $ do
    (fs :: Features) <- rand 10 arbitrary
    dbUpdate . UserGroupUpdate $ set ugFeatures (Just fs) ug
    ug' <- fmap fromJust . dbQuery . UserGroupGet $ get ugID ug
    assertEqual "Updating feature flags works" (Just fs) (get ugFeatures ug')

testNewCompanyFeatureFlagDefaults :: TestEnv ()
testNewCompanyFeatureFlagDefaults = do
  ug <- addNewUserGroup
  let fs = fromJust $ get ugFeatures ug
  checkNewAccountFlags (fAdminUsers fs)
  checkNewAccountFlags (fRegularUsers fs)

  where
    checkNewAccountFlags ff = do
      assertEqual "New company can use templates" True (ffCanUseTemplates ff)
      assertEqual "New company can use branding"  True (ffCanUseBranding ff)
      assertEqual "New company can use author attachments"
                  True
                  (ffCanUseAuthorAttachments ff)
      assertEqual "New company can use signatory attachments"
                  True
                  (ffCanUseSignatoryAttachments ff)
      assertEqual "New company can use mass sendout"    True (ffCanUseMassSendout ff)
      assertEqual "New company can use sms invitations" True (ffCanUseSMSInvitations ff)
      assertEqual "New company can use sms confirmations"
                  True
                  (ffCanUseSMSConfirmations ff)
      assertEqual "New company can't use dk authentication to view"
                  False
                  (ffCanUseDKAuthenticationToView ff)
      assertEqual "New company can't use dk authentication to sign"
                  False
                  (ffCanUseDKAuthenticationToSign ff)
      assertEqual "New company can't use fi authentication to view"
                  False
                  (ffCanUseFIAuthenticationToView ff)
      assertEqual "New company can't use no authentication to view"
                  False
                  (ffCanUseNOAuthenticationToView ff)
      assertEqual "New company can't use no authentication to sign"
                  False
                  (ffCanUseNOAuthenticationToSign ff)
      assertEqual "New company can't use se authentication to view"
                  False
                  (ffCanUseSEAuthenticationToView ff)
      assertEqual "New company can't use se authentication to sign"
                  False
                  (ffCanUseSEAuthenticationToSign ff)
      assertEqual "New company can use sms pin authentication to view"
                  True
                  (ffCanUseSMSPinAuthenticationToView ff)
      assertEqual "New company can use sms pin authentication to sign"
                  True
                  (ffCanUseSMSPinAuthenticationToSign ff)
      assertEqual "New company can use standard authentication to view"
                  True
                  (ffCanUseStandardAuthenticationToView ff)
      assertEqual "New company can use standard authentication to sign"
                  True
                  (ffCanUseStandardAuthenticationToSign ff)
      assertEqual "New company can't use verimi authentication to view"
                  False
                  (ffCanUseVerimiAuthenticationToView ff)
      assertEqual "New company can't use idin authentication to view"
                  False
                  (ffCanUseIDINAuthenticationToView ff)
      assertEqual "New company can use email invitation"
                  True
                  (ffCanUseEmailInvitations ff)
      assertEqual "New company can use email confirmation"
                  True
                  (ffCanUseEmailConfirmations ff)
      assertEqual "New company can use API invitation" True (ffCanUseAPIInvitations ff)
      assertEqual "New company can use Pad invitation" True (ffCanUsePadInvitations ff)
      assertEqual "New company can't use Shareable Links"
                  False
                  (ffCanUseShareableLinks ff)
      assertEqual "New company can use Forwarding" True  (ffCanUseForwarding ff)
      assertEqual "New company can't use Portal"   False (ffCanUsePortal ff)
