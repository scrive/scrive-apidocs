module UserViewSampler(
    userViewSamples
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import UserView
import SamplerHelper
import SampleData


userViewSamples :: [Test]
userViewSamples = 
                     [testGroup "sample user emails" 
                           [testCase "new user" sampleNewUserMail,
                            testCase "password change" samplePasswordChangeMail,
                            testCase "invite subaccount" sampleInviteSubaccountMail,
                            testCase "new account created by admin" sampleNewAccountCreatedByAdminMail],
                      testGroup "sample user flash messages"
                           [testCase "user details saved" sampleUserDetailsSavedFlashMsg,
                            testCase "must accept tos" sampleMustAcceptTOSFlashMsg,
                            testCase "password not strong" samplePasswordNotStrongFlashMsg,
                            testCase "bad old password" sampleBadOldPasswordFlashMsg,
                            testCase "passwords don't match" samplePasswordsDontMatchFlashMsg]]


sampleNewUserMail =
  sampleMail "new_user" 
             (\t -> newUserMail t aHost aTestEmail aTestName aTestPassword)

samplePasswordChangeMail =
  sampleMail "password_change" 
             (\t -> passwordChangeMail t aHost aTestEmail aTestName aTestPassword)

sampleInviteSubaccountMail =
  sampleMail "invite_subaccount" 
             (\t -> inviteSubaccountMail t aHost aTestName aTestCompany anotherTestEmail anotherTestName aTestPassword)

sampleNewAccountCreatedByAdminMail =
  sampleMail "new_account_by_admin" 
             (\t -> mailNewAccountCreatedByAdmin t aTestCtx anotherTestName anotherTestEmail aTestPassword)        

sampleUserDetailsSavedFlashMsg =
  sampleFlashMsg "user_details_saved" flashMessageUserDetailsSaved

sampleMustAcceptTOSFlashMsg =
  sampleFlashMsg "must_accept_tos" flashMessageMustAcceptTOS

samplePasswordNotStrongFlashMsg =
  sampleFlashMsg "password_not_strong" flashMessagePasswordNotStrong

sampleBadOldPasswordFlashMsg =
  sampleFlashMsg "bad_old_password" flashMessageBadOldPassword

samplePasswordsDontMatchFlashMsg =
  sampleFlashMsg "passwords_dont_match" flashMessagePasswordsDontMatch

