module DocViewMailSampler(
    docViewMailSamples
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import DocViewMail
import SamplerHelper
import SampleData


docViewMailSamples :: [Test]
docViewMailSamples = 
                     [testGroup "sample document emails"
                           [testCase "remind not signed" sampleRemindNotSignedMail,
                            testCase "remind not signed (with custom msg)" sampleRemindNotSignedMailWithCustomMsg,
                            testCase "remind signed" sampleRemindSignedMail,
                            testCase "remind signed (with custom msg)" sampleRemindSignedMailWithCustomMsg,
                            testCase "document rejected" sampleDocumentRejectedMail,
                            testCase "document rejected (with custom msg)" sampleDocumentRejectedMailWithCustomMsg,
                            testCase "invitation to sign" sampleInvitationToSignMail,
                            testCase "document closed for signatories" sampleDocumentClosedForSignatoriesMail,
                            testCase "document closed for author" sampleDocumentClosedForAuthorMail,
                            testCase "document awaiting" sampleDocumentAwaitingMail,
                            testCase "document cancelled" sampleDocumentCancelledMail,
                            testCase "document cancelled (with custom msg)" sampleDocumentCancelledMailWithCustomMsg]]


sampleRemindNotSignedMail =
  sampleMail "remind_not_signed" 
             (\t -> mailDocumentRemind t Nothing aTestCtx anUnsignedDocument anUnsignedSigLink)

sampleRemindNotSignedMailWithCustomMsg =
  sampleMail "remind_not_signed_with_custom_msg" 
             (\t -> mailDocumentRemind t (Just aCustomMsg) aTestCtx anUnsignedDocument anUnsignedSigLink)

sampleRemindSignedMail =
  sampleMail "remind_signed"
             (\t -> mailDocumentRemind t Nothing aTestCtx aSignedDocument aSignedSigLink)

sampleRemindSignedMailWithCustomMsg =
  sampleMail "remind_signed_with_custom_msg"
             (\t -> mailDocumentRemind t (Just aCustomMsg) aTestCtx aSignedDocument aSignedSigLink)

sampleDocumentRejectedMail =
  sampleMail "document_rejected"
             (\t -> mailDocumentRejectedForAuthor t Nothing aTestCtx aTestName anUnsignedDocument anotherTestName)

sampleDocumentRejectedMailWithCustomMsg =
  sampleMail "document_rejected_with_custom_msg"
             (\t -> mailDocumentRejectedForAuthor t (Just aCustomMsg) aTestCtx aTestName anUnsignedDocument anotherTestName)

sampleInvitationToSignMail =
  sampleMail "invitation_to_sign"
             (\t -> mailInvitationToSign t aTestCtx anUnsignedDocument anUnsignedSigLink)

sampleDocumentClosedForSignatoriesMail =
  sampleMail "document_closed_for_signatories"
             (\t -> mailDocumentClosedForSignatories t aTestCtx aSignedDocument aSignedSigLink)

sampleDocumentClosedForAuthorMail =
  sampleMail "document_closed_for_author"
             (\t -> mailDocumentClosedForAuthor t aTestCtx aTestName aSignedDocument)

sampleDocumentAwaitingMail =
  sampleMail "document_awaiting"
             (\t -> mailDocumentAwaitingForAuthor t aTestCtx aTestName anUnsignedDocument)

sampleDocumentCancelledMail =
  sampleMail "document_cancelled"
             (\t -> mailCancelDocumentByAuthor t Nothing aTestCtx anUnsignedDocument anUnsignedSigLink)

sampleDocumentCancelledMailWithCustomMsg =
  sampleMail "document_cancelled_with_custom_msg"
             (\t -> mailCancelDocumentByAuthor t (Just aCustomMsg) aTestCtx anUnsignedDocument anUnsignedSigLink)

