module DocViewSampler(
    docViewSamples
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import DocView
import SamplerHelper
import SampleData
import User
import DocState
import UserState
import Misc
import AppView
import KontraLink
import HSP
import System.IO.Unsafe
import qualified Data.ByteString.UTF8 as BS
import System.IO.UTF8 as UTF8

docViewSamples :: [Test]
docViewSamples = [testGroup "sample document flash messages"
                           [testCase "document draft saved" sampleDocumentDraftSavedFlashMsg,
                            testCase "document restarted" sampleDocumentRestartedFlashMsg,
                            testCase "remind unsigned mail sent" sampleRemindUnsignedMailSentFlashMsg,
                            testCase "remind signed mail sent" sampleRemindSignedMailSentFlashMsg,
                            testCase "document canceled" sampleCanceledFlashMsg ],
                  testGroup "sample document views"
                           [testCase "landpage sign invite" sampleLandpageSignInviteView,
                            testCase "landpage rejected view" sampleLandpageRejectedView,
                            testCase "landpage signed (no account)" sampleLandpageSignedNoAccountView,
                            testCase "landpage signed (has account)" sampleLandpageSignedHasAccountView,
                            testCase "landpage login for save" sampleLandpageLoginForSaveView, 
                            testCase "landpage document saved" sampleDocumentSavedView,
                            testCase "document for sign" sampleDocumentViewForSign
                           ]]


sampleDocumentDraftSavedFlashMsg =
  sampleFlashMsg "document_details_saved" flashDocumentDraftSaved

sampleDocumentRestartedFlashMsg =
  sampleFlashMsg "document_restarted" flashDocumentRestarted

sampleRemindUnsignedMailSentFlashMsg = 
  sampleFlashMsg "remind_unsigned_mail_sent" (\t -> flashRemindMailSent t anUnsignedSigLink)

sampleRemindSignedMailSentFlashMsg = 
  sampleFlashMsg "remind_signed_mail_sent" (\t -> flashRemindMailSent t aSignedSigLink)

sampleCanceledFlashMsg =
  sampleFlashMsg "document_canceled" flashMessageCanceled

sampleLandpageSignInviteView =
  let ctx = aTestCtx{ctxmaybeuser=Nothing}
  in sampleView2 "landpage_sign_invite" (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja"
                                               (fmap cdata $ landpageSignInviteView templ anUnsignedDocument))

sampleLandpageRejectedView =
  let ctx = aTestCtx{ctxmaybeuser=Nothing}
  in sampleView2 "landpage_rejected_view" (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja"
                                               (fmap cdata $ landpageRejectedView templ anUnsignedDocument))

sampleLandpageSignedNoAccountView =
  let ctx = aTestCtx{ctxmaybeuser=Nothing}
  in sampleView2 "landpage_signed_no_account" (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja"
                                               (fmap cdata $ landpageSignedView templ aSignedDocument aSignedSigLink False))

sampleLandpageSignedHasAccountView =
  let ctx = aTestCtx{ctxmaybeuser=Nothing}
  in sampleView2 "landpage_signed_has_account" (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja"
                                               (fmap cdata $ landpageSignedView templ aSignedDocument aSignedSigLink True))

sampleLandpageLoginForSaveView =
  let ctx = aTestCtx{ctxmaybeuser=Nothing}
  in sampleView2 "landpage_login_for_save" (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja"
                                               (fmap cdata $ landpageLoginForSaveView templ))

sampleDocumentSavedView =
  let ctx = aTestCtx{ctxmaybeuser=Nothing}
  in sampleView2 "document_saved" (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja"
                                               (fmap cdata $ landpageDocumentSavedView templ))

sampleDocumentViewForSign =
    let ctx = aTestCtx{ctxmaybeuser=Nothing}
        document = anUnsignedDocument
        wassigned = False
        author = aTestUser
        invitedlink = head $ documentsignatorylinks document
    in
    sampleView2 "document_for_sign" (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja"
                                               (fmap cdata $ pageDocumentForSign (LinkSignDoc document invitedlink) 
                                                     document (ctx{ctxtemplates=templ}) invitedlink wassigned author))


sampleView2 name action = sample name "view" (\t -> renderHSPToString (action t)) (UTF8.writeFile)
