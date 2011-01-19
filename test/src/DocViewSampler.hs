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
                            testCase "document for sign (logged out)" sampleDocumentViewForSign_loggedOut,
                            testCase "document for sign (logged in as author)" sampleDocumentViewForSign_loggedInAsAuthor,
                            testCase "document for sign (logged in as signatory)" sampleDocumentViewForSign_loggedInAsSignatory,
                            testCase "document for sign (with long data)" sampleDocumentViewForSign_longData,
                            testCase "document for sign (with international chars)" sampleDocumentViewForSign_internationalChars,
                            testCase "document for sign (with html in data)" sampleDocumentViewForSign_htmlInData,
                            testCase "document for sign (with empty data)" sampleDocumentViewForSign_blankData,
                            testCase "document for sign (with a few signatories)" sampleDocumentViewForSign_aFewSignatories,
                            testCase "document for sign (with a lot of signatories)" sampleDocumentViewForSign_aLotOfSignatories]]


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

sampleDocumentViewForSign_loggedOut = sampleDocumentViewForSign 
                                                          "document_for_sign_logged_out" 
                                                          Nothing 
                                                          "a_test_doc.pdf"
                                                          [("annie@testcorp.com", "Annie Angus", "Test Corp", "1234"),
                                                           ("clara@testcorp.com", "Clara Contrary", "Test Corp", "2222")]

sampleDocumentViewForSign_loggedInAsAuthor = sampleDocumentViewForSign 
                                                          "document_for_sign_logged_in_as_author" 
                                                          (Just aTestUser)
                                                          "a_test_doc.pdf"
                                                          [("annie@testcorp.com", "Annie Angus", "Test Corp", "1234"),
                                                           ("clara@testcorp.com", "Clara Contrary", "Test Corp", "2222")]

sampleDocumentViewForSign_loggedInAsSignatory = sampleDocumentViewForSign 
                                                          "document_for_sign_logged_in_as_signatory" 
                                                          (Just annie)
                                                          "a_test_doc.pdf"
                                                          [("annie@testcorp.com", "Annie Angus", "Test Corp", "1111"),
                                                           ("clara@testcorp.com", "Clara Contrary", "Test Corp", "2222")]
                                                  where  annie :: User
                                                         annie = aTestUser{userid = UserID 1111, 
                                                                          userinfo = someTestUserInfo{
                                                                                       userfstname=(BS.fromString "Annie"), 
                                                                                       usersndname=(BS.fromString "Angus"),
                                                                                       usercompanyname=(BS.fromString "Test Corp")}}

sampleDocumentViewForSign_longData = sampleDocumentViewForSign 
                                                          "document_for_sign_with_long_data" 
                                                          Nothing
                                                          "A Test Document With an Extremely Long Name With Lots of Spaces in Too - Okay it might be unlikely but it is perfectly possible.pdf"
                                                          [("annie@testcorp.com", "An Especially Long Name for the Signatory - Admittedly unlikely, but it's important to test", "Test Corp", "1111"),
                                                           ("clara@testcorp.com", "Clara Contrary", "Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch is in Wales", "2222"),
                                                           ("dave@acorpwithaveryverylongaddresssowecantestthelayout.com", "Dave Daventry", "Test Corp", "3333"),
                                                           ("emma@testcorp.com", "Emma Emo", "Test Corp", "44444444444444444444444444444444444")]

sampleDocumentViewForSign_internationalChars = sampleDocumentViewForSign 
                                                          "document_for_sign_with_international_chars" 
                                                          Nothing 
                                                          "Mam nadzieję, że to ma sens (mały żółw).pdf"
                                                          [("tinyturtle@turtlecorp.com", "mały żółw", "korpus żółwia", "1111"),
                                                           ("clara@testcorp.com", "Клара Контрерый", "тест компании", "2222"),
                                                           ("dave@testcorp.com", "戴夫达文特里", "測試公司", "3333"),
                                                           ("emma@testcorp.com", "エマエモ", "テスト会社", "4444"),
                                                           ("fred@svenskatecken.se", "Fred Felint", "svenska tecken ÅÄÖåäö", "5555")]    

sampleDocumentViewForSign_htmlInData = sampleDocumentViewForSign 
                                                  "document_for_sign_with_html_in_data" 
                                                  Nothing 
                                                  "<a=\"some%20html\">can you see some html?</a>.pdf"
                                                  [("annie@testcorp.com", "<escapethis>should be in tags</escapethis>", "Test Corp", "1111"),
                                                   ("clara@testcorp.com", "Clara Contrary", "<script type=\"text/javascript\">should be in tags</script>", "2222"),
                                                   ("dave@testcorp.com", "&lt;Dave Daventry surrounded by funny stuff&gt;", "Test Corp", "3333"),
                                                   ("emma@testcorp.com", "Emma Emo", "Test%20Corp%20with%20funny%20stuff", "4444")]

sampleDocumentViewForSign_blankData = sampleDocumentViewForSign 
                                                  "document_for_sign_with_blank_data" 
                                                  Nothing 
                                                  "a_test_doc.pdf"
                                                  [("annie@hasnoname.com", "", "Test Corp", "1111"),
                                                   ("clara@hasnocompany.com", "Clara Contrary", "", "2222"),
                                                   ("dave@hasneithernamenorcompany.com", "", "", "3333")]

sampleDocumentViewForSign_aFewSignatories = sampleDocumentViewForSign 
                                                  "document_for_sign_with_a_few_signatories" 
                                                  Nothing 
                                                  "a_test_doc.pdf"
                                                  [("annie@testcorp.com", "Annie Angus", "Test Corp", "1111"),
                                                   ("clara@testcorp.com", "Clara Contrary", "Test Corp", "2222"),
                                                   ("dave@testcorp.com", "Dave Daventry", "Test Corp", "3333"),
                                                   ("emma@testcorp.com", "Emma Emo", "Test Corp", "4444")]

sampleDocumentViewForSign_aLotOfSignatories = sampleDocumentViewForSign 
                                                  "document_for_sign_with_a_lot_of_signatories" 
                                                  Nothing 
                                                  "a_test_doc.pdf"
                                                   oneHundredSignatories

sampleDocumentViewForSign testName ctxmaybeuser' documenttitle' sigs =
    let ctx = aTestCtx{ctxmaybeuser=ctxmaybeuser'}
        document = anUnsignedDocument{documenttitle = BS.fromString documenttitle',
                                      documentsignatorylinks = map 
                                                                (\sig@(sigemail, signame, sigcompany, signum) ->  
                                                                      createSigLink sigemail signame sigcompany signum) 
                                                               sigs
                                     }
        wassigned = False
        author = aTestUser
        invitedlink = head $ documentsignatorylinks document
    in
    sampleView2 testName (\templ -> pageFromBody' "../public" (ctx{ctxtemplates=templ}) TopNone "kontrakcja"
                                    (fmap cdata $ pageDocumentForSign (LinkSignDoc document invitedlink) 
                                          document (ctx{ctxtemplates=templ}) invitedlink wassigned author))

createSigLink sigemail signame sigcompany signum = anUnsignedSigLink{
                                                       signatorydetails = otherSignatoryDetails{
                                                                            signatoryemail = BS.fromString sigemail,
                                                                            signatoryname = BS.fromString signame,
                                                                            signatorycompany = BS.fromString sigcompany,
                                                                            signatorynumber = BS.fromString signum}}

sampleView2 name action = sample name "view" (\t -> renderHSPToString (action t)) (UTF8.writeFile)

oneHundredSignatories = [("signatory_1@testcorp.com", "Signatory 1", "Test Corp", "1"),
                         ("signatory_2@testcorp.com", "Signatory 2", "Test Corp", "2"),
                         ("signatory_3@testcorp.com", "Signatory 3", "Test Corp", "3"),
                         ("signatory_4@testcorp.com", "Signatory 4", "Test Corp", "4"),
                         ("signatory_5@testcorp.com", "Signatory 5", "Test Corp", "5"),
                         ("signatory_6@testcorp.com", "Signatory 6", "Test Corp", "6"),
                         ("signatory_7@testcorp.com", "Signatory 7", "Test Corp", "7"),
                         ("signatory_8@testcorp.com", "Signatory 8", "Test Corp", "8"),
                         ("signatory_9@testcorp.com", "Signatory 9", "Test Corp", "9"),
                         ("signatory_10@testcorp.com", "Signatory 10", "Test Corp", "10"),
                         ("signatory_11@testcorp.com", "Signatory 11", "Test Corp", "11"),
                         ("signatory_12@testcorp.com", "Signatory 12", "Test Corp", "12"),
                         ("signatory_13@testcorp.com", "Signatory 13", "Test Corp", "13"),
                         ("signatory_14@testcorp.com", "Signatory 14", "Test Corp", "14"),
                         ("signatory_15@testcorp.com", "Signatory 15", "Test Corp", "15"),
                         ("signatory_16@testcorp.com", "Signatory 16", "Test Corp", "16"),
                         ("signatory_17@testcorp.com", "Signatory 17", "Test Corp", "17"),
                         ("signatory_18@testcorp.com", "Signatory 18", "Test Corp", "18"),
                         ("signatory_19@testcorp.com", "Signatory 19", "Test Corp", "19"),
                         ("signatory_20@testcorp.com", "Signatory 20", "Test Corp", "20"),
                         ("signatory_21@testcorp.com", "Signatory 21", "Test Corp", "21"),
                         ("signatory_22@testcorp.com", "Signatory 22", "Test Corp", "22"),
                         ("signatory_23@testcorp.com", "Signatory 23", "Test Corp", "23"),
                         ("signatory_24@testcorp.com", "Signatory 24", "Test Corp", "24"),
                         ("signatory_25@testcorp.com", "Signatory 25", "Test Corp", "25"),
                         ("signatory_26@testcorp.com", "Signatory 26", "Test Corp", "26"),
                         ("signatory_27@testcorp.com", "Signatory 27", "Test Corp", "27"),
                         ("signatory_28@testcorp.com", "Signatory 28", "Test Corp", "28"),
                         ("signatory_29@testcorp.com", "Signatory 29", "Test Corp", "29"),
                         ("signatory_30@testcorp.com", "Signatory 30", "Test Corp", "30"),
                         ("signatory_31@testcorp.com", "Signatory 31", "Test Corp", "31"),
                         ("signatory_32@testcorp.com", "Signatory 32", "Test Corp", "32"),
                         ("signatory_33@testcorp.com", "Signatory 33", "Test Corp", "33"),
                         ("signatory_34@testcorp.com", "Signatory 34", "Test Corp", "34"),
                         ("signatory_35@testcorp.com", "Signatory 35", "Test Corp", "35"),
                         ("signatory_36@testcorp.com", "Signatory 36", "Test Corp", "36"),
                         ("signatory_37@testcorp.com", "Signatory 37", "Test Corp", "37"),
                         ("signatory_38@testcorp.com", "Signatory 38", "Test Corp", "38"),
                         ("signatory_39@testcorp.com", "Signatory 39", "Test Corp", "39"),
                         ("signatory_40@testcorp.com", "Signatory 40", "Test Corp", "40"),
                         ("signatory_41@testcorp.com", "Signatory 41", "Test Corp", "41"),
                         ("signatory_42@testcorp.com", "Signatory 42", "Test Corp", "42"),
                         ("signatory_43@testcorp.com", "Signatory 43", "Test Corp", "43"),
                         ("signatory_44@testcorp.com", "Signatory 44", "Test Corp", "44"),
                         ("signatory_45@testcorp.com", "Signatory 45", "Test Corp", "45"),
                         ("signatory_46@testcorp.com", "Signatory 46", "Test Corp", "46"),
                         ("signatory_47@testcorp.com", "Signatory 47", "Test Corp", "47"),
                         ("signatory_48@testcorp.com", "Signatory 48", "Test Corp", "48"),
                         ("signatory_49@testcorp.com", "Signatory 49", "Test Corp", "49"),
                         ("signatory_50@testcorp.com", "Signatory 50", "Test Corp", "50"),
                         ("signatory_51@testcorp.com", "Signatory 51", "Test Corp", "51"),
                         ("signatory_52@testcorp.com", "Signatory 52", "Test Corp", "52"),
                         ("signatory_53@testcorp.com", "Signatory 53", "Test Corp", "53"),
                         ("signatory_54@testcorp.com", "Signatory 54", "Test Corp", "54"),
                         ("signatory_55@testcorp.com", "Signatory 55", "Test Corp", "55"),
                         ("signatory_56@testcorp.com", "Signatory 56", "Test Corp", "56"),
                         ("signatory_57@testcorp.com", "Signatory 57", "Test Corp", "57"),
                         ("signatory_58@testcorp.com", "Signatory 58", "Test Corp", "58"),
                         ("signatory_59@testcorp.com", "Signatory 59", "Test Corp", "59"),
                         ("signatory_60@testcorp.com", "Signatory 60", "Test Corp", "60"),
                         ("signatory_61@testcorp.com", "Signatory 61", "Test Corp", "61"),
                         ("signatory_62@testcorp.com", "Signatory 62", "Test Corp", "62"),
                         ("signatory_63@testcorp.com", "Signatory 63", "Test Corp", "63"),
                         ("signatory_64@testcorp.com", "Signatory 64", "Test Corp", "64"),
                         ("signatory_65@testcorp.com", "Signatory 65", "Test Corp", "65"),
                         ("signatory_66@testcorp.com", "Signatory 66", "Test Corp", "66"),
                         ("signatory_67@testcorp.com", "Signatory 67", "Test Corp", "67"),
                         ("signatory_68@testcorp.com", "Signatory 68", "Test Corp", "68"),
                         ("signatory_69@testcorp.com", "Signatory 69", "Test Corp", "69"),
                         ("signatory_70@testcorp.com", "Signatory 70", "Test Corp", "70"),
                         ("signatory_71@testcorp.com", "Signatory 71", "Test Corp", "71"),
                         ("signatory_72@testcorp.com", "Signatory 72", "Test Corp", "72"),
                         ("signatory_73@testcorp.com", "Signatory 73", "Test Corp", "73"),
                         ("signatory_74@testcorp.com", "Signatory 74", "Test Corp", "74"),
                         ("signatory_75@testcorp.com", "Signatory 75", "Test Corp", "75"),
                         ("signatory_76@testcorp.com", "Signatory 76", "Test Corp", "76"),
                         ("signatory_77@testcorp.com", "Signatory 77", "Test Corp", "77"),
                         ("signatory_78@testcorp.com", "Signatory 78", "Test Corp", "78"),
                         ("signatory_79@testcorp.com", "Signatory 79", "Test Corp", "79"),
                         ("signatory_80@testcorp.com", "Signatory 80", "Test Corp", "80"),
                         ("signatory_81@testcorp.com", "Signatory 81", "Test Corp", "81"),
                         ("signatory_82@testcorp.com", "Signatory 82", "Test Corp", "82"),
                         ("signatory_83@testcorp.com", "Signatory 83", "Test Corp", "83"),
                         ("signatory_84@testcorp.com", "Signatory 84", "Test Corp", "84"),
                         ("signatory_85@testcorp.com", "Signatory 85", "Test Corp", "85"),
                         ("signatory_86@testcorp.com", "Signatory 86", "Test Corp", "86"),
                         ("signatory_87@testcorp.com", "Signatory 87", "Test Corp", "87"),
                         ("signatory_88@testcorp.com", "Signatory 88", "Test Corp", "88"),
                         ("signatory_89@testcorp.com", "Signatory 89", "Test Corp", "89"),
                         ("signatory_90@testcorp.com", "Signatory 90", "Test Corp", "90"),
                         ("signatory_91@testcorp.com", "Signatory 91", "Test Corp", "91"),
                         ("signatory_92@testcorp.com", "Signatory 92", "Test Corp", "92"),
                         ("signatory_93@testcorp.com", "Signatory 93", "Test Corp", "93"),
                         ("signatory_94@testcorp.com", "Signatory 94", "Test Corp", "94"),
                         ("signatory_95@testcorp.com", "Signatory 95", "Test Corp", "95"),
                         ("signatory_96@testcorp.com", "Signatory 96", "Test Corp", "96"),
                         ("signatory_97@testcorp.com", "Signatory 97", "Test Corp", "97"),
                         ("signatory_98@testcorp.com", "Signatory 98", "Test Corp", "98"),
                         ("signatory_99@testcorp.com", "Signatory 99", "Test Corp", "99"),
                         ("signatory_100@testcorp.com", "Signatory 100", "Test Corp", "100")]
