module Doc.API.V2.Calls.DocumentPostCallsTest (apiV2DocumentPostCallsTests) where

import Control.Monad.Trans
import Happstack.Server
import Test.Framework
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Context
import DB.Query (dbQuery, dbUpdate)
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.DocumentGetCalls (docApiV2Get)
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.SignatoryCalls (docApiV2SigSign)
import Doc.API.V2.Guards
import Doc.API.V2.Mock.TestUtils
import Doc.Class
import Doc.DocumentMonad (withDocumentID)
import Doc.Model.Query
import Doc.Model.Update
import Doc.Types.Document
import Doc.Types.DocumentStatus (DocumentStatus(..))
import Doc.Types.SignatoryAccessToken
import Doc.Types.SignatoryConsentQuestion (SignatoryConsentQuestion(..))
import Doc.Types.SignatoryLink (SignatoryLink(..))
import EvidenceLog.Model
import File.Model
import Generators.DocumentGenerators
import Generators.OccurenceControl
import TestingUtil
import TestKontra
import User.Lang (defaultLang)
import User.Model
import UserGroup.Types
import Util.Actor (userActor)

apiV2DocumentPostCallsTests :: TestEnvSt -> Test
apiV2DocumentPostCallsTests env = testGroup "APIv2DocumentPostCalls"
  [ testThat "API v2 New"                                   env testDocApiV2New
  , testThat "API v2 New from template"                     env testDocApiV2NewFromTemplate
  , testThat "API v2 New from template for company shared"  env testDocApiV2NewFromTemplateShared
  , testThat "API v2 Update"                                env testDocApiV2Update
  , testThat "API v2 Start"                                 env testDocApiV2Start
  , testThat "API v2 Prolong"                               env testDocApiV2Prolong
  , testThat "API v2 Prolong before document timeout"       env testDocApiV2ProlongBeforeTimeout
  , testThat "API v2 Cancel"                                env testDocApiV2Cancel
  , testThat "API v2 Trash"                                 env testDocApiV2Trash
  , testThat "API v2 Delete"                                env testDocApiV2Delete
  , testThat "API v2 Trash (Multiple)"                      env testDocApiV2TrashMultiple
  , testThat "API v2 Trash (Multiple) - DocID Limit"        env testDocApiV2TrashMultipleLimit  -- checks anti-DoS measure
  , testThat "API v2 Delete (Multiple)"                     env testDocApiV2DeleteMultiple
  , testThat "API v2 Delete (Multiple) - DocID Limit"       env testDocApiV2DeleteMultipleLimit -- checks anti-DoS measure
  , testThat "API v2 Remind"                                env testDocApiV2Remind
  , testThat "API v2 Forward"                               env testDocApiV2Forward
  , testThat "API v2 Set file"                              env testDocApiV2SetFile
  , testThat "API v2 Set attachments"                       env testDocApiV2SetAttachments
  , testThat "API v2 Set attachments with incremental=true" env testDocApiV2SetAttachmentsIncrementally
  , testThat "API v2 Set auto-reminder"                     env testDocApiV2SetAutoReminder
  , testThat "API v2 Remove page"                           env testDocApiV2RemovePages
  , testThat "API v2 Clone"                                 env testDocApiV2Clone
  , testThat "API v2 Restart"                               env testDocApiV2Restart
  , testThat "API v2 Callback"                              env testDocApiV2Callback
  , testThat "API v2 Set sharing"                           env testDocApiV2SetSharing
  , testThat "API v2 Set sharing - DocID Limit"             env testDocApiV2SetSharingLimit     -- checks anti-DoS measure
  , testThat "API v2 Change email and mobile"               env testDocApiV2SigChangeEmailAndMobile

  , testThat "API v2 Update fails when a consent module is defined for a non-signing party"
             env testDocApiV2SigUpdateFailsIfConsentModuleOnNonSigningParty
  , testThat "API v2 Update sets responses to null in the consent module"
             env testDocApiV2SigUpdateNoConsentResponses

  , testThat "API v2 Generate shareable link for template"
             env testDocApiV2GenerateShareableLink
  , testThat "API v2 Discard shareable link for template"
             env testDocApiV2DiscardShareableLink

  , testThat "API v2 Add image"
             env testDocApiV2AddImage
  , testThat "API V2 Add evidence log event"
             env testDocApiV2AddEvidenceLogEvent
  ]

testDocApiV2New :: TestEnv ()
testDocApiV2New = do
  user   <- addNewRandomUser
  ctx    <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  status <- getMockDocStatus <$> testDocApiV2New' ctx
  assertEqual "Document should be in preparation" Preparation status

testDocApiV2NewFromTemplate :: TestEnv ()
testDocApiV2NewFromTemplate = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx
  tmpl <- dbQuery $ GetDocumentByDocumentID did
  assertEqual "Document is in user's folder" (documentfolderid tmpl) (userhomefolderid user)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_template <- getMockDocIsTemplate <$> mockDocTestRequestHelper ctx
      POST [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did) 200
    assertEqual "Document should be template" True is_template
    tmpl2 <- dbQuery $ GetDocumentByDocumentID did
    assertEqual "Template is still in user's folder" (documentfolderid tmpl2) (userhomefolderid user)


  do -- Just to ensure limited scope so we don't test against the wrong thing
    mDoc <-  mockDocTestRequestHelper ctx POST [] (docApiV2NewFromTemplate did) 201
    assertEqual "New document should NOT be template" False (getMockDocIsTemplate mDoc)
    doc <- dbQuery . GetDocumentByDocumentID $ getMockDocId mDoc
    assertEqual "New document is in user's folder" (documentfolderid doc) (userhomefolderid user)

testDocApiV2NewFromTemplateShared :: TestEnv ()
testDocApiV2NewFromTemplateShared = do
  ug <- addNewUserGroup
  let ugid = get ugID ug
  author                <- addNewRandomCompanyUser ugid False
  ctxauthor             <- set ctxmaybeuser (Just author) <$> mkContext defaultLang
  did                   <- getMockDocId <$> testDocApiV2New' ctxauthor

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_template <- getMockDocIsTemplate <$> mockDocTestRequestHelper ctxauthor
      POST [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did) 200
    assertEqual "Document should be template" True is_template

  void  $ randomUpdate $ SetDocumentSharing [did] True
  user <- addNewRandomCompanyUser ugid False
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mDoc <-  mockDocTestRequestHelper ctx POST [] (docApiV2NewFromTemplate did) 201
    assertEqual "New document should NOT be template" False (getMockDocIsTemplate mDoc)

    doc <- dbQuery . GetDocumentByDocumentID $ getMockDocId mDoc
    assertEqual "New document is in user's folder" (documentfolderid doc) (userhomefolderid user)

testDocApiV2Update :: TestEnv ()
testDocApiV2Update = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  let new_title = "testTitle blah 42$#$%^"
  updated_title <- getMockDocTitle <$> mockDocTestRequestHelper ctx
    POST [("document", inText $ "{\"title\":\"" <> new_title <> "\"}")] (docApiV2Update did) 200
  assertEqual "Title should be updated" new_title (T.pack updated_title)

testDocApiV2SigUpdateFailsIfConsentModuleOnNonSigningParty :: TestEnv ()
testDocApiV2SigUpdateFailsIfConsentModuleOnNonSigningParty = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  contents <- liftIO $ readFile $
    inTestDir "json/api_v2/test-DocUpdateConsentModuleOnNonSigningParty.json"
  response <- testRequestHelper ctx
    POST [("document", inText $ T.pack contents)] (docApiV2Update did) 400

  assertBool "The error is about the consent module"
             ("onsent module" `BS.isInfixOf` BSL.toStrict response)

testDocApiV2SigUpdateNoConsentResponses :: TestEnv ()
testDocApiV2SigUpdateNoConsentResponses = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  contents <- liftIO $ readFile $
    inTestDir "json/api_v2/test-DocUpdateNoConsentResponses.json"
  void $ testRequestHelper ctx
    POST [("document", inText $ T.pack contents)] (docApiV2Update did) 200

  withDocumentID did $ do
    sls <- documentsignatorylinks <$> theDocument
    let allNull = flip all sls $ \sl ->
          all (isNothing . scqResponse) (signatorylinkconsentquestions sl)
    assertBool "All the consent responses should be null" allNull

testDocApiV2Start :: TestEnv ()
testDocApiV2Start = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  void  $ testDocApiV2StartNew ctx

testDocApiV2Prolong :: TestEnv ()
testDocApiV2Prolong = do
  user    <- addNewRandomUser
  ctx     <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  assertEqual "Default number of days should match" 90 $ getMockDocDaysToSign mockDoc
  let did = getMockDocId mockDoc
  withDocumentID did $ do
    dbUpdate $ TimeoutDocument (userActor ctx user)
  -- Current limit is 365 days
  void $ jsonTestRequestHelper ctx POST [("days", inText "366")] (docApiV2Prolong did) 400
  prolonged_status <- getMockDocStatus <$> mockDocTestRequestHelper ctx POST [("days", inText "365")] (docApiV2Prolong did) 200
  assertEqual "Document status should match" Pending prolonged_status

testDocApiV2ProlongBeforeTimeout :: TestEnv()
testDocApiV2ProlongBeforeTimeout = do
  user    <- addNewRandomUser
  ctx     <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  assertEqual "Default number of days should match" 90 $ getMockDocDaysToSign mockDoc
  let did = getMockDocId mockDoc
  -- Current limit is 365 days, ensure we still enforce it
  void $ jsonTestRequestHelper ctx POST [("days", inText "366")] (docApiV2Prolong did) 400
  -- Also for pending documents current timeout + days should not be greater then 365
  void $ jsonTestRequestHelper ctx POST [("days", inText "300")] (docApiV2Prolong did) 400
  prolonged_status <- getMockDocStatus <$> mockDocTestRequestHelper ctx POST [("days", inText "200")] (docApiV2Prolong did) 200
  assertEqual "Document status should match" Pending prolonged_status

testDocApiV2Cancel :: TestEnv ()
testDocApiV2Cancel = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2StartNew ctx

  cancel_status <- getMockDocStatus <$> mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200
  assertEqual "Document status should match" Canceled cancel_status

testDocApiV2Trash :: TestEnv ()
testDocApiV2Trash = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  is_trashed <- getMockDocIsTrashed <$> mockDocTestRequestHelper ctx POST [] (docApiV2Trash did) 200
  assertEqual "Document should be trashed after call" True is_trashed

testDocApiV2Delete :: TestEnv ()
testDocApiV2Delete = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  void $ mockDocTestRequestHelper ctx POST [] (docApiV2Trash did) 200

  mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Delete did) 200
  assertEqual "Document should be trashed after call" True (getMockDocIsTrashed mockDoc)
  assertEqual "Document should be deleted after call" True (getMockDocIsDeleted mockDoc)

testDocApiV2TrashMultiple :: TestEnv ()
testDocApiV2TrashMultiple = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did1 <- getMockDocId <$> testDocApiV2New' ctx
  did2 <- getMockDocId <$> testDocApiV2New' ctx
  did3 <- getMockDocId <$> testDocApiV2New' ctx
  let input = [("document_ids", inText . showt $ map show [did1, did2, did3])]
  mockDocs <- mockDocTestRequestHelperMultiple ctx POST input docApiV2TrashMultiple 200
  forM_ mockDocs $ \mockDoc -> do
    assertEqual "Document should be trashed after call" True $ getMockDocIsTrashed mockDoc

-- IMPORTANT NOTE: This test checks that the document_ids length limit functions.
--                 This is important to mitigate a potential DoS attack vector.
testDocApiV2TrashMultipleLimit :: TestEnv ()
testDocApiV2TrashMultipleLimit = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  let input = [("document_ids", inText . showt $ map show [1..101])]
  response <- testRequestHelper ctx POST input docApiV2TrashMultiple 400
  assertBool "DocApiV2TrashMultiple should error if given more than 100 document_ids"
    ("document_ids parameter can't have more than 100 positions"
      `BS.isInfixOf` BSL.toStrict response)

testDocApiV2DeleteMultiple :: TestEnv ()
testDocApiV2DeleteMultiple = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did1 <- getMockDocId <$> testDocApiV2New' ctx
  did2 <- getMockDocId <$> testDocApiV2New' ctx
  did3 <- getMockDocId <$> testDocApiV2New' ctx
  let input = [("document_ids", inText . showt $ map show [did1, did2, did3])]
  void $ mockDocTestRequestHelperMultiple ctx POST input docApiV2TrashMultiple 200
  mockDocs <- mockDocTestRequestHelperMultiple ctx POST input docApiV2DeleteMultiple 200
  forM_ mockDocs $ \mockDoc -> do
    assertEqual "Document should be trashed after call" True $ getMockDocIsTrashed mockDoc
    assertEqual "Document should be deleted after call" True $ getMockDocIsDeleted mockDoc

-- IMPORTANT NOTE: This test checks that the document_ids length limit functions.
--                 This is important to mitigate a potential DoS attack vector.
testDocApiV2DeleteMultipleLimit :: TestEnv ()
testDocApiV2DeleteMultipleLimit = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  let input = [("document_ids", inText . showt $ map show [1..101])]
  response <- testRequestHelper ctx POST input docApiV2DeleteMultiple 400
  assertBool "docApiV2DeleteMultiple should error if given more than 100 document_ids"
    ("document_ids parameter can't have more than 100 positions"
      `BS.isInfixOf` BSL.toStrict response)

testDocApiV2Remind :: TestEnv ()
testDocApiV2Remind = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2StartNew ctx
  void  $ testRequestHelper ctx POST [] (docApiV2Remind did) 202

testDocApiV2Forward :: TestEnv ()
testDocApiV2Forward = do
  user    <- addNewRandomUser
  ctx     <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 1 mockDoc

  void $ mockDocTestRequestHelper ctx
    POST
      [ ("fields", inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
    (docApiV2SigSign did slid) 200

  void $ testRequestHelper ctx POST [("email", inText "2.a2@22.e.aa")]
    (docApiV2Forward did) 202

testDocApiV2SetFile :: TestEnv ()
testDocApiV2SetFile = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do -- Just to ensure limited scope so we don't test against the wrong thing
    hasFile <- getMockDocHasFile <$> mockDocTestRequestHelper ctx POST [] (docApiV2SetFile did) 200
    assertBool "There should be no file set" (not hasFile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    hasFile <- getMockDocHasFile <$> mockDocTestRequestHelper ctx
      POST [("file", inFile $ inTestDir "pdfs/simple-rotate-180.pdf")]
      (docApiV2SetFile did) 200
    assertBool "There should now be a file set" hasFile

testDocApiV2SetAttachments :: TestEnv ()
testDocApiV2SetAttachments = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mda <- mockDocTestRequestHelper ctx
      POST [
        ("attachments", inText $ "[" <>
            "{\"name\" : \"A1\", \"required\" : false, \"add_to_sealed_file\" : true, \"file_param\" : \"attachment_0\"}," <>
            "{\"name\" : \"A2\", \"required\" : true, \"add_to_sealed_file\" : false, \"file_param\" : \"other_attachment\"}" <>
        "]")
      ,("attachment_0", inFile $ inTestDir "pdfs/simple-rotate-90.pdf")
      ,("other_attachment", inFile $ inTestDir "pdfs/simple-rotate-180.pdf")
      ]
      (docApiV2SetAttachments did) 200
    assertEqual "Number of 'author_attachments' should match those set" 2 (getMockDocAuthorAttachmentLength mda)

    assertEqual "Attachment 'A1' should be named as such" "A1" (getMockDocAuthorAttachmentName 1 mda)
    assertEqual "Attachment 'A1' should not be required" False (getMockDocAuthorAttachmentRequired 1 mda)
    assertEqual "Attachment 'A1' should be added to sealed file" True (getMockAuthorAttachmentAddedToSealedFile 1 mda)
    assertBool "Attachment 'A1' should have a file set" (getMockDocAuthorAttachmentHasFile 1 mda)

    assertEqual "Attachment 'A2' should be named as such" "A2" (getMockDocAuthorAttachmentName 2 mda)
    assertEqual "Attachment 'A2' should be required" True (getMockDocAuthorAttachmentRequired 2 mda)
    assertEqual "Attachment 'A2' should not be added to sealed file" False (getMockAuthorAttachmentAddedToSealedFile 2 mda)

    assertBool "Attachment 'A2' should have a file set" (getMockDocAuthorAttachmentHasFile 2 mda)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mdnoa <- mockDocTestRequestHelper ctx
      POST [("attachments", inText "[]")] (docApiV2SetAttachments did) 200
    assertEqual "Number of 'author_attachments' should match those set" 0 (getMockDocAuthorAttachmentLength mdnoa)

testDocApiV2SetAttachmentsIncrementally :: TestEnv ()
testDocApiV2SetAttachmentsIncrementally = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  did <- getMockDocId <$> testDocApiV2New' ctx

  void $ mockDocTestRequestHelper ctx
    POST [
      ("attachments", inText "[{\"name\" : \"A1\", \"required\" : false, \"add_to_sealed_file\" : true, \"file_param\" : \"attachment_0\"}]")
    , ("attachment_0", inFile $ inTestDir "pdfs/simple-rotate-90.pdf")
    ]
    (docApiV2SetAttachments did) 200

  -- It adds a new attachment without touching the old one.
  do
    mda <- mockDocTestRequestHelper ctx
      POST [
        ("attachments", inText "[{\"name\" : \"A2\", \"required\" : true, \"add_to_sealed_file\" : false, \"file_param\" : \"other_attachment\"}]")
      , ("other_attachment", inFile $ inTestDir "pdfs/simple-rotate-180.pdf")
      , ("incremental", inText "true")
      ]
      (docApiV2SetAttachments did) 200

    assertEqual "Number of 'author_attachments' should match those set" 2 (getMockDocAuthorAttachmentLength mda)

    assertEqual "Attachment 'A1' should be named as such" "A1" (getMockDocAuthorAttachmentName 1 mda)
    assertEqual "Attachment 'A1' should not be required" False (getMockDocAuthorAttachmentRequired 1 mda)
    assertEqual "Attachment 'A1' should be added to sealed file" True (getMockAuthorAttachmentAddedToSealedFile 1 mda)
    assertBool "Attachment 'A1' should have a file set" (getMockDocAuthorAttachmentHasFile 1 mda)

    assertEqual "Attachment 'A2' should be named as such" "A2" (getMockDocAuthorAttachmentName 2 mda)
    assertEqual "Attachment 'A2' should be required" True (getMockDocAuthorAttachmentRequired 2 mda)
    assertEqual "Attachment 'A2' should not be added to sealed file" False (getMockAuthorAttachmentAddedToSealedFile 2 mda)

    assertBool "Attachment 'A2' should have a file set" (getMockDocAuthorAttachmentHasFile 2 mda)

  -- It returns an error if two attachments have the same name.
  void $ testRequestHelper ctx
    POST [
      ("attachments", inText "[{\"name\" : \"A2\", \"required\" : true, \"add_to_sealed_file\" : false, \"file_param\" : \"other_attachment\"}, {\"name\" : \"A2\", \"required\" : true, \"add_to_sealed_file\" : false, \"file_param\" : \"yet_another_attachment\"}]")
    , ("other_attachment", inFile $ inTestDir "pdfs/simple-rotate-180.pdf")
    , ("yet_another_attachment", inFile $ inTestDir "pdfs/50page.pdf")
    , ("incremental", inText "true")
    ]
    (docApiV2SetAttachments did) 400

  -- It overwrites the attachment with the same name.
  do
    mda <- mockDocTestRequestHelper ctx
      POST [
        ("attachments", inText "[{\"name\" : \"A2\", \"required\" : false, \"add_to_sealed_file\" : true, \"file_param\" : \"other_attachment\"}]")
      , ("other_attachment", inFile $ inTestDir "pdfs/50page.pdf")
      , ("incremental", inText "true")
      ]
      (docApiV2SetAttachments did) 200

    assertEqual "Attachment 'A2' should be named as such" "A2" (getMockDocAuthorAttachmentName 2 mda)
    assertEqual "Attachment 'A2' should not be required" False (getMockDocAuthorAttachmentRequired 2 mda)
    assertEqual "Attachment 'A2' should be added to sealed file" True (getMockAuthorAttachmentAddedToSealedFile 2 mda)

    assertBool "Attachment 'A2' should have a file set" (getMockDocAuthorAttachmentHasFile 2 mda)

  return ()

testDocApiV2SetAutoReminder :: TestEnv ()
testDocApiV2SetAutoReminder = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2StartNew ctx

  void $ getMockDocHasAutoRemindTime <$> mockDocTestRequestHelper ctx
    POST [("days", inText "89")] (docApiV2SetAutoReminder did) 200
  -- FIXME setting this doesn't update the auto remind time
  -- immediately, bug in core?  assertJust auto_remind_time

testDocApiV2Clone :: TestEnv ()
testDocApiV2Clone = do
  user    <- addNewRandomUser
  ctx     <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2New' ctx
  let did = getMockDocId mockDoc

  mockDocClone <- mockDocTestRequestHelper ctx POST [] (docApiV2Clone did) 201
  assertEqual "Cloned document should have same structure as original" (cleanMockDocForComparison mockDoc) (cleanMockDocForComparison mockDocClone)

testDocApiV2RemovePages :: TestEnv ()
testDocApiV2RemovePages = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do -- File changes and name stays the same when removing pages
    mocDocWith50PagesFile <- mockDocTestRequestHelper ctx
      POST [("file", inFile $ inTestDir "pdfs/50page.pdf")]
      (docApiV2SetFile did) 200
    mockDocWithoutFewPages  <- mockDocTestRequestHelper ctx
      POST [("pages", inText "[1,50]")]
      (docApiV2RemovePages did) 200
    assertBool "After removing pages file name is not changed" (getMockDocFileName mockDocWithoutFewPages == getMockDocFileName  mocDocWith50PagesFile)
    assertBool "After removing pages file changes" (getMockDocFileId mockDocWithoutFewPages /= getMockDocFileId mocDocWith50PagesFile)

testDocApiV2Restart :: TestEnv ()
testDocApiV2Restart = do
  user    <- addNewRandomUser
  ctx     <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  let did = getMockDocId mockDoc

  void $ mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200

  mockDocRestart <- mockDocTestRequestHelper ctx POST [] (docApiV2Restart did) 201
  assertEqual "Restarted document should have same structure as original" (cleanMockDocForComparison mockDoc) (cleanMockDocForComparison mockDocRestart)

testDocApiV2Callback :: TestEnv ()
testDocApiV2Callback = do
  user    <- addNewRandomUser
  ctx     <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2New' ctx
  let did = getMockDocId mockDoc

  -- Should fail for documents in preparation
  void $ testRequestHelper ctx POST [] (docApiV2Callback did) 409

  mockDocStart <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200

  -- TODO
  -- Right now as API v2 is not "active", V2 callbacks will not run
  -- When it becomes active this test should expand to test that a callback URL
  -- is actually called, by setting up some mock server or something...
  void $ testRequestHelper ctx POST [] (docApiV2Callback did) 202

  mockDocAfterCallback <- mockDocTestRequestHelper ctx GET [] (docApiV2Get did) 200
  assertEqual "Document after callback should have same structure as original" (cleanMockDocForComparison mockDocStart) (cleanMockDocForComparison mockDocAfterCallback)
  assertEqual "Document after callback should be exactly the same" mockDocStart mockDocAfterCallback

  _cancel <- mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200
  -- Should work after document is cancelled too
  void $ testRequestHelper ctx POST [] (docApiV2Callback did) 202

testDocApiV2SetSharing :: TestEnv ()
testDocApiV2SetSharing = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do
    check <- getMockDocIsTemplate <$> mockDocTestRequestHelper ctx
      POST [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did) 200
    assertEqual "Document should be template" True check

  forM_ [(True, "true"), (False, "false")] $ \(value, param) -> do
    void $ testRequestHelper ctx POST
      [ ("document_ids", inText $ T.pack ("[" <> show (show did) <> "]"))
      , ("shared", inText param) ]
      docApiV2SetSharing 202
    isShared <- mockDocIsShared <$> mockDocTestRequestHelper ctx
      GET [] (docApiV2Get did) 200
    assertEqual "Document should have correct sharing" value isShared

-- IMPORTANT NOTE: This test checks that the document_ids length limit functions.
--                 This is important to mitigate a potential DoS attack vector.
testDocApiV2SetSharingLimit :: TestEnv ()
testDocApiV2SetSharingLimit = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  let idList = show $ map show [1..101]
      input = [("document_ids", inText $ T.pack idList), ("shared", inText "true")]
  response <- testRequestHelper ctx POST input docApiV2SetSharing 400
  assertBool "docApiV2SetSharing should error if given more than 100 document_ids"
    ("document_ids parameter can't have more than 100 positions"
      `BS.isInfixOf` BSL.toStrict response)

testDocApiV2SigChangeEmailAndMobile :: TestEnv ()
testDocApiV2SigChangeEmailAndMobile = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang

  -- Params that we will re-use
  let param_email x = ("email", inText x)
      valid_email = "person+23@scrive.se"
      invalid_email = "@random_junk.foo"
      param_mobile x = ("mobile_number", inText x)
      valid_mobile = "+46987654321"
      invalid_mobile = "45678"
      orig_email = "testperson@scrive.se"
      orig_mobile = "+46123456789"
  -- Creates a document with extra signatory, that has the right fields
  let documentForTest = do
        draftDoc <- testDocApiV2New' ctx
        let did = getMockDocId draftDoc
            updateDoc = mockDocToInput
              . setMockDocSigLinkStandardField 2 "email" orig_email
              . setMockDocSigLinkStandardField 2 "mobile" orig_mobile $
              addStandardSigLinksToMockDoc 1 draftDoc
        _start <- getMockDocId <$> mockDocTestRequestHelper ctx POST [("document", updateDoc)] (docApiV2Update did) 200
        mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
        assertEqual "Document status should match after 'start' call" Pending (getMockDocStatus mockDoc)
        return (did, getMockDocSigLinkId 1 mockDoc, getMockDocSigLinkId 2 mockDoc)

  liftIO $ print "DUPA0"
  -- You should not be able to change the author's email or mobile
  do
    (did, author_slid, _slid) <- documentForTest
    void $ jsonTestRequestHelper ctx POST [param_email valid_email, param_mobile valid_mobile]
      (docApiV2SigChangeEmailAndMobile did author_slid) 409
    void $ jsonTestRequestHelper ctx POST [param_email valid_email]
      (docApiV2SigChangeEmailAndMobile did author_slid) 409
    void $ jsonTestRequestHelper ctx POST [param_mobile valid_mobile]
      (docApiV2SigChangeEmailAndMobile did author_slid) 409
  liftIO $ print "DUPA1"
  -- Should work for other signatory
  do
    (did, _author_slid, slid) <- documentForTest
    -- Try invalid combinations
    void $ testRequestHelper ctx POST [param_email invalid_email, param_mobile invalid_mobile]
      (docApiV2SigChangeEmailAndMobile did slid) 400
    void $ testRequestHelper ctx POST [param_email valid_email, param_mobile invalid_mobile]
      (docApiV2SigChangeEmailAndMobile did slid) 400
    void $ testRequestHelper ctx POST [param_email invalid_email, param_mobile valid_mobile]
      (docApiV2SigChangeEmailAndMobile did slid) 400
    -- Then test valid case
    emailAndPhone <- mockDocTestRequestHelper ctx POST [param_email valid_email, param_mobile valid_mobile]
      (docApiV2SigChangeEmailAndMobile did slid) 200
    assertEqual "Email should have changed" valid_email (T.pack $ getMockDocSigLinkEmail 2 emailAndPhone)
    assertEqual "Mobile should have changed" valid_mobile (T.pack $ getMockDocSigLinkMobileNumber 2 emailAndPhone)
  liftIO $ print "DUPA2"
  do
    (did, _author_slid, slid) <- documentForTest
    void $ testRequestHelper ctx POST [param_email invalid_email]
      (docApiV2SigChangeEmailAndMobile did slid) 400
    emailOnly <- mockDocTestRequestHelper ctx POST [param_email valid_email]
      (docApiV2SigChangeEmailAndMobile did slid) 200
    assertEqual "Email should have changed" valid_email (T.pack $ getMockDocSigLinkEmail 2 emailOnly)
    assertEqual "Mobile should NOT have changed" orig_mobile (getMockDocSigLinkMobileNumber 2 emailOnly)
  liftIO $ print "DUPA3"
  do
    (did, _author_slid, slid) <- documentForTest
    void $ testRequestHelper ctx POST [param_mobile invalid_mobile]
      (docApiV2SigChangeEmailAndMobile did slid) 400
    liftIO $ print "DUPA4"
    mobileOnly <- mockDocTestRequestHelper ctx POST [param_mobile valid_mobile]
      (docApiV2SigChangeEmailAndMobile did slid) 200
    liftIO $ print "DUPA5"
    assertEqual "Email should NOT have changed" orig_email (getMockDocSigLinkEmail 2 mobileOnly)
    assertEqual "Mobile should have changed" valid_mobile (T.pack $ getMockDocSigLinkMobileNumber 2 mobileOnly)

  do -- If we change mobile, we only want to change access token for mobile
    (did, _author_slid, slid) <- documentForTest
    mhForEmail <- dbUpdate $ NewSignatoryAccessToken
      slid SignatoryAccessTokenForMailBeforeClosing Nothing
    mhForSMS <- dbUpdate $ NewSignatoryAccessToken
      slid SignatoryAccessTokenForSMSBeforeClosing Nothing
    _ <- mockDocTestRequestHelper ctx POST
      [param_email valid_email]
      (docApiV2SigChangeEmailAndMobile did slid) 200
    sl <- dbQuery $ GetSignatoryLinkByID did slid
    assert $ not $
      any ((==mhForEmail) . signatoryAccessTokenHash) (signatoryaccesstokens sl)
    assert $
      any ((==mhForSMS) . signatoryAccessTokenHash) (signatoryaccesstokens sl)

  do -- If we change email, we only want to change access token for email
    (did, _author_slid, slid) <- documentForTest
    mhForEmail <- dbUpdate $ NewSignatoryAccessToken
      slid SignatoryAccessTokenForMailBeforeClosing Nothing
    mhForSMS <- dbUpdate $ NewSignatoryAccessToken
      slid SignatoryAccessTokenForSMSBeforeClosing Nothing
    _ <- mockDocTestRequestHelper ctx POST
      [param_mobile valid_mobile]
      (docApiV2SigChangeEmailAndMobile did slid) 200
    sl <- dbQuery $ GetSignatoryLinkByID did slid
    assert $
      any ((==mhForEmail) . signatoryAccessTokenHash) (signatoryaccesstokens sl)
    assert $ not $
      any ((==mhForSMS) . signatoryAccessTokenHash) (signatoryaccesstokens sl)

testDocApiV2GenerateShareableLink :: TestEnv ()
testDocApiV2GenerateShareableLink = replicateM_ 100 $ do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang

  doc <- do
    fid  <- addNewRandomFile
    file <- randomQuery $ GetFileByFileID fid
    doc' <- rand 10 $ runOccurenceControl 0.5 $
      startableDocumentOC (userid user) file
    did  <- randomUpdate $ StoreDocumentForTesting doc'
    randomQuery $ GetDocumentByDocumentID did

  if isTemplate doc
     && isNothing (documentCanBeStarted (doc { documenttype = Signable }))
    then do
      void $ testRequestHelper ctx POST []
        (docApiV2GenerateShareableLink (documentid doc)) 200

      doc' <- randomQuery $ GetDocumentByDocumentID $ documentid doc

      assertNotEqual "Shareable link hash should have changed"
                     (documentshareablelinkhash doc)
                     (documentshareablelinkhash doc')

    else do
      void $ testRequestHelper ctx POST []
        (docApiV2GenerateShareableLink (documentid doc)) 409

testDocApiV2DiscardShareableLink :: TestEnv ()
testDocApiV2DiscardShareableLink = replicateM_ 10 $ do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  doc <- addRandomDocument (rdaDefault user)
    { rdaTypes = OneOf [Template]
    }

  void $ testRequestHelper ctx POST []
    (docApiV2DiscardShareableLink (documentid doc)) 202

  doc' <- randomQuery $ GetDocumentByDocumentID $ documentid doc
  assertNothing $ documentshareablelinkhash doc'

testDocApiV2AddImage :: TestEnv ()
testDocApiV2AddImage = do
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do -- File changes and name stays the same when adding images
    mocDocFile <- mockDocTestRequestHelper ctx
      POST [("file", inFile $ inTestDir "pdfs/simple.pdf")]
      (docApiV2SetFile did) 200
    mockDocFileWithImage  <- mockDocTestRequestHelper ctx
      POST [("image", inText "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAMAAAADCAYAAABWKLW/AAAABHNCSVQICAgIfAhkiAAAABVJREFUCJljvPJq+38GKGBiQAIoHAB+fwN657AIuwAAAABJRU5ErkJggg==")
          , ("pageno", inText "1")
          , ("x", inText "0.5")
          , ("y", inText "0.5")]
      (docApiV2AddImage did) 200
    assertBool "After adding image file name is not changed" (getMockDocFileName mockDocFileWithImage == getMockDocFileName mocDocFile)
    assertBool "After adding image file changes" (getMockDocFileId mockDocFileWithImage /= getMockDocFileId mocDocFile)

testDocApiV2AddEvidenceLogEvent :: TestEnv ()
testDocApiV2AddEvidenceLogEvent = do
  user    <- addNewRandomUser
  -- siglink <- rand 1 $ randomSigLinkByStatus Closed
  ctx     <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  did     <- getMockDocId <$> testDocApiV2New' ctx

  _ <- testRequestHelper
    ctx
    POST
    [("text", inText $ "blarg")]
    (docApiV2AddEvidenceEvent did) 201

  lg <- dbQuery $ GetEvidenceLog did
  let
    rejectEvent e = evType e == Current CustomEventEvidence
  assertJust $ find rejectEvent lg

  return ()
