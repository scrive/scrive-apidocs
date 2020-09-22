module Doc.API.V2.Calls.DocumentPostCallsTest (apiV2DocumentPostCallsTests) where

import Control.Monad.Trans
import Happstack.Server
import Test.Framework
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.Text as T

import AccessControl.Model
  ( AccessControlCreateForUser(..), AccessControlRemoveRole(..)
  )
import AccessControl.Types (AccessRole(..), AccessRoleTarget(..))
import DB.Query (dbQuery, dbUpdate)
import DigitalSignatureMethod
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.DocumentGetCalls (docApiV2Get)
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.SignatoryCalls (docApiV2SigSign)
import Doc.API.V2.Guards
import Doc.API.V2.Mock.TestUtils
import Doc.Class
import Doc.DocumentMonad (withDocumentID)
import Doc.Model
import Doc.Types.Document
import Doc.Types.DocumentStatus (DocumentStatus(..))
import Doc.Types.DocumentTag (DocumentTag(..))
import Doc.Types.SignatoryAccessToken
import Doc.Types.SignatoryConsentQuestion (SignatoryConsentQuestion(..))
import Doc.Types.SignatoryLink (SignatoryLink(..))
import EvidenceLog.Model
import File.Model
import Folder.Model
import Generators.DocumentGenerators
import Generators.OccurenceControl
import TestingUtil
import TestKontra
import User.Lang (defaultLang)
import Util.Actor (userActor)

apiV2DocumentPostCallsTests :: TestEnvSt -> Test
apiV2DocumentPostCallsTests env = testGroup
  "APIv2DocumentPostCalls"
  [ testThat "API v2 New"               env testDocApiV2New
  , testThat "API v2 New from template" env testDocApiV2NewFromTemplate
  , testThat "API v2 New from template for company shared"
             env
             testDocApiV2NewFromTemplateShared
  , testThat "API v2 New from template with BPID (RBS hack, see Core-1712)"
             env
             testDocApiV2NewFromTemplateWithBPID
  , testThat "API v2 Update"            env testDocApiV2Update
  , testThat "API v2 Start"             env testDocApiV2Start
  , testThat "API v2 Start for PAdES"   env testDocApiV2StartPades
  , testThat "API v2 Prolong"           env testDocApiV2Prolong
  , testThat "API v2 Prolong before document timeout" env testDocApiV2ProlongBeforeTimeout
  , testThat "API v2 Cancel"            env testDocApiV2Cancel
  , testThat "API v2 Trash"             env testDocApiV2Trash
  , testThat "API v2 Delete"            env testDocApiV2Delete
  , testThat "API v2 Trash (Multiple)"  env testDocApiV2TrashMultiple
  , testThat "API v2 Trash (Multiple) - DocID Limit" env testDocApiV2TrashMultipleLimit  -- checks anti-DoS measure
  , testThat "API v2 Delete (Multiple)" env testDocApiV2DeleteMultiple
  , testThat "API v2 Delete (Multiple) - DocID Limit" env testDocApiV2DeleteMultipleLimit -- checks anti-DoS measure
  , testThat "API v2 Remind"            env testDocApiV2Remind
  , testThat "API v2 Forward"           env testDocApiV2Forward
  , testThat "API v2 Set file"          env testDocApiV2SetFile
  , testThat "API v2 Set attachments"   env testDocApiV2SetAttachments
  , testThat "API v2 Set attachments with incremental=true"
             env
             testDocApiV2SetAttachmentsIncrementally
  , testThat "API v2 Set auto-reminder"         env testDocApiV2SetAutoReminder
  , testThat "API v2 Remove page"               env testDocApiV2RemovePages
  , testThat "API v2 Clone"                     env testDocApiV2Clone
  , testThat "API v2 Restart"                   env testDocApiV2Restart
  , testThat "API v2 Callback"                  env testDocApiV2Callback
  , testThat "API v2 Set sharing"               env testDocApiV2SetSharing
  , testThat "API v2 Set sharing - DocID Limit" env testDocApiV2SetSharingLimit     -- checks anti-DoS measure
  , testThat "API v2 Change email and mobile"   env testDocApiV2SigChangeEmailAndMobile
  , testThat
    "API v2 Update fails when a consent module is defined for a non-signing party"
    env
    testDocApiV2SigUpdateFailsIfConsentModuleOnNonSigningParty
  , testThat "Test API v2 'update' changing folder_id for document" env testDocInFolder
  , testThat "API v2 Update sets responses to null in the consent module"
             env
             testDocApiV2SigUpdateNoConsentResponses
  , testThat "API v2 Generate shareable link for template"
             env
             testDocApiV2GenerateShareableLink
  , testThat "API v2 Discard shareable link for template"
             env
             testDocApiV2DiscardShareableLink
  , testThat "API v2 Add image"              env testDocApiV2AddImage
  , testThat "API V2 Add evidence log event" env testDocApiV2AddEvidenceLogEvent
  , testThat "API v2 Set user_group_to_impersonate_for_eid"
             env
             testDocUpdateImpersonateEID
  , testThat "API v2 Start document impersonating other user group for eid"
             env
             testDocApiV2StartImpersonateEID
  , testThat "API v2 New from template impersonating other user group for eid"
             env
             testDocApiV2NewFromTemplateImpersonateEID
  ]

testDocApiV2New :: TestEnv ()
testDocApiV2New = do
  user   <- instantiateRandomUser
  ctx    <- mkContextWithUser defaultLang user
  status <- getMockDocStatus <$> testDocApiV2New' ctx
  assertEqual "Document should be in preparation" Preparation status

testDocApiV2NewFromTemplate :: TestEnv ()
testDocApiV2NewFromTemplate = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx
  tmpl <- dbQuery $ GetDocumentByDocumentID did
  assertEqual "Document is in user's folder"
              (Just $ documentfolderid tmpl)
              (user ^. #homeFolderID)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_template <- getMockDocIsTemplate <$> mockDocTestRequestHelper
      ctx
      POST
      [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did)
      200
    assertEqual "Document should be template" True is_template
    tmpl2 <- dbQuery $ GetDocumentByDocumentID did
    assertEqual "Template is still in user's folder"
                (Just $ documentfolderid tmpl2)
                (user ^. #homeFolderID)


  do -- Just to ensure limited scope so we don't test against the wrong thing
    mDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2NewFromTemplate did) 201
    assertEqual "New document should NOT be template" False (getMockDocIsTemplate mDoc)
    doc <- dbQuery . GetDocumentByDocumentID $ getMockDocId mDoc
    assertEqual "New document is in user's folder"
                (Just $ documentfolderid doc)
                (user ^. #homeFolderID)

testDocApiV2NewFromTemplateShared :: TestEnv ()
testDocApiV2NewFromTemplateShared = do
  ug <- instantiateRandomUserGroup
  let ugid = ug ^. #id
  author    <- instantiateUser $ randomUserTemplate { groupID = return ugid }
  ctxauthor <- mkContextWithUser defaultLang author
  did       <- getMockDocId <$> testDocApiV2New' ctxauthor

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_template <- getMockDocIsTemplate <$> mockDocTestRequestHelper
      ctxauthor
      POST
      [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did)
      200
    assertEqual "Document should be template" True is_template

  void . randomUpdate $ SetDocumentSharing [did] True
  user <- instantiateUser $ randomUserTemplate { groupID = return ugid }
  ctx  <- mkContextWithUser defaultLang user

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2NewFromTemplate did) 201
    assertEqual "New document should NOT be template" False (getMockDocIsTemplate mDoc)

    doc <- dbQuery . GetDocumentByDocumentID $ getMockDocId mDoc
    assertEqual "New document is in user's folder"
                (Just $ documentfolderid doc)
                (user ^. #homeFolderID)

testDocApiV2NewFromTemplateWithBPID :: TestEnv ()
testDocApiV2NewFromTemplateWithBPID = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx
  tmpl <- dbQuery $ GetDocumentByDocumentID did
  assertEqual "Document is in user's folder"
              (Just $ documentfolderid tmpl)
              (user ^. #homeFolderID)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_template <- getMockDocIsTemplate <$> mockDocTestRequestHelper
      ctx
      POST
      [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did)
      200
    assertEqual "Document should be template" True is_template
    tmpl2 <- dbQuery $ GetDocumentByDocumentID did
    assertEqual "Template is still in user's folder"
                (Just $ documentfolderid tmpl2)
                (user ^. #homeFolderID)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    let testbpid = "testbpid"
    mDoc <- mockDocTestRequestHelper ctx
                                     POST
                                     [("bpid", inText testbpid)]
                                     (docApiV2NewFromTemplate did)
                                     201
    assertEqual "New document should NOT be template" False (getMockDocIsTemplate mDoc)
    doc <- dbQuery . GetDocumentByDocumentID $ getMockDocId mDoc
    assertEqual "New document is in user's folder"
                (Just $ documentfolderid doc)
                (user ^. #homeFolderID)
    assertEqual "New document has BPID tag"
                (S.singleton (DocumentTag "bpid" testbpid))
                (documenttags doc)

testDocApiV2Update :: TestEnv ()
testDocApiV2Update = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  let new_title = "testTitle blah 42$#$%^"
  updated_title <- getMockDocTitle <$> mockDocTestRequestHelper
    ctx
    POST
    [("document", inText $ "{\"title\":\"" <> new_title <> "\"}")]
    (docApiV2Update did)
    200
  assertEqual "Title should be updated" new_title (T.pack updated_title)

testDocApiV2SigUpdateFailsIfConsentModuleOnNonSigningParty :: TestEnv ()
testDocApiV2SigUpdateFailsIfConsentModuleOnNonSigningParty = do
  user     <- instantiateRandomUser
  ctx      <- mkContextWithUser defaultLang user
  did      <- getMockDocId <$> testDocApiV2New' ctx

  contents <- liftIO . readFile $ inTestDir
    "json/api_v2/test-DocUpdateConsentModuleOnNonSigningParty.json"
  response <- testRequestHelper ctx
                                POST
                                [("document", inText $ T.pack contents)]
                                (docApiV2Update did)
                                400

  assertBool "The error is about the consent module"
             ("onsent module" `BS.isInfixOf` BSL.toStrict response)

testDocApiV2SigUpdateNoConsentResponses :: TestEnv ()
testDocApiV2SigUpdateNoConsentResponses = do
  user     <- instantiateRandomUser
  ctx      <- mkContextWithUser defaultLang user
  did      <- getMockDocId <$> testDocApiV2New' ctx

  contents <- liftIO . readFile $ inTestDir
    "json/api_v2/test-DocUpdateNoConsentResponses.json"
  void $ testRequestHelper ctx
                           POST
                           [("document", inText $ T.pack contents)]
                           (docApiV2Update did)
                           200

  withDocumentID did $ do
    sls <- documentsignatorylinks <$> theDocument
    let allNull = flip all sls
          $ \sl -> all (isNothing . scqResponse) (signatorylinkconsentquestions sl)
    assertBool "All the consent responses should be null" allNull

testDocApiV2Start :: TestEnv ()
testDocApiV2Start = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  void $ testDocApiV2StartNew ctx

testDocApiV2StartPades :: TestEnv ()
testDocApiV2StartPades = do
  user <- instantiateRandomPadesUser
  ctx  <- mkContextWithUser defaultLang user
  mdoc <- testDocApiV2StartNew ctx
  doc  <- dbQuery $ GetDocumentByDocumentID (getMockDocId mdoc)
  assertEqual "Document's sealing method should be"
              Pades
              (documentdigitalsignaturemethod doc)

testDocApiV2Prolong :: TestEnv ()
testDocApiV2Prolong = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
  mockDoc <- testDocApiV2StartNew ctx
  assertEqual "Default number of days should match" 90 $ getMockDocDaysToSign mockDoc
  let did = getMockDocId mockDoc
  withDocumentID did $ do
    dbUpdate $ TimeoutDocument (userActor ctx user)
  -- Current limit is 365 days
  void $ jsonTestRequestHelper ctx POST [("days", inText "366")] (docApiV2Prolong did) 400
  prolonged_status <- getMockDocStatus <$> mockDocTestRequestHelper
    ctx
    POST
    [("days", inText "365")]
    (docApiV2Prolong did)
    200
  assertEqual "Document status should match" Pending prolonged_status

testDocApiV2ProlongBeforeTimeout :: TestEnv ()
testDocApiV2ProlongBeforeTimeout = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
  mockDoc <- testDocApiV2StartNew ctx
  assertEqual "Default number of days should match" 90 $ getMockDocDaysToSign mockDoc
  let did = getMockDocId mockDoc
  -- Current limit is 365 days, ensure we still enforce it
  void $ jsonTestRequestHelper ctx POST [("days", inText "366")] (docApiV2Prolong did) 400
  -- Also for pending documents current timeout + days should not be greater then 365
  void $ jsonTestRequestHelper ctx POST [("days", inText "300")] (docApiV2Prolong did) 400
  prolonged_status <- getMockDocStatus <$> mockDocTestRequestHelper
    ctx
    POST
    [("days", inText "200")]
    (docApiV2Prolong did)
    200
  assertEqual "Document status should match" Pending prolonged_status

testDocApiV2Cancel :: TestEnv ()
testDocApiV2Cancel = do
  user          <- instantiateRandomUser
  ctx           <- mkContextWithUser defaultLang user
  did           <- getMockDocId <$> testDocApiV2StartNew ctx

  cancel_status <- getMockDocStatus
    <$> mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200
  assertEqual "Document status should match" Canceled cancel_status

testDocApiV2Trash :: TestEnv ()
testDocApiV2Trash = do
  user       <- instantiateRandomUser
  ctx        <- mkContextWithUser defaultLang user
  did        <- getMockDocId <$> testDocApiV2New' ctx

  is_trashed <- getMockDocIsTrashed
    <$> mockDocTestRequestHelper ctx POST [] (docApiV2Trash did) 200
  assertEqual "Document should be trashed after call" True is_trashed

testDocApiV2Delete :: TestEnv ()
testDocApiV2Delete = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  void $ mockDocTestRequestHelper ctx POST [] (docApiV2Trash did) 200

  mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Delete did) 200
  assertEqual "Document should be trashed after call" True (getMockDocIsTrashed mockDoc)
  assertEqual "Document should be deleted after call" True (getMockDocIsDeleted mockDoc)

testDocApiV2TrashMultiple :: TestEnv ()
testDocApiV2TrashMultiple = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
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
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  let input = [("document_ids", inText . showt $ map show [1 .. 101])]
  response <- testRequestHelper ctx POST input docApiV2TrashMultiple 400
  assertBool
    "DocApiV2TrashMultiple should error if given more than 100 document_ids"
    (              "document_ids parameter can't have more than 100 positions"
    `BS.isInfixOf` BSL.toStrict response
    )

testDocApiV2DeleteMultiple :: TestEnv ()
testDocApiV2DeleteMultiple = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
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
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  let input = [("document_ids", inText . showt $ map show [1 .. 101])]
  response <- testRequestHelper ctx POST input docApiV2DeleteMultiple 400
  assertBool
    "docApiV2DeleteMultiple should error if given more than 100 document_ids"
    (              "document_ids parameter can't have more than 100 positions"
    `BS.isInfixOf` BSL.toStrict response
    )

testDocApiV2Remind :: TestEnv ()
testDocApiV2Remind = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2StartNew ctx
  void $ testRequestHelper ctx POST [] (docApiV2Remind did) 202

testDocApiV2Forward :: TestEnv ()
testDocApiV2Forward = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
  mockDoc <- testDocApiV2StartNew ctx
  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 1 mockDoc

  void $ mockDocTestRequestHelper
    ctx
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did slid)
    200

  void $ testRequestHelper ctx
                           POST
                           [("email", inText "2.a2@22.e.aa")]
                           (docApiV2Forward did)
                           202

testDocApiV2SetFile :: TestEnv ()
testDocApiV2SetFile = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do -- Just to ensure limited scope so we don't test against the wrong thing
    hasFile <- getMockDocHasFile
      <$> mockDocTestRequestHelper ctx POST [] (docApiV2SetFile did) 200
    assertBool "There should be no file set" (not hasFile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    hasFile <- getMockDocHasFile <$> mockDocTestRequestHelper
      ctx
      POST
      [("file", inFile $ inTestDir "pdfs/simple-rotate-180.pdf")]
      (docApiV2SetFile did)
      200
    assertBool "There should now be a file set" hasFile

testDocApiV2SetAttachments :: TestEnv ()
testDocApiV2SetAttachments = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mda <- mockDocTestRequestHelper
      ctx
      POST
      [ ( "attachments"
        , inText
        $ "["
        <> "{\"name\" : \"A1\", \"required\" : false, \"add_to_sealed_file\" : true, \"file_param\" : \"attachment_0\"},"
        <> "{\"name\" : \"A2\", \"required\" : true, \"add_to_sealed_file\" : false, \"file_param\" : \"other_attachment\"}"
        <> "]"
        )
      , ("attachment_0"    , inFile $ inTestDir "pdfs/simple-rotate-90.pdf")
      , ("other_attachment", inFile $ inTestDir "pdfs/simple-rotate-180.pdf")
      ]
      (docApiV2SetAttachments did)
      200
    assertEqual "Number of 'author_attachments' should match those set"
                2
                (getMockDocAuthorAttachmentLength mda)

    assertEqual "Attachment 'A1' should be named as such"
                "A1"
                (getMockDocAuthorAttachmentName 1 mda)
    assertEqual "Attachment 'A1' should not be required"
                False
                (getMockDocAuthorAttachmentRequired 1 mda)
    assertEqual "Attachment 'A1' should be added to sealed file"
                True
                (getMockAuthorAttachmentAddedToSealedFile 1 mda)
    assertBool "Attachment 'A1' should have a file set"
               (getMockDocAuthorAttachmentHasFile 1 mda)

    assertEqual "Attachment 'A2' should be named as such"
                "A2"
                (getMockDocAuthorAttachmentName 2 mda)
    assertEqual "Attachment 'A2' should be required"
                True
                (getMockDocAuthorAttachmentRequired 2 mda)
    assertEqual "Attachment 'A2' should not be added to sealed file"
                False
                (getMockAuthorAttachmentAddedToSealedFile 2 mda)

    assertBool "Attachment 'A2' should have a file set"
               (getMockDocAuthorAttachmentHasFile 2 mda)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mdnoa <- mockDocTestRequestHelper ctx
                                      POST
                                      [("attachments", inText "[]")]
                                      (docApiV2SetAttachments did)
                                      200
    assertEqual "Number of 'author_attachments' should match those set"
                0
                (getMockDocAuthorAttachmentLength mdnoa)

testDocApiV2SetAttachmentsIncrementally :: TestEnv ()
testDocApiV2SetAttachmentsIncrementally = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  void $ mockDocTestRequestHelper
    ctx
    POST
    [ ( "attachments"
      , inText
        "[{\"name\" : \"A1\", \"required\" : false, \"add_to_sealed_file\" : true, \"file_param\" : \"attachment_0\"}]"
      )
    , ("attachment_0", inFile $ inTestDir "pdfs/simple-rotate-90.pdf")
    ]
    (docApiV2SetAttachments did)
    200

  -- It adds a new attachment without touching the old one.
  do
    mda <- mockDocTestRequestHelper
      ctx
      POST
      [ ( "attachments"
        , inText
          "[{\"name\" : \"A2\", \"required\" : true, \"add_to_sealed_file\" : false, \"file_param\" : \"other_attachment\"}]"
        )
      , ("other_attachment", inFile $ inTestDir "pdfs/simple-rotate-180.pdf")
      , ("incremental"     , inText "true")
      ]
      (docApiV2SetAttachments did)
      200

    assertEqual "Number of 'author_attachments' should match those set"
                2
                (getMockDocAuthorAttachmentLength mda)

    assertEqual "Attachment 'A1' should be named as such"
                "A1"
                (getMockDocAuthorAttachmentName 1 mda)
    assertEqual "Attachment 'A1' should not be required"
                False
                (getMockDocAuthorAttachmentRequired 1 mda)
    assertEqual "Attachment 'A1' should be added to sealed file"
                True
                (getMockAuthorAttachmentAddedToSealedFile 1 mda)
    assertBool "Attachment 'A1' should have a file set"
               (getMockDocAuthorAttachmentHasFile 1 mda)

    assertEqual "Attachment 'A2' should be named as such"
                "A2"
                (getMockDocAuthorAttachmentName 2 mda)
    assertEqual "Attachment 'A2' should be required"
                True
                (getMockDocAuthorAttachmentRequired 2 mda)
    assertEqual "Attachment 'A2' should not be added to sealed file"
                False
                (getMockAuthorAttachmentAddedToSealedFile 2 mda)

    assertBool "Attachment 'A2' should have a file set"
               (getMockDocAuthorAttachmentHasFile 2 mda)

  -- It returns an error if two attachments have the same name.
  void $ testRequestHelper
    ctx
    POST
    [ ( "attachments"
      , inText
        "[{\"name\" : \"A2\", \"required\" : true, \"add_to_sealed_file\" : false, \"file_param\" : \"other_attachment\"}, {\"name\" : \"A2\", \"required\" : true, \"add_to_sealed_file\" : false, \"file_param\" : \"yet_another_attachment\"}]"
      )
    , ("other_attachment", inFile $ inTestDir "pdfs/simple-rotate-180.pdf")
    , ("yet_another_attachment", inFile $ inTestDir "pdfs/50page.pdf")
    , ("incremental"           , inText "true")
    ]
    (docApiV2SetAttachments did)
    400

  -- It overwrites the attachment with the same name.
  do
    mda <- mockDocTestRequestHelper
      ctx
      POST
      [ ( "attachments"
        , inText
          "[{\"name\" : \"A2\", \"required\" : false, \"add_to_sealed_file\" : true, \"file_param\" : \"other_attachment\"}]"
        )
      , ("other_attachment", inFile $ inTestDir "pdfs/50page.pdf")
      , ("incremental"     , inText "true")
      ]
      (docApiV2SetAttachments did)
      200

    assertEqual "Attachment 'A2' should be named as such"
                "A2"
                (getMockDocAuthorAttachmentName 2 mda)
    assertEqual "Attachment 'A2' should not be required"
                False
                (getMockDocAuthorAttachmentRequired 2 mda)
    assertEqual "Attachment 'A2' should be added to sealed file"
                True
                (getMockAuthorAttachmentAddedToSealedFile 2 mda)

    assertBool "Attachment 'A2' should have a file set"
               (getMockDocAuthorAttachmentHasFile 2 mda)

  return ()

testDocApiV2SetAutoReminder :: TestEnv ()
testDocApiV2SetAutoReminder = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2StartNew ctx

  void $ getMockDocHasAutoRemindTime <$> mockDocTestRequestHelper
    ctx
    POST
    [("days", inText "89")]
    (docApiV2SetAutoReminder did)
    200
  -- FIXME setting this doesn't update the auto remind time
  -- immediately, bug in core?  assertJust auto_remind_time

testDocApiV2Clone :: TestEnv ()
testDocApiV2Clone = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
  mockDoc <- testDocApiV2New' ctx
  let did = getMockDocId mockDoc

  mockDocClone <- mockDocTestRequestHelper ctx POST [] (docApiV2Clone did) 201
  assertEqual "Cloned document should have same structure as original"
              (cleanMockDocForComparison mockDoc)
              (cleanMockDocForComparison mockDocClone)

testDocApiV2RemovePages :: TestEnv ()
testDocApiV2RemovePages = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do -- File changes and name stays the same when removing pages
    mocDocWith50PagesFile <- mockDocTestRequestHelper
      ctx
      POST
      [("file", inFile $ inTestDir "pdfs/50page.pdf")]
      (docApiV2SetFile did)
      200
    mockDocWithoutFewPages <- mockDocTestRequestHelper ctx
                                                       POST
                                                       [("pages", inText "[1,50]")]
                                                       (docApiV2RemovePages did)
                                                       200
    assertBool
      "After removing pages file name is not changed"
      (  getMockDocFileName mockDocWithoutFewPages
      == getMockDocFileName mocDocWith50PagesFile
      )
    assertBool
      "After removing pages file changes"
      (getMockDocFileId mockDocWithoutFewPages /= getMockDocFileId mocDocWith50PagesFile)

testDocApiV2Restart :: TestEnv ()
testDocApiV2Restart = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
  mockDoc <- testDocApiV2StartNew ctx
  let did = getMockDocId mockDoc

  void $ mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200

  mockDocRestart <- mockDocTestRequestHelper ctx POST [] (docApiV2Restart did) 201
  assertEqual "Restarted document should have same structure as original"
              (cleanMockDocForComparison mockDoc)
              (cleanMockDocForComparison mockDocRestart)

testDocApiV2Callback :: TestEnv ()
testDocApiV2Callback = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
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
  assertEqual "Document after callback should have same structure as original"
              (cleanMockDocForComparison mockDocStart)
              (cleanMockDocForComparison mockDocAfterCallback)
  assertEqual "Document after callback should be exactly the same"
              mockDocStart
              mockDocAfterCallback

  _cancel <- mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200
  -- Should work after document is cancelled too
  void $ testRequestHelper ctx POST [] (docApiV2Callback did) 202

testDocApiV2SetSharing :: TestEnv ()
testDocApiV2SetSharing = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do
    check <- getMockDocIsTemplate <$> mockDocTestRequestHelper
      ctx
      POST
      [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did)
      200
    assertEqual "Document should be template" True check

  forM_ [(True, "true"), (False, "false")] $ \(value, param) -> do
    void $ testRequestHelper
      ctx
      POST
      [ ("document_ids", inText $ T.pack ("[" <> show (show did) <> "]"))
      , ("shared"      , inText param)
      ]
      docApiV2SetSharing
      202
    isShared <- mockDocIsShared
      <$> mockDocTestRequestHelper ctx GET [] (docApiV2Get did) 200
    assertEqual "Document should have correct sharing" value isShared

-- IMPORTANT NOTE: This test checks that the document_ids length limit functions.
--                 This is important to mitigate a potential DoS attack vector.
testDocApiV2SetSharingLimit :: TestEnv ()
testDocApiV2SetSharingLimit = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  let idList = show $ map show [1 .. 101]
      input  = [("document_ids", inText $ T.pack idList), ("shared", inText "true")]
  response <- testRequestHelper ctx POST input docApiV2SetSharing 400
  assertBool
    "docApiV2SetSharing should error if given more than 100 document_ids"
    (              "document_ids parameter can't have more than 100 positions"
    `BS.isInfixOf` BSL.toStrict response
    )

testDocApiV2SigChangeEmailAndMobile :: TestEnv ()
testDocApiV2SigChangeEmailAndMobile = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user

  -- Params that we will re-use
  let param_email x = ("email", inText x)
      valid_email   = "person+23@scrive.se"
      invalid_email = "@random_junk.foo"
      param_mobile x = ("mobile_number", inText x)
      valid_mobile   = "+46987654321"
      invalid_mobile = "45678"
      orig_email     = "testperson@scrive.se"
      orig_mobile    = "+46123456789"
  -- Creates a document with extra signatory, that has the right fields
  let documentForTest = do
        draftDoc <- testDocApiV2New' ctx
        let did = getMockDocId draftDoc
            updateDoc =
              mockDocToInput
                . setMockDocSigLinkStandardField 2 "email"  orig_email
                . setMockDocSigLinkStandardField 2 "mobile" orig_mobile
                $ addStandardSigLinksToMockDoc 1 draftDoc
        _start <- getMockDocId <$> mockDocTestRequestHelper ctx
                                                            POST
                                                            [("document", updateDoc)]
                                                            (docApiV2Update did)
                                                            200
        mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
        assertEqual "Document status should match after 'start' call"
                    Pending
                    (getMockDocStatus mockDoc)
        return (did, getMockDocSigLinkId 1 mockDoc, getMockDocSigLinkId 2 mockDoc)

  liftIO $ print "DUPA0"
  -- You should not be able to change the author's email or mobile
  do
    (did, author_slid, _slid) <- documentForTest
    void $ jsonTestRequestHelper ctx
                                 POST
                                 [param_email valid_email, param_mobile valid_mobile]
                                 (docApiV2SigChangeEmailAndMobile did author_slid)
                                 409
    void $ jsonTestRequestHelper ctx
                                 POST
                                 [param_email valid_email]
                                 (docApiV2SigChangeEmailAndMobile did author_slid)
                                 409
    void $ jsonTestRequestHelper ctx
                                 POST
                                 [param_mobile valid_mobile]
                                 (docApiV2SigChangeEmailAndMobile did author_slid)
                                 409
  liftIO $ print "DUPA1"
  -- Should work for other signatory
  do
    (did, _author_slid, slid) <- documentForTest
    -- Try invalid combinations
    void $ testRequestHelper ctx
                             POST
                             [param_email invalid_email, param_mobile invalid_mobile]
                             (docApiV2SigChangeEmailAndMobile did slid)
                             400
    void $ testRequestHelper ctx
                             POST
                             [param_email valid_email, param_mobile invalid_mobile]
                             (docApiV2SigChangeEmailAndMobile did slid)
                             400
    void $ testRequestHelper ctx
                             POST
                             [param_email invalid_email, param_mobile valid_mobile]
                             (docApiV2SigChangeEmailAndMobile did slid)
                             400
    -- Then test valid case
    emailAndPhone <- mockDocTestRequestHelper
      ctx
      POST
      [param_email valid_email, param_mobile valid_mobile]
      (docApiV2SigChangeEmailAndMobile did slid)
      200
    assertEqual "Email should have changed"
                valid_email
                (T.pack $ getMockDocSigLinkEmail 2 emailAndPhone)
    assertEqual "Mobile should have changed"
                valid_mobile
                (T.pack $ getMockDocSigLinkMobileNumber 2 emailAndPhone)
  liftIO $ print "DUPA2"
  do
    (did, _author_slid, slid) <- documentForTest
    void $ testRequestHelper ctx
                             POST
                             [param_email invalid_email]
                             (docApiV2SigChangeEmailAndMobile did slid)
                             400
    emailOnly <- mockDocTestRequestHelper ctx
                                          POST
                                          [param_email valid_email]
                                          (docApiV2SigChangeEmailAndMobile did slid)
                                          200
    assertEqual "Email should have changed"
                valid_email
                (T.pack $ getMockDocSigLinkEmail 2 emailOnly)
    assertEqual "Mobile should NOT have changed"
                orig_mobile
                (getMockDocSigLinkMobileNumber 2 emailOnly)
  liftIO $ print "DUPA3"
  do
    (did, _author_slid, slid) <- documentForTest
    void $ testRequestHelper ctx
                             POST
                             [param_mobile invalid_mobile]
                             (docApiV2SigChangeEmailAndMobile did slid)
                             400
    liftIO $ print "DUPA4"
    mobileOnly <- mockDocTestRequestHelper ctx
                                           POST
                                           [param_mobile valid_mobile]
                                           (docApiV2SigChangeEmailAndMobile did slid)
                                           200
    liftIO $ print "DUPA5"
    assertEqual "Email should NOT have changed"
                orig_email
                (getMockDocSigLinkEmail 2 mobileOnly)
    assertEqual "Mobile should have changed"
                valid_mobile
                (T.pack $ getMockDocSigLinkMobileNumber 2 mobileOnly)

  do -- If we change mobile, we only want to change access token for mobile
    (did, _author_slid, slid) <- documentForTest
    mhForEmail                <- dbUpdate
      $ NewSignatoryAccessToken slid SignatoryAccessTokenForMailBeforeClosing Nothing
    mhForSMS <- dbUpdate
      $ NewSignatoryAccessToken slid SignatoryAccessTokenForSMSBeforeClosing Nothing
    _ <- mockDocTestRequestHelper ctx
                                  POST
                                  [param_email valid_email]
                                  (docApiV2SigChangeEmailAndMobile did slid)
                                  200
    sl <- dbQuery $ GetSignatoryLinkByID did slid
    assert . not $ any ((== mhForEmail) . signatoryAccessTokenHash)
                       (signatoryaccesstokens sl)
    assert $ any ((== mhForSMS) . signatoryAccessTokenHash) (signatoryaccesstokens sl)

  do -- If we change email, we only want to change access token for email
    (did, _author_slid, slid) <- documentForTest
    mhForEmail                <- dbUpdate
      $ NewSignatoryAccessToken slid SignatoryAccessTokenForMailBeforeClosing Nothing
    mhForSMS <- dbUpdate
      $ NewSignatoryAccessToken slid SignatoryAccessTokenForSMSBeforeClosing Nothing
    _ <- mockDocTestRequestHelper ctx
                                  POST
                                  [param_mobile valid_mobile]
                                  (docApiV2SigChangeEmailAndMobile did slid)
                                  200
    sl <- dbQuery $ GetSignatoryLinkByID did slid
    assert $ any ((== mhForEmail) . signatoryAccessTokenHash) (signatoryaccesstokens sl)
    assert . not $ any ((== mhForSMS) . signatoryAccessTokenHash)
                       (signatoryaccesstokens sl)

testDocApiV2GenerateShareableLink :: TestEnv ()
testDocApiV2GenerateShareableLink = replicateM_ 100 $ do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user

  doc  <- do
    fid  <- addNewRandomFile
    file <- randomQuery $ GetFileByFileID fid
    doc2 <- rand 10 . runOccurenceControl 0.5 $ startableDocumentOC (user ^. #id) file
    let doc3 = doc2 { documentfolderid = fromJust $ user ^. #homeFolderID }
    did <- randomUpdate $ StoreDocumentForTesting doc3
    randomQuery $ GetDocumentByDocumentID did

  if isTemplate doc && isNothing (documentCanBeStarted (doc { documenttype = Signable }))
    then do
      void $ testRequestHelper ctx
                               POST
                               []
                               (docApiV2GenerateShareableLink (documentid doc))
                               200

      doc' <- randomQuery . GetDocumentByDocumentID $ documentid doc

      assertNotEqual "Shareable link hash should have changed"
                     (documentshareablelinkhash doc)
                     (documentshareablelinkhash doc')
    else do
      void $ testRequestHelper ctx
                               POST
                               []
                               (docApiV2GenerateShareableLink (documentid doc))
                               409

testDocApiV2DiscardShareableLink :: TestEnv ()
testDocApiV2DiscardShareableLink = replicateM_ 10 $ do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  doc  <- addRandomDocument (rdaDefault user) { rdaTypes = OneOf [Template] }

  void $ testRequestHelper ctx POST [] (docApiV2DiscardShareableLink (documentid doc)) 202

  doc' <- randomQuery . GetDocumentByDocumentID $ documentid doc
  assertNothing $ documentshareablelinkhash doc'

testDocApiV2AddImage :: TestEnv ()
testDocApiV2AddImage = do
  user <- instantiateRandomUser
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  do -- File changes and name stays the same when adding images
    mocDocFile <- mockDocTestRequestHelper
      ctx
      POST
      [("file", inFile $ inTestDir "pdfs/simple.pdf")]
      (docApiV2SetFile did)
      200
    mockDocFileWithImage <- mockDocTestRequestHelper
      ctx
      POST
      [ ( "image"
        , inText
          "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAMAAAADCAYAAABWKLW/AAAABHNCSVQICAgIfAhkiAAAABVJREFUCJljvPJq+38GKGBiQAIoHAB+fwN657AIuwAAAABJRU5ErkJggg=="
        )
      , ("pageno", inText "1")
      , ("x"     , inText "0.5")
      , ("y"     , inText "0.5")
      ]
      (docApiV2AddImage did)
      200
    assertBool
      "After adding image file name is not changed"
      (getMockDocFileName mockDocFileWithImage == getMockDocFileName mocDocFile)
    assertBool "After adding image file changes"
               (getMockDocFileId mockDocFileWithImage /= getMockDocFileId mocDocFile)

testDocInFolder :: TestEnv ()
testDocInFolder = do
  admin    <- instantiateUser $ randomUserTemplate { isCompanyAdmin = True }
  adminCtx <- mkContextWithUser defaultLang admin
  nonAdmin <- instantiateUser
    $ randomUserTemplate { groupID = return $ admin ^. #groupID }
  let nonAdminCtx       = set #maybeUser (Just nonAdmin) adminCtx

  let adminHomeFolderId = fromJust $ admin ^. #homeFolderID
  -- user tries to create document in admins home folder - should fail
  let createInNonOwnedFolder =
        [ ("file", inFile $ inTestDir "pdfs/simple.pdf")
        , ("saved"    , inText "false")
        , ("folder_id", inText . showt $ adminHomeFolderId)
        ]
  _ <- mockDocTestRequestHelper nonAdminCtx POST createInNonOwnedFolder docApiV2New 403

  -- create subfolder
  adminSubfolder <- dbUpdate . FolderCreate $ set #parentID
                                                  (Just adminHomeFolderId)
                                                  defaultFolder
  let subfolderID = adminSubfolder ^. #id
  -- create document in subfolder
  let createInSubfolder =
        [ ("file", inFile $ inTestDir "pdfs/simple.pdf")
        , ("saved"    , inText "false")
        , ("folder_id", inText . showt $ subfolderID)
        ]
  doc2 <- mockDocTestRequestHelper adminCtx POST createInSubfolder docApiV2New 201
  let did_2 = getMockDocId doc2
  void $ mockDocTestRequestHelper adminCtx POST [] (docApiV2Get did_2) 200
  assertEqual "Returned document has provided folder_id"
              subfolderID
              (getMockDocFolderId doc2)

  -- verify that document has subfolderID
  docFromDB <- dbQuery $ GetDocumentByDocumentID did_2
  assertEqual "Created document has the provided folder_id"
              (adminSubfolder ^. #id)
              (documentfolderid docFromDB)

  -- create document from template in subfolder
  _ <- mockDocTestRequestHelper adminCtx
                                POST
                                [("document", inText "{\"is_template\": true}")]
                                (docApiV2Update did_2)
                                200
  let createFromTemplateinSubfolder = [("folder_id", inText . showt $ subfolderID)]
  doc3 <- mockDocTestRequestHelper adminCtx
                                   POST
                                   createFromTemplateinSubfolder
                                   (docApiV2NewFromTemplate did_2)
                                   201
  assertEqual "Returned document has provided folder id"
              subfolderID
              (getMockDocFolderId doc3)

  -- update document and move it to user home folder
  let docMove = inText $ T.pack "{\"folder_id\": \"" <> showt adminHomeFolderId <> "\"}"
      rq_update_params = [("document", docMove)]
      rq_update_code = 200

  -- moving the document as regular user should not work
  _ <- mockDocTestRequestHelper nonAdminCtx
                                POST
                                rq_update_params
                                (docApiV2Update did_2)
                                403

  -- moving the document to empty folder should not work
  let docMoveToNothing = inText $ T.pack "{\"folder_id\": null}"
  _ <- mockDocTestRequestHelper adminCtx
                                POST
                                [("document", docMoveToNothing)]
                                (docApiV2Update did_2)
                                400

  -- moving the document as admin to home folder should succeed
  doc4 <- mockDocTestRequestHelper adminCtx
                                   POST
                                   rq_update_params
                                   (docApiV2Update did_2)
                                   rq_update_code

  -- verify that document has home folderID now
  assertEqual "Moved document has user home folderID returned"
              adminHomeFolderId
              (getMockDocFolderId doc4)

testDocApiV2AddEvidenceLogEvent :: TestEnv ()
testDocApiV2AddEvidenceLogEvent = do
  user <- instantiateRandomUser
  -- siglink <- rand 1 $ randomSigLinkByStatus Closed
  ctx  <- mkContextWithUser defaultLang user
  did  <- getMockDocId <$> testDocApiV2New' ctx

  _    <- testRequestHelper ctx
                            POST
                            [("text", inText "blarg")]
                            (docApiV2AddEvidenceEvent did)
                            201

  lg <- dbQuery $ GetEvidenceLog did
  let rejectEvent e = evType e == Current CustomEventEvidence
  assertJust $ find rejectEvent lg

  return ()

testDocUpdateImpersonateEID :: TestEnv ()
testDocUpdateImpersonateEID = do
  user <- instantiateRandomUser
  let uid = user ^. #id
  ctx <- mkContextWithUser defaultLang user
  did <- getMockDocId <$> testDocApiV2New' ctx

  -- User group to impersonate
  ug  <- instantiateRandomUserGroup
  let ugid = ug ^. #id

  let update =
        "{\"experimental_features\": {\"user_group_to_impersonate_for_eid\":\""
          <> showt ugid
          <> "\"}}"
  let params = [("document", inText update)]
  request  <- mkRequestWithHeaders POST params []

  (res, _) <- runTestKontra request ctx $ docApiV2Update did
  assertEqual
    "Setting 'user_group_to_impersonate_for_eid' without sufficient\
    \ permissions should fail"
    403
    (rsCode res)

  -- grant impersonate role
  void . dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  mdoc <- mockDocTestRequestHelper ctx POST params (docApiV2Update did) 200
  assertEqual "'user_group_to_impersonate_for_eid' should match"
              (Just (show ugid))
              (getMockDocUserGroupForEid mdoc)

testDocApiV2StartImpersonateEID :: TestEnv ()
testDocApiV2StartImpersonateEID = do
  user <- instantiateRandomUser
  let uid = user ^. #id
  ctx <- mkContextWithUser defaultLang user
  did <- getMockDocId <$> testDocApiV2New' ctx

  -- User group to impersonate
  ug  <- instantiateRandomUserGroup
  let ugid = ug ^. #id

  -- grant impersonate role
  Just (AccessRoleUser roleid _ _) <-
    dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  do  -- set 'user_group_to_impersonate_for_eid' field
    let update =
          "{\"experimental_features\": {\"user_group_to_impersonate_for_eid\":\""
            <> showt ugid
            <> "\"}}"
    let params = [("document", inText update)]

    mdoc <- mockDocTestRequestHelper ctx POST params (docApiV2Update did) 200
    assertEqual "user_group_to_impersonate_for_eid should match"
                (Just (show ugid))
                (getMockDocUserGroupForEid mdoc)

  -- remove impersonate role again
  void . dbUpdate $ AccessControlRemoveRole roleid

  emptyPOSTRequest <- mkRequestWithHeaders POST [] []
  (resFail, _)     <- runTestKontra emptyPOSTRequest ctx $ docApiV2Start did
  assertEqual
    "Starting a document impersonating another user group without sufficient\
    \ permissions should fail"
    403
    (rsCode resFail)

  -- grant impersonate role again
  void . dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  (resSucc, _) <- runTestKontra emptyPOSTRequest ctx $ docApiV2Start did
  assertEqual
    "Starting a document impersonating another user group with sufficient\
    \ permissions should succeed"
    200
    (rsCode resSucc)

testDocApiV2NewFromTemplateImpersonateEID :: TestEnv ()
testDocApiV2NewFromTemplateImpersonateEID = do
  user <- instantiateRandomUser
  let uid = user ^. #id
  ctx <- mkContextWithUser defaultLang user
  did <- getMockDocId <$> testDocApiV2New' ctx

  -- User group to impersonate
  ug  <- instantiateRandomUserGroup
  let ugid = ug ^. #id

  -- grant impersonate role
  Just (AccessRoleUser roleid _ _) <-
    dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  do  -- set 'user_group_to_impersonate_for_eid' field and make template
    let update =
          "{\"experimental_features\": {\"user_group_to_impersonate_for_eid\":\""
            <> showt ugid
            <> "\"}"
            <> ", \"is_template\" : true}"
    let params = [("document", inText update)]

    mdoc <- mockDocTestRequestHelper ctx POST params (docApiV2Update did) 200
    assertEqual "user_group_to_impersonate_for_eid should match"
                (Just (show ugid))
                (getMockDocUserGroupForEid mdoc)

  -- remove impersonate role again
  void . dbUpdate $ AccessControlRemoveRole roleid

  emptyPOSTRequest <- mkRequestWithHeaders POST [] []
  (resFail, _)     <- runTestKontra emptyPOSTRequest ctx $ docApiV2NewFromTemplate did
  assertEqual
    "Starting a document from template impersonating another user group without\
    \ sufficient permissions should fail"
    403
    (rsCode resFail)

  -- grant impersonate role again
  void . dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  (resSucc, _) <- runTestKontra emptyPOSTRequest ctx $ docApiV2NewFromTemplate did
  assertEqual
    "Starting a document from template impersonating another user group with\
    \ sufficient permissions should succeed"
    201
    (rsCode resSucc)
