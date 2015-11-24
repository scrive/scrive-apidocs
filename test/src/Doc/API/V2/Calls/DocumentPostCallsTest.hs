module Doc.API.V2.Calls.DocumentPostCallsTest
  ( apiV2DocumentPostCallsTests
  , testDocApiV2New'
  , testDocApiV2Start'
  ) where

import Data.Default
import Happstack.Server
import Test.Framework

import Company.Model
import Context
import DB.Query (dbUpdate)
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.SignatoryCalls (docApiV2SigSign)
import Doc.API.V2.MockTestUtils
import Doc.API.V2.MockUnjson
import Doc.DocumentID (DocumentID)
import Doc.DocumentMonad (withDocumentID)
import Doc.Model.Update (SetDocumentSharing(..), TimeoutDocument(..))
import KontraPrelude
import TestingUtil
import TestKontra
import User.Model (User)
import Util.Actor (userActor)

apiV2DocumentPostCallsTests :: TestEnvSt -> Test
apiV2DocumentPostCallsTests env = testGroup "APIv2DocumentPostCalls" $
  [ testThat "API v2 New"                                   env testDocApiV2New
  , testThat "API v2 New from template"                     env testDocApiV2NewFromTemplate
  , testThat "API v2 New from template for company shared"  env testDocApiV2NewFromTemplateShared
  , testThat "API v2 Update"                                env testDocApiV2Update
  , testThat "API v2 Start"                                 env testDocApiV2Start
  , testThat "API v2 Prolong"                               env testDocApiV2Prolong
  , testThat "API v2 Cancel"                                env testDocApiV2Cancel
  , testThat "API v2 Trash"                                 env testDocApiV2Trash
  , testThat "API v2 Delete"                                env testDocApiV2Delete
  , testThat "API v2 Remind"                                env testDocApiV2Remind
  , testThat "API v2 Forward"                               env testDocApiV2Forward
  , testThat "API v2 Set file"                              env testDocApiV2SetFile
  , testThat "API v2 Set attachments"                       env testDocApiV2SetAttachments
  , testThat "API v2 Set auto-reminder"                     env testDocApiV2SetAutoReminder
  , testThat "API v2 Clone"                                 env testDocApiV2Clone
  , testThat "API v2 Restart"                               env testDocApiV2Restart
  , testThat "API v2 Set signatory authentication to-view"  env testDocApiV2SigSetAuthenticationToView
  , testThat "API v2 Set signatory authentication to-sign"  env testDocApiV2SigSetAuthenticationToSign
  ]

testDocApiV2New :: TestEnv ()
testDocApiV2New = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  status <- mockDocStatus <$> testDocApiV2New' ctx
  assertEqual "Document should be in preparation" "preparation" status

testDocApiV2New' :: Context -> TestEnv MockDoc
testDocApiV2New' ctx = mockDocTestRequestHelper ctx POST [("file", inFile "test/pdfs/simple.pdf")] docApiV2New 201

testDocApiV2NewFromTemplate :: TestEnv ()
testDocApiV2NewFromTemplate = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_template <- mockDocIsTemplate <$> mockDocTestRequestHelper ctx
      POST [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did) 200
    assertEqual "Document should be template" True is_template

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_not_template <- mockDocIsTemplate <$> mockDocTestRequestHelper ctx POST [] (docApiV2NewFromTemplate did) 201
    assertEqual "New document should NOT be template" False is_not_template

testDocApiV2NewFromTemplateShared :: TestEnv ()
testDocApiV2NewFromTemplateShared = do
  (Company {companyid}) <- addNewCompany
  author <- addNewRandomCompanyUser companyid False
  ctxauthor <- (\c -> c { ctxmaybeuser = Just author }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctxauthor

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_template <- mockDocIsTemplate <$> mockDocTestRequestHelper ctxauthor
      POST [("document", inText "{\"is_template\":true}")]
      (docApiV2Update did) 200
    assertEqual "Document should be template" True is_template

  _ <- randomUpdate $ SetDocumentSharing [did] True
  user <- addNewRandomCompanyUser companyid False
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def

  do -- Just to ensure limited scope so we don't test against the wrong thing
    is_not_template <- mockDocIsTemplate <$> mockDocTestRequestHelper ctx POST [] (docApiV2NewFromTemplate did) 201
    assertEqual "New document should NOT be template" False is_not_template

testDocApiV2Update :: TestEnv ()
testDocApiV2Update = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  let new_title = "testTitle blah 42$#$%^"
  updated_title <- mockDocTitle <$> mockDocTestRequestHelper ctx
    POST [("document", inText $ "{\"title\":\"" ++ new_title ++ "\"}")] (docApiV2Update did) 200
  assertEqual "Title should be updated" new_title updated_title

testDocApiV2Start :: TestEnv ()
testDocApiV2Start = do
  _ <- testDocApiV2Start'
  return ()

testDocApiV2Start' :: TestEnv (User, Context, DocumentID, MockDoc)
testDocApiV2Start' = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx
  mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
  assertEqual "Document status should match after 'start' call" "pending" (mockDocStatus mockDoc)
  return (user, ctx, did, mockDoc)

testDocApiV2Prolong :: TestEnv ()
testDocApiV2Prolong = do
  (user, ctx, did, _) <- testDocApiV2Start'

  withDocumentID did $ do
    dbUpdate $ TimeoutDocument (userActor ctx user)
  -- FIXME check status for timed out

  prolong_status <- mockDocStatus <$> mockDocTestRequestHelper ctx POST [("days", inText "1")] (docApiV2Prolong did) 200
  assertEqual "Document status should match" "pending" prolong_status

testDocApiV2Cancel :: TestEnv ()
testDocApiV2Cancel = do
  (_, ctx, did, _) <- testDocApiV2Start'

  cancel_status <- mockDocStatus <$> mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200
  assertEqual "Document status should match" "canceled" cancel_status

testDocApiV2Trash :: TestEnv ()
testDocApiV2Trash = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  is_trashed <- mockDocIsTrashed <$> mockDocTestRequestHelper ctx POST [] (docApiV2Trash did) 200
  assertEqual "Document should be trashed after call" True is_trashed

testDocApiV2Delete :: TestEnv ()
testDocApiV2Delete = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  _ <- mockDocTestRequestHelper ctx POST [] (docApiV2Trash did) 200

  mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Delete did) 200
  assertEqual "Document should be trashed after call" True (mockDocIsTrashed mockDoc)
  assertEqual "Document should be deleted after call" True (mockDocIsDeleted mockDoc)

testDocApiV2Remind :: TestEnv ()
testDocApiV2Remind = do
  (_, ctx, did, _) <- testDocApiV2Start'
  _ <- mockDocTestRequestHelper ctx POST [] (docApiV2Remind did) 202
  return ()

testDocApiV2Forward :: TestEnv ()
testDocApiV2Forward = do
  (_, ctx, did, mockDoc) <- testDocApiV2Start'
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  _ <- mockDocTestRequestHelper ctx
    POST [("fields", inText "[]"),("accepted_author_attachments", inText "[]")]
    (docApiV2SigSign did slid) 200

  _ <- mockDocTestRequestHelper ctx POST [("email", inText "2.a2@22.e.aa")]
    (docApiV2Forward did) 202
  return ()

testDocApiV2SetFile :: TestEnv ()
testDocApiV2SetFile = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  do -- Just to ensure limited scope so we don't test against the wrong thing
    no_file <- mockDocFile <$> mockDocTestRequestHelper ctx POST [] (docApiV2SetFile did) 200
    assertNothing no_file

  do -- Just to ensure limited scope so we don't test against the wrong thing
    set_file <- mockDocFile <$> mockDocTestRequestHelper ctx
      POST [("file", inFile "test/pdfs/simple-rotate-180.pdf")]
      (docApiV2SetFile did) 200
    assertJust set_file

testDocApiV2SetAttachments :: TestEnv ()
testDocApiV2SetAttachments = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  do -- Just to ensure limited scope so we don't test against the wrong thing
    two_attachments <- mockDocAuthorAttachments <$> mockDocTestRequestHelper ctx
      POST [
        ("attachments", inText $ "[" <>
            "{\"name\" : \"A1\", \"required\" : false, \"file_param\" : \"attachment_0\"}," <>
            "{\"name\" : \"A2\", \"required\" : true, \"file_param\" : \"other_attachment\"}" <>
        "]")
      ,("attachment_0", inFile "test/pdfs/simple-rotate-90.pdf")
      ,("other_attachment", inFile "test/pdfs/simple-rotate-180.pdf")
      ]
      (docApiV2SetAttachments did) 200
    assertEqual "Number of 'author_attachments' should match those set" 2 (length two_attachments)
    let a1:_ = filter (\a -> mockAuthorAttachmentName a == "A1") two_attachments
        a2:_ = filter (\a -> mockAuthorAttachmentName a == "A2") two_attachments
    assertEqual "Attachment 'A1' should not be required" False (mockAuthorAttachmentRequired a1)
    assertEqual "Attachment 'A2' should be required" True (mockAuthorAttachmentRequired a2)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    no_attachments <- mockDocAuthorAttachments <$> mockDocTestRequestHelper ctx
      POST [("attachments", inText "[]")] (docApiV2SetAttachments did) 200
    assertEqual "Number of 'author_attachments' should match those set" 0 (length no_attachments)

testDocApiV2SetAutoReminder :: TestEnv ()
testDocApiV2SetAutoReminder = do
  (_, ctx, did, _) <- testDocApiV2Start'

  _auto_remind_time <- mockDocAutoRemindTime <$> mockDocTestRequestHelper ctx
    POST [("days", inText "89")] (docApiV2SetAutoReminder did) 200
  -- FIXME setting this doesn't update the auto remind time immediately, bug in core?
  -- assertJust auto_remind_time
  return ()

testDocApiV2Clone :: TestEnv ()
testDocApiV2Clone = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  mockDoc <- testDocApiV2New' ctx
  let did = documentIDFromMockDoc mockDoc

  mockDocClone <- mockDocTestRequestHelper ctx POST [] (docApiV2Clone did) 201
  assertEqual "Cloned document should have same structure as original" (mockDocToCompare mockDoc) (mockDocToCompare mockDocClone)

testDocApiV2Restart :: TestEnv ()
testDocApiV2Restart = do
  (_, ctx, did, mockDoc) <- testDocApiV2Start'

  _ <- mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200

  mockDocRestart <- mockDocTestRequestHelper ctx POST [] (docApiV2Restart did) 201
  assertEqual "Restarted document should have same structure as original" (mockDocToCompare mockDoc) (mockDocToCompare mockDocRestart)

testDocApiV2SigSetAuthenticationToView :: TestEnv ()
testDocApiV2SigSetAuthenticationToView = do
  (_,ctx,did,mockDoc) <- testDocApiV2Start'
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid = "se_bankid"
      no_bankid = "no_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10 = "1234567890"
      se_ssn_12 = "123456789012"
      no_ssn = "12345678901"
      param_mobile x = ("mobile_number", inText x)
      no_mobile = "+4712345678"

  -- Some invalid requests
  _ <- jsonTestRequestHelper ctx POST [param_ssn se_ssn_10]
    (docApiV2SigSetAuthenticationToView did slid) 400
  _ <- jsonTestRequestHelper ctx POST [param_auth "god_is_witness"]
    (docApiV2SigSetAuthenticationToView did slid) 400
  -- FIXME as this is random the SL could have a correct SSN already!
  _ <- jsonTestRequestHelper ctx POST [param_auth no_bankid]
    (docApiV2SigSetAuthenticationToView did slid) 409
  _ <- jsonTestRequestHelper ctx POST [param_auth no_bankid, param_ssn se_ssn_10]
    (docApiV2SigSetAuthenticationToView did slid) 409
  _ <- jsonTestRequestHelper ctx POST [param_auth se_bankid]
    (docApiV2SigSetAuthenticationToView did slid) 409
  _ <- jsonTestRequestHelper ctx POST [param_auth se_bankid, param_ssn no_ssn]
    (docApiV2SigSetAuthenticationToView did slid) 409

  let getAuthToView md = getForSigNumberFromMockDoc mockSigLinkAuthMethodToView md 1
      getPersonalNumber' md = getFieldValueOfTypeForSigNumberFromMockDoc' md 1 "personal_number"
      getMobileNumber' md = getFieldValueOfTypeForSigNumberFromMockDoc' md 1 "mobile"

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- mockDocTestRequestHelper ctx POST [param_auth se_bankid, param_ssn se_ssn_10]
      (docApiV2SigSetAuthenticationToView did slid) 200
    assertEqual "Authentication to view should be set" se_bankid (getAuthToView mockDocSE10)
    assertEqual "SE-10 Personal number should be set" se_ssn_10 (getPersonalNumber' mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- mockDocTestRequestHelper ctx POST [param_auth standard_auth]
      (docApiV2SigSetAuthenticationToView did slid) 200
    assertEqual "Authentication to view should be set" standard_auth (getAuthToView mockDocStandard1)
    assertEqual "SE-10 Personal number should STILL be set" se_ssn_10 (getPersonalNumber' mockDocStandard1)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- mockDocTestRequestHelper ctx POST [param_auth se_bankid, param_ssn se_ssn_12]
      (docApiV2SigSetAuthenticationToView did slid) 200
    assertEqual "Authentication to view should be set" se_bankid (getAuthToView mockDocSE12)
    assertEqual "SE-12 Personal number should be set" se_ssn_12 (getPersonalNumber' mockDocSE12)

  -- Invalid NO SSN
  _ <- jsonTestRequestHelper ctx POST [param_auth no_bankid]
    (docApiV2SigSetAuthenticationToView did slid) 409

  -- Valid NO BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNO <- mockDocTestRequestHelper ctx POST [param_auth no_bankid, param_ssn no_ssn]
      (docApiV2SigSetAuthenticationToView did slid) 200
    assertEqual "Authentication to view should be set" no_bankid (getAuthToView mockDocNO)
    assertEqual "NO Personal number should be set" no_ssn (getPersonalNumber' mockDocNO)

  -- Invalid NO Mobile
  _ <- jsonTestRequestHelper ctx POST [param_auth no_bankid, param_mobile "-1"]
    (docApiV2SigSetAuthenticationToView did slid) 409
  _ <- jsonTestRequestHelper ctx POST [param_auth no_bankid, param_mobile (init no_mobile)]
    (docApiV2SigSetAuthenticationToView did slid) 409
  _ <- jsonTestRequestHelper ctx POST [param_auth no_bankid, param_mobile (no_mobile ++ "5")]
    (docApiV2SigSetAuthenticationToView did slid) 409

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOMobile <- mockDocTestRequestHelper ctx POST [param_auth no_bankid, param_mobile no_mobile]
      (docApiV2SigSetAuthenticationToView did slid) 200
    assertEqual "Authentication to view should be set" no_bankid (getAuthToView mockDocNOMobile)
    assertEqual "NO Mobile number should be set" no_mobile (getMobileNumber' mockDocNOMobile)
    assertEqual "NO Personal number should STILL be set" no_ssn (getPersonalNumber' mockDocNOMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOEmptyMobile <- mockDocTestRequestHelper ctx POST [param_auth no_bankid, param_mobile ""]
      (docApiV2SigSetAuthenticationToView did slid) 200
    assertEqual "Authentication to view should be set" no_bankid (getAuthToView mockDocNOEmptyMobile)
    assertEqual "NO Mobile number should be empty" "" (getMobileNumber' mockDocNOEmptyMobile)
    assertEqual "NO Personal number should STILL be set" no_ssn (getPersonalNumber' mockDocNOEmptyMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- mockDocTestRequestHelper ctx POST [param_auth standard_auth]
      (docApiV2SigSetAuthenticationToView did slid) 200
    assertEqual "Authentication to view should be set" standard_auth (getAuthToView mockDocStandard2)
    assertEqual "NO Personal number should STILL be set" no_ssn (getPersonalNumber' mockDocStandard2)

testDocApiV2SigSetAuthenticationToSign :: TestEnv ()
testDocApiV2SigSetAuthenticationToSign = do
  (_,ctx,did,mockDoc) <- testDocApiV2Start'
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid = "se_bankid"
      sms_pin = "sms_pin"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10 = "1234567890"
      se_ssn_12 = "123456789012"
      se_ssn_invalid = "1234"
      param_mobile x = ("mobile_number", inText x)
      valid_mobile = "+4612345678"
      _invalid_mobile = "45678"

  -- Some invalid requests
  _ <- jsonTestRequestHelper ctx POST [param_auth "god_is_witness"]
    (docApiV2SigSetAuthenticationToSign did slid) 400
  _ <- jsonTestRequestHelper ctx POST [param_auth se_bankid, param_ssn se_ssn_invalid]
    (docApiV2SigSetAuthenticationToSign did slid) 409
  -- FIXME this works, but is it supposed to?
  --_ <- jsonTestRequestHelper ctx POST [param_auth sms_pin, param_mobile invalid_mobile]
  --  (docApiV2SigSetAuthenticationToSign did slid) 409

  let getAuthToSign md = getForSigNumberFromMockDoc mockSigLinkAuthMethodToSign md 1
      getPersonalNumber' md = getFieldValueOfTypeForSigNumberFromMockDoc' md 1 "personal_number"
      getMobileNumber' md = getFieldValueOfTypeForSigNumberFromMockDoc' md 1 "mobile"

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSEEmpty <- mockDocTestRequestHelper ctx POST [param_auth se_bankid]
      (docApiV2SigSetAuthenticationToSign did slid) 200
    assertEqual "Authentication to sign should be set" se_bankid (getAuthToSign mockDocSEEmpty)
    assertEqual "Personal number should not be set" "" (getPersonalNumber' mockDocSEEmpty)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- mockDocTestRequestHelper ctx POST [param_auth se_bankid, param_ssn se_ssn_10]
      (docApiV2SigSetAuthenticationToSign did slid) 200
    assertEqual "Authentication to sign should be set" se_bankid (getAuthToSign mockDocSE10)
    assertEqual "Personal number should be set (10 digit SE)" se_ssn_10 (getPersonalNumber' mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- mockDocTestRequestHelper ctx POST [param_auth se_bankid, param_ssn se_ssn_12]
      (docApiV2SigSetAuthenticationToSign did slid) 200
    assertEqual "Authentication to sign should be set" se_bankid (getAuthToSign mockDocSE12)
    assertEqual "Personal number should be set (12 digit SE)" se_ssn_12 (getPersonalNumber' mockDocSE12)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- mockDocTestRequestHelper ctx POST [param_auth standard_auth]
      (docApiV2SigSetAuthenticationToSign did slid) 200
    assertEqual "Authentication to sign should be set" standard_auth (getAuthToSign mockDocStandard1)
    assertEqual "Personal number should STILL be set (12 digit SE)" se_ssn_12 (getPersonalNumber' mockDocStandard1)

  -- Valid SMS PIN
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSMSEmpty <- mockDocTestRequestHelper ctx POST [param_auth sms_pin]
      (docApiV2SigSetAuthenticationToSign did slid) 200
    assertEqual "Authentication to sign should be set" sms_pin (getAuthToSign mockDocSMSEmpty)
    assertEqual "Mobile number should not be set" "" (getMobileNumber' mockDocSMSEmpty)
    assertEqual "Personal number should STILL be set (12 digit SE)" se_ssn_12 (getPersonalNumber' mockDocSMSEmpty)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSMS <- mockDocTestRequestHelper ctx POST [param_auth sms_pin, param_mobile valid_mobile]
      (docApiV2SigSetAuthenticationToSign did slid) 200
    assertEqual "Authentication to sign should be set" sms_pin (getAuthToSign mockDocSMS)
    assertEqual "Mobile number should be set" valid_mobile (getMobileNumber' mockDocSMS)
    assertEqual "Personal number should STILL be set (12 digit SE)" se_ssn_12 (getPersonalNumber' mockDocSMS)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- mockDocTestRequestHelper ctx POST [param_auth standard_auth]
      (docApiV2SigSetAuthenticationToSign did slid) 200
    assertEqual "Authentication to sign should be set" standard_auth (getAuthToSign mockDocStandard2)
    assertEqual "Mobile number should STILL be set" valid_mobile (getMobileNumber' mockDocStandard2)
    assertEqual "Personal number should STILL be set (12 digit SE)" se_ssn_12 (getPersonalNumber' mockDocStandard2)
