module Doc.API.V2.Calls.DocumentPostCallsTest
  ( apiV2DocumentPostCallsTests
  , testDocApiV2New'
  ) where

import Data.Aeson
import Data.Default
import Happstack.Server
import Test.Framework
import qualified Data.Vector as V

import Context
import DB.Query (dbUpdate)
import Doc.API.AesonTestUtils
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.SignatoryCalls (docApiV2SigSign)
import Doc.DocumentID (DocumentID)
import Doc.DocumentMonad (withDocumentID)
import Doc.Model.Update (TimeoutDocument(..))
import KontraPrelude
import TestingUtil
import TestKontra
import Util.Actor (userActor)

apiV2DocumentPostCallsTests :: TestEnvSt -> Test
apiV2DocumentPostCallsTests env = testGroup "APIv2DocumentPostCalls" $
  [ testThat "API v2 New"                                   env testDocApiV2New
  , testThat "API v2 New from template"                     env testDocApiV2NewFromTemplate
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
  , testThat "API v2 Set signatory authentication to-sign"  env testDocApiV2SigSetAuthenticationToSign
  ]

testDocApiV2New :: TestEnv ()
testDocApiV2New = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  _ <- testDocApiV2New' ctx
  return ()

testDocApiV2New' :: Context -> TestEnv (DocumentID, Value)
testDocApiV2New' ctx = do
  req <- mkRequest POST [("file", inFile "test/pdfs/simple.pdf")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2New
  assertEqual "Successful `docApiV2New` response code" 201 (rsCode rsp)
  docJSON <- valueFromBS (rsBody rsp)
  did <- documentIDFromValue docJSON
  return (did, docJSON)

testDocApiV2NewFromTemplate :: TestEnv ()
testDocApiV2NewFromTemplate = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqUpdate <- mkRequest POST [("document", inText "{\"is_template\":true}")]
  _ <- runTestKontra reqUpdate ctx $ docApiV2Update did

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2NewFromTemplate did
  assertEqual "Successful `docApiV2NewFromTemplate` response code" 201 (rsCode rsp)
  _ <- parseMockDocumentFromBS did (rsBody rsp)
  return ()

testDocApiV2Update :: TestEnv ()
testDocApiV2Update = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  req <- mkRequest POST [("document", inText "{\"title\":\"testTitle\"}")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2Update did
  assertEqual "Successful `docApiV2Update` response code" 200 (rsCode rsp)
  docJSON <- parseMockDocumentFromBS did (rsBody rsp)
  docTitle <- lookupObjectString "title" docJSON
  assertEqual "Updated 'title' should match" "testTitle" docTitle

testDocApiV2Start :: TestEnv ()
testDocApiV2Start = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2Start did
  assertEqual "Successful `docApiV2Start` response code" 200 (rsCode rsp)
  docJSON <- parseMockDocumentFromBS did (rsBody rsp)
  docStatus <- lookupObjectString "status" docJSON
  assertEqual "Document status should match" "pending" docStatus

testDocApiV2Prolong :: TestEnv ()
testDocApiV2Prolong = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  withDocumentID did $ do
    dbUpdate $ TimeoutDocument (userActor ctx user)

  req <- mkRequest POST [("days", inText "1")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2Prolong did
  assertEqual "Successful `docApiV2Prolong` response code" 200 (rsCode rsp)
  docJSON <- parseMockDocumentFromBS did (rsBody rsp)
  docStatus <- lookupObjectString "status" docJSON
  assertEqual "Document status should match" "pending" docStatus

testDocApiV2Cancel :: TestEnv ()
testDocApiV2Cancel = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2Cancel did
  assertEqual "Successful `docApiV2Cancel` response code" 200 (rsCode rsp)
  docJSON <- parseMockDocumentFromBS did (rsBody rsp)
  docStatus <- lookupObjectString "status" docJSON
  assertEqual "Document status should match" "canceled" docStatus

testDocApiV2Trash :: TestEnv ()
testDocApiV2Trash = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2Trash did
  assertEqual "Successful `docApiV2Trash` response code" 200 (rsCode rsp)
  docJSON <- parseMockDocumentFromBS did (rsBody rsp)
  isTrashed <- lookupObjectBool "is_trashed" docJSON
  assertEqual "Value of 'is_trashed' should match" True isTrashed

testDocApiV2Delete :: TestEnv ()
testDocApiV2Delete = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqTrash <- mkRequest POST []
  _ <- runTestKontra reqTrash ctx $ docApiV2Trash did

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2Delete did
  assertEqual "Successful `docApiV2Delete` response code" 200 (rsCode rsp)
  docJSON <- parseMockDocumentFromBS did (rsBody rsp)
  isTrashed <- lookupObjectBool "is_trashed" docJSON
  assertEqual "Value of 'is_trashed' should match" True isTrashed
  isDeleted <- lookupObjectBool "is_deleted" docJSON
  assertEqual "Value of 'is_deleted' should match" True isDeleted

testDocApiV2Remind :: TestEnv ()
testDocApiV2Remind = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2Remind did
  assertEqual "Successful `docApiV2Remind` response code" 202 (rsCode rsp)

testDocApiV2Forward :: TestEnv ()
testDocApiV2Forward = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, docJSON) <- testDocApiV2New' ctx
  slid:_ <- signatoryLinkIDsFromValue docJSON

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  reqSign <- mkRequest POST [("fields", inText "[]")]
  _ <- runTestKontra reqSign ctx $ docApiV2SigSign did slid

  req <- mkRequest POST [("email", inText "2.a2@22.e.aa")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2Forward did
  assertEqual "Successful `docApiV2Forward` response code" 202 (rsCode rsp)

testDocApiV2SetFile :: TestEnv ()
testDocApiV2SetFile = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqNoFile <- mkRequest POST []
  (rspNoFile,_) <- runTestKontra reqNoFile ctx $ docApiV2SetFile did
  assertEqual "Successful `docApiV2SetFile` response code" 200 (rsCode rspNoFile)
  docJSONNoFile <- parseMockDocumentFromBS did (rsBody rspNoFile)
  lookupObjectNull "file" docJSONNoFile

  reqSetFile <- mkRequest POST [("file", inFile "test/pdfs/simple-rotate-180.pdf")]
  (rspSetFile,_) <- runTestKontra reqSetFile ctx $ docApiV2SetFile did
  assertEqual "Successful `docApiV2SetFile` response code" 200 (rsCode rspSetFile)
  docJSONSetFile <- parseMockDocumentFromBS did (rsBody rspSetFile)
  _ <- lookupObjectObjectValue "file" docJSONSetFile
  return ()

testDocApiV2SetAttachments :: TestEnv ()
testDocApiV2SetAttachments = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqSet <- mkRequest POST [("attachment_0", inFile "test/pdfs/simple-rotate-90.pdf")
                           ,("attachment_1", inFile "test/pdfs/simple-rotate-180.pdf")]
  (rspSet,_) <- runTestKontra reqSet ctx $ docApiV2SetAttachments did
  assertEqual "Successful `docApiV2SetAttachments` response code" 200 (rsCode rspSet)
  docJSONSet <- parseMockDocumentFromBS did (rsBody rspSet)
  attachmentsArraySet <- lookupObjectArray "author_attachments" docJSONSet
  assertEqual "Number of 'author_attachments' should be equal" 2 (V.length attachmentsArraySet)

  reqUnSet <- mkRequest POST []
  (rspUnSet,_) <- runTestKontra reqUnSet ctx $ docApiV2SetAttachments did
  assertEqual "Successful `docApiV2SetAttachments` response code" 200 (rsCode rspUnSet)
  docJSONUnSet <- parseMockDocumentFromBS did (rsBody rspUnSet)
  attachmentsArrayUnSet <- lookupObjectArray "author_attachments" docJSONUnSet
  assertEqual "Number of 'author_attachments' should be equal" 0 (V.length attachmentsArrayUnSet)

testDocApiV2SetAutoReminder :: TestEnv ()
testDocApiV2SetAutoReminder = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  req <- mkRequest POST [("days", inText "89")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2SetAutoReminder did
  assertEqual "Successful `docApiV2SetAutoReminder` response code" 200 (rsCode rsp)
  _docJSON <- parseMockDocumentFromBS did (rsBody rsp)
  -- FIXME setting autoReminder doesn't update immediately, bug in core?
  -- _ <- lookupObjectString "auto_remind_time" docJSON
  return ()

testDocApiV2Clone :: TestEnv ()
testDocApiV2Clone = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2Clone did
  assertEqual "Successful `docApiV2Clone` response code" 201 (rsCode rsp)
  _ <- parseMockDocumentFromBS did (rsBody rsp)
  return ()

testDocApiV2Restart :: TestEnv ()
testDocApiV2Restart = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  reqCancel <- mkRequest POST []
  _ <- runTestKontra reqCancel ctx $ docApiV2Cancel did

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2Restart did
  assertEqual "Successful `docApiV2Restart` response code" 201 (rsCode rsp)
  _ <- parseMockDocumentFromBS did (rsBody rsp)
  return ()

testDocApiV2SigSetAuthenticationToSign :: TestEnv ()
testDocApiV2SigSetAuthenticationToSign = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, docJSON) <- testDocApiV2New' ctx
  slid:_ <- signatoryLinkIDsFromValue docJSON

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  req <- mkRequest POST [("authentication_type", inText "sms_pin")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2SigSetAuthenticationToSign did slid
  assertEqual "Successful `docApiV2SigSetAuthenticationToSign` response code" 200 (rsCode rsp)
