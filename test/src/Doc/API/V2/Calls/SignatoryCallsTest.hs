module Doc.API.V2.Calls.SignatoryCallsTest (apiV2SignatoryCallsTests) where

import Data.Default
import Happstack.Server
import Test.Framework

import Context
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.DocumentPostCallsTest (testDocApiV2New', testDocApiV2Start')
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.MockTestUtils
import Doc.API.V2.MockUnjson
import KontraPrelude
import TestingUtil
import TestKontra

apiV2SignatoryCallsTests :: TestEnvSt -> Test
apiV2SignatoryCallsTests env = testGroup "APIv2SignatoryCalls" $
  [ testThat "API v2 Signatory Reject"          env testDocApiV2SigReject
  , testThat "API v2 Signatory Check"           env testDocApiV2SigCheck
  , testThat "API v2 Signatory Sign"            env testDocApiV2SigSign
  , testThat "API v2 Signatory Send SMS PIN"    env testDocApiV2SigSendSmsPin
  , testThat "API v2 Signatory Set attachment"  env testDocApiV2SigSetAttachment
  ]

testDocApiV2SigReject :: TestEnv ()
testDocApiV2SigReject = do
  (_,ctx,did,mockDoc) <- testDocApiV2Start'
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  mockDocRejected <- mockDocTestRequestHelper ctx POST [] (docApiV2SigReject did slid) 200
  assertEqual "Document status after reject call should match" "rejected" (mockDocStatus mockDocRejected)
  assertJust $ getForSigNumberFromMockDoc mockSigLinkRejectedTime mockDocRejected 1

testDocApiV2SigCheck :: TestEnv ()
testDocApiV2SigCheck = do
  (_,ctx,did,mockDoc) <- testDocApiV2Start'
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  _ <- mockDocTestRequestHelper ctx
    POST [("fields", inText "[]"), ("accepted_author_attachments", inText "[]")]
    (docApiV2SigCheck did slid) 200
  return ()

testDocApiV2SigSign :: TestEnv ()
testDocApiV2SigSign = do
  (_,ctx,did,mockDoc) <- testDocApiV2Start'
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  mockDocSigned <- mockDocTestRequestHelper ctx
    POST [("fields", inText "[]"), ("accepted_author_attachments", inText "[]")]
    (docApiV2SigSign did slid) 200
  assertEqual "Document status after signing should match" "closed" (mockDocStatus mockDocSigned)
  assertJust $ getForSigNumberFromMockDoc mockSigLinkSignTime mockDocSigned 1

testDocApiV2SigSendSmsPin :: TestEnv ()
testDocApiV2SigSendSmsPin = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  mockDoc <- testDocApiV2New' ctx
  let did = documentIDFromMockDoc mockDoc
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  let updateDoc = mockDocToInput $ setForSigNumberFromMockDoc
        (\msl -> msl { mockSigLinkAuthMethodToSign = "sms_pin" }) mockDoc 1
  _update <- mockDocTestRequestHelper ctx POST [("document", updateDoc)] (docApiV2Update did) 200

  _start <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200

  _send_pin <- jsonTestRequestHelper ctx POST [("phone", inText "+46123456789")] (docApiV2SigSendSmsPin did slid) 202
  return ()

testDocApiV2SigSetAttachment :: TestEnv ()
testDocApiV2SigSetAttachment = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  mockDoc <- testDocApiV2New' ctx
  let did = documentIDFromMockDoc mockDoc
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  do -- Just to ensure limited scope so we don't test against the wrong thing
    let updateDoc = mockDocToInput $ setForSigNumberFromMockDoc
          (\msl -> msl { mockSigLinkAttachments = [
              MockSigAttachment { mockSigAttachmentName = "Foo2"
                                , mockSigAttachmentDescription = "FooDee"
                                , mockSigAttachmentFile = Nothing
                                }
                      ] }) mockDoc 1
    _update <- mockDocTestRequestHelper ctx POST [("document", updateDoc)] (docApiV2Update did) 200
    _start <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
    return ()

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSetAttachment <- mockDocTestRequestHelper ctx
      POST [("name", inText "Foo2") ,("attachment", inFile "test/pdfs/simple-rotate-90.pdf")]
      (docApiV2SigSetAttachment did slid) 200
    let sig_attachments = getForSigNumberFromMockDoc mockSigLinkAttachments mockDocSetAttachment 1
    assertEqual "Party should have one attachment" 1 (length sig_attachments)
    let att1:_ = sig_attachments
    assertJust (mockSigAttachmentFile att1)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocUnsetAttachment <- mockDocTestRequestHelper ctx POST [("name", inText "Foo2")] (docApiV2SigSetAttachment did slid) 200
    let sig_attachments_unset = getForSigNumberFromMockDoc mockSigLinkAttachments mockDocUnsetAttachment 1
    assertEqual "Party should still have one attachment" 1 (length sig_attachments_unset)
    let att1_unset:_ = sig_attachments_unset
    assertNothing (mockSigAttachmentFile att1_unset)
