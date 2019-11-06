module Doc.API.V2.Calls.SignatoryCallsTest (apiV2SignatoryCallsTests) where

import Happstack.Server
import Test.Framework

import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Types.DocumentStatus (DocumentStatus(..))
import Doc.Types.SignatoryLink
  ( AuthenticationToSignMethod(SMSPinAuthenticationToSign) )

import TestingUtil
import TestKontra
import User.Lang (defaultLang)

apiV2SignatoryCallsTests :: TestEnvSt -> Test
apiV2SignatoryCallsTests env =
  testGroup "APIv2SignatoryCalls"
    $ [ testThat "API v2 Signatory Reject"         env testDocApiV2SigReject
      , testThat "API v2 Signatory Check"          env testDocApiV2SigCheck
      , testThat "API v2 Signatory Sign"           env testDocApiV2SigSign
      , testThat "API v2 Signatory Send SMS PIN"   env testDocApiV2SigSendSmsPin
      , testThat "API v2 Signatory Set attachment" env testDocApiV2SigSetAttachment
      ]

testDocApiV2SigReject :: TestEnv ()
testDocApiV2SigReject = do
  user    <- addNewRandomUser
  ctx     <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  let did  = getMockDocId mockDoc
  let slid = getMockDocSigLinkId 1 mockDoc

  mockDocRejected <- mockDocTestRequestHelper ctx POST [] (docApiV2SigReject did slid) 200
  assertEqual "Document status after reject call should match"
              Rejected
              (getMockDocStatus mockDocRejected)
  assertBool "Signatory should have rejected the document"
             (getMockDocSigLinkHasRejected 1 mockDocRejected)

testDocApiV2SigCheck :: TestEnv ()
testDocApiV2SigCheck = do
  user    <- addNewRandomUser
  ctx     <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  let did  = getMockDocId mockDoc
  let slid = getMockDocSigLinkId 1 mockDoc

  void $ testRequestHelper
    ctx
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigCheck did slid)
    200
  return ()

testDocApiV2SigSign :: TestEnv ()
testDocApiV2SigSign = do
  user    <- addNewRandomUser
  ctx     <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  let did  = getMockDocId mockDoc
  let slid = getMockDocSigLinkId 1 mockDoc

  mockDocSigned <- mockDocTestRequestHelper
    ctx
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did slid)
    200
  assertEqual "Document status after signing should match"
              Closed
              (getMockDocStatus mockDocSigned)
  assertBool "Signatory should have signed" (getMockDocSigLinkHasSigned 1 mockDocSigned)

testDocApiV2SigSendSmsPin :: TestEnv ()
testDocApiV2SigSendSmsPin = do
  user    <- addNewRandomUser
  ctx     <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  mockDoc <- testDocApiV2New' ctx
  let did  = getMockDocId mockDoc
  let slid = getMockDocSigLinkId 1 mockDoc

  let updateDoc =
        mockDocToInput
          $ setMockDocSigLinkAuthToSignMethod 1 SMSPinAuthenticationToSign
          $ setMockDocSigLinkStandardField 1 "mobile" "+46123456789"
          $ mockDoc
  _update <- mockDocTestRequestHelper ctx
                                      POST
                                      [("document", updateDoc)]
                                      (docApiV2Update did)
                                      200

  _start    <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200

  _send_pin <- testRequestHelper ctx
                                 POST
                                 [("mobile", inText "+46123456789")]
                                 (docApiV2SigSendSmsPinToSign did slid)
                                 202
  return ()

testDocApiV2SigSetAttachment :: TestEnv ()
testDocApiV2SigSetAttachment = do
  user    <- addNewRandomUser
  ctx     <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  mockDoc <- testDocApiV2New' ctx
  let did  = getMockDocId mockDoc
  let slid = getMockDocSigLinkId 1 mockDoc

  do -- Just to ensure limited scope so we don't test against the wrong thing
    let updateDoc =
          mockDocToInput $ setMockDocSigLinkAttachments 1 [("Foo2", "FooDee")] mockDoc
    _update <- mockDocTestRequestHelper ctx
                                        POST
                                        [("document", updateDoc)]
                                        (docApiV2Update did)
                                        200
    _start <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
    return ()

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSetAttachment <- mockDocTestRequestHelper
      ctx
      POST
      [ ("name"      , inText "Foo2")
      , ("attachment", inFile $ inTestDir "pdfs/simple-rotate-90.pdf")
      ]
      (docApiV2SigSetAttachment did slid)
      200
    assertEqual "Party should have one attachment"
                1
                (getMockDocSigLinkAttachmentsLength 1 mockDocSetAttachment)
    assertBool "The attachment should have a file set"
               (getMockDocSigLinkAttachmentHasFile 1 1 mockDocSetAttachment)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocUnsetAttachment <- mockDocTestRequestHelper
      ctx
      POST
      [("name", inText "Foo2")]
      (docApiV2SigSetAttachment did slid)
      200
    assertEqual "Party should still have one attachment"
                1
                (getMockDocSigLinkAttachmentsLength 1 mockDocUnsetAttachment)
    assertBool "The attachment should have no file"
               (not $ getMockDocSigLinkAttachmentHasFile 1 1 mockDocUnsetAttachment)
