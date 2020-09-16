module Doc.API.V2.Calls.SignatoryCallsTest (apiV2SignatoryCallsTests) where

import Crypto.RNG
import Data.Text (pack)
import Data.Time.Clock
import Happstack.Server
import Test.Framework

import DB.Query
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
import Doc.SMSPin.Model
import Doc.Types.DocumentStatus (DocumentStatus(..))
import Doc.Types.SignatoryLink
import TestingUtil
import TestKontra
import User.Lang (defaultLang)

apiV2SignatoryCallsTests :: TestEnvSt -> Test
apiV2SignatoryCallsTests env = testGroup
  "APIv2SignatoryCalls"
  [ testThat "API v2 Signatory Reject"            env testDocApiV2SigReject
  , testThat "API v2 Signatory Check"             env testDocApiV2SigCheck
  , testThat "API v2 Signatory Sign"              env testDocApiV2SigSign
  , testThat "API v2 Signatory view with SMS PIN" env testDocApiV2SigViewWithSmsPin
  , testThat "API v2 Signatory view with SMS PIN is blocked after 3 attempts"
             env
             testDocApiV2SigViewWithSmsPinBlocked
  , testThat "API v2 Signatory check with SMS PIN" env testDocApiV2SigCheckWithSmsPin
  , testThat "API v2 Signatory check with SMS PIN is blocked after 3 attempts"
             env
             testDocApiV2SigCheckWithSmsPinBlocked
  , testThat "API v2 Signatory sign with SMS PIN" env testDocApiV2SigSignWithSmsPin
  , testThat "API v2 Signatory sign with SMS PIN is blocked after 3 attempts"
             env
             testDocApiV2SigSignWithSmsPinBlocked
  , testThat "API v2 Signatory view archived with SMS PIN"
             env
             testDocApiV2SigViewArchivedWithSmsPin
  , testThat "API v2 Signatory view archived with SMS PIN is blocked after 3 attempts"
             env
             testDocApiV2SigViewArchivedWithSmsPinBlocked
  , testThat "API v2 Signatory Send SMS PIN"   env testDocApiV2SigSendSmsPin
  , testThat "API v2 Signatory Set attachment" env testDocApiV2SigSetAttachment
  ]

testDocApiV2SigReject :: TestEnv ()
testDocApiV2SigReject = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
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
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
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

testDocApiV2SigSign :: TestEnv ()
testDocApiV2SigSign = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
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
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
  mockDoc <- testDocApiV2New' ctx
  let did  = getMockDocId mockDoc
  let slid = getMockDocSigLinkId 1 mockDoc

  let updateDoc =
        mockDocToInput
          . setMockDocSigLinkAuthToSignMethod 1 SMSPinAuthenticationToSign
          $ setMockDocSigLinkStandardField 1 "mobile" "+46123456789" mockDoc
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

testDocApiV2SigViewWithSmsPin :: TestEnv ()
testDocApiV2SigViewWithSmsPin = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user >>= addContextSession
  mockDoc <- testDocApiV2New' ctx
  let did    = getMockDocId mockDoc
  let slid   = getMockDocSigLinkId 1 mockDoc
  let mobile = "+46123456789"
  let updateDoc =
        setMockDocSigLinkAuthToViewMethod 1 SMSPinAuthenticationToView
          $ setMockDocSigLinkStandardField 1 "mobile" mobile mockDoc
  let generatePin = dbUpdate $ GetOrCreateSignatoryPin SMSPinToView slid (pack mobile)
  let identifyWithPin expectedRsCode pin = void $ testRequestHelper
        ctx
        POST
        [("sms_pin", inText pin)]
        (docApiV2SigIdentifyToViewWithSmsPin did slid)
        expectedRsCode

  void $ testDocApiV2Update' ctx updateDoc
  void $ testDocApiV2Start' ctx (getMockDocId mockDoc)

  replicateM_ 2
  -- Generating new PIN does not reset bad attempts counter
    $   generatePin
    >>= generateIncorrectPin
    >>= identifyWithPin 400

  -- Correct PIN is not blocked after just 2 bad attempts
  generatePin >>= identifyWithPin 202

  -- Attempts are reset after successful attempts.
  generatePin >>= generateIncorrectPin >>= identifyWithPin 400
  -- 3rd bad attempts doesn't block PIN in this case.
  generatePin >>= identifyWithPin 202

testDocApiV2SigViewWithSmsPinBlocked :: TestEnv ()
testDocApiV2SigViewWithSmsPinBlocked = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user >>= addContextSession
  mockDoc <- testDocApiV2New' ctx
  let did    = getMockDocId mockDoc
  let slid   = getMockDocSigLinkId 1 mockDoc
  let mobile = "+46123456789"
  let updateDoc =
        setMockDocSigLinkAuthToViewMethod 1 SMSPinAuthenticationToView
          $ setMockDocSigLinkStandardField 1 "mobile" mobile mockDoc
  let generatePin = dbUpdate $ GetOrCreateSignatoryPin SMSPinToView slid (pack mobile)
  let identifyWithPin expectedRsCode pin = void $ testRequestHelper
        ctx
        POST
        [("sms_pin", inText pin)]
        (docApiV2SigIdentifyToViewWithSmsPin did slid)
        expectedRsCode

  void $ testDocApiV2Update' ctx updateDoc
  void $ testDocApiV2Start' ctx (getMockDocId mockDoc)

  replicateM_ 3
    -- Generating new PIN does not reset bad attempts counter
    $   generatePin
    >>= generateIncorrectPin
    >>= identifyWithPin 400

  -- Correct PIN is blocked after 3 bad attempts
  generatePin >>= identifyWithPin 400

  modifyTestTime (addUTCTime 3599)
  -- Correct PIN is still blocked after 59 minutes and 59 seconds
  generatePin >>= identifyWithPin 400

  modifyTestTime (addUTCTime 3600)
  -- Correct PIN is unblocked after 1 hour
  generatePin >>= identifyWithPin 202

testDocApiV2SigCheckWithSmsPin :: TestEnv ()
testDocApiV2SigCheckWithSmsPin = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user >>= addContextSession
  mockDoc <- testDocApiV2New' ctx
  let did    = getMockDocId mockDoc
  let slid   = getMockDocSigLinkId 1 mockDoc
  let mobile = "+46123456789"
  let updateDoc =
        setMockDocSigLinkAuthToSignMethod 1 SMSPinAuthenticationToSign
          $ setMockDocSigLinkStandardField 1 "mobile" mobile mockDoc
  let generatePin = dbUpdate $ GetOrCreateSignatoryPin SMSPinToSign slid (pack mobile)
  let checkWithPin expectedRsCode pin = void $ testRequestHelper
        ctx
        POST
        [ ("sms_pin"          , inText pin)
        , ("fields"           , inText "[]")
        , ("accepted_author_attachments", inText "[]")
        , ("consent_responses", inText "[]")
        ]
        (docApiV2SigCheck did slid)
        expectedRsCode

  void $ testDocApiV2Update' ctx updateDoc
  void $ testDocApiV2Start' ctx (getMockDocId mockDoc)

  replicateM_ 2
  -- Generating new PIN does not reset bad attempts counter
    $   generatePin
    >>= generateIncorrectPin
    >>= checkWithPin 400

  -- Correct PIN is not blocked after just 2 bad attempts
  generatePin >>= checkWithPin 200

  -- Attempts are reset after successful attempts.
  generatePin >>= generateIncorrectPin >>= checkWithPin 400
  -- 3rd bad attempts doesn't block PIN in this case.
  generatePin >>= checkWithPin 200

testDocApiV2SigCheckWithSmsPinBlocked :: TestEnv ()
testDocApiV2SigCheckWithSmsPinBlocked = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user >>= addContextSession
  mockDoc <- testDocApiV2New' ctx
  let did    = getMockDocId mockDoc
  let slid   = getMockDocSigLinkId 1 mockDoc
  let mobile = "+46123456789"
  let updateDoc =
        setMockDocSigLinkAuthToSignMethod 1 SMSPinAuthenticationToSign
          $ setMockDocSigLinkStandardField 1 "mobile" mobile mockDoc
  let generatePin = dbUpdate $ GetOrCreateSignatoryPin SMSPinToSign slid (pack mobile)
  let checkWithPin expectedRsCode pin = void $ testRequestHelper
        ctx
        POST
        [ ("sms_pin"          , inText pin)
        , ("fields"           , inText "[]")
        , ("accepted_author_attachments", inText "[]")
        , ("consent_responses", inText "[]")
        ]
        (docApiV2SigCheck did slid)
        expectedRsCode

  void $ testDocApiV2Update' ctx updateDoc
  void $ testDocApiV2Start' ctx (getMockDocId mockDoc)

  replicateM_ 3
    -- Generating new PIN does not reset bad attempts counter
    $   generatePin
    >>= generateIncorrectPin
    >>= checkWithPin 400

  -- Correct PIN is blocked after 3 bad attempts
  generatePin >>= checkWithPin 400

  modifyTestTime (addUTCTime 3599)
  -- Correct PIN is still blocked after 59 minutes and 59 seconds
  generatePin >>= checkWithPin 400

  modifyTestTime (addUTCTime 3600)
  -- Correct PIN is unblocked after 1 hour
  generatePin >>= checkWithPin 200

testDocApiV2SigSignWithSmsPin :: TestEnv ()
testDocApiV2SigSignWithSmsPin = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user >>= addContextSession
  mockDoc <- testDocApiV2New' ctx
  let did    = getMockDocId mockDoc
  let slid   = getMockDocSigLinkId 1 mockDoc
  let mobile = "+46123456789"
  let updateDoc =
        setMockDocSigLinkAuthToSignMethod 1 SMSPinAuthenticationToSign
          $ setMockDocSigLinkStandardField 1 "mobile" mobile mockDoc
  let generatePin = dbUpdate $ GetOrCreateSignatoryPin SMSPinToSign slid (pack mobile)
  let signWithPin expectedRsCode pin = void $ testRequestHelper
        ctx
        POST
        [ ("sms_pin"          , inText pin)
        , ("fields"           , inText "[]")
        , ("accepted_author_attachments", inText "[]")
        , ("consent_responses", inText "[]")
        ]
        (docApiV2SigSign did slid)
        expectedRsCode

  void $ testDocApiV2Update' ctx updateDoc
  void $ testDocApiV2Start' ctx (getMockDocId mockDoc)

  replicateM_ 2
  -- Generating new PIN does not reset bad attempts counter
    $   generatePin
    >>= generateIncorrectPin
    >>= signWithPin 403

  -- Correct PIN is not blocked after just 2 bad attempts
  generatePin >>= signWithPin 200

  -- Testing PIN attempts reset doesn't make sense here
  -- because document will be signed after this.

testDocApiV2SigSignWithSmsPinBlocked :: TestEnv ()
testDocApiV2SigSignWithSmsPinBlocked = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user >>= addContextSession
  mockDoc <- testDocApiV2New' ctx
  let did    = getMockDocId mockDoc
  let slid   = getMockDocSigLinkId 1 mockDoc
  let mobile = "+46123456789"
  let updateDoc =
        setMockDocSigLinkAuthToSignMethod 1 SMSPinAuthenticationToSign
          $ setMockDocSigLinkStandardField 1 "mobile" mobile mockDoc
  let generatePin = dbUpdate $ GetOrCreateSignatoryPin SMSPinToSign slid (pack mobile)
  let signWithPin expectedRsCode pin = void $ testRequestHelper
        ctx
        POST
        [ ("sms_pin"          , inText pin)
        , ("fields"           , inText "[]")
        , ("accepted_author_attachments", inText "[]")
        , ("consent_responses", inText "[]")
        ]
        (docApiV2SigSign did slid)
        expectedRsCode

  void $ testDocApiV2Update' ctx updateDoc
  void $ testDocApiV2Start' ctx (getMockDocId mockDoc)

  replicateM_ 3
    -- Generating new PIN does not reset bad attempts counter
    $   generatePin
    >>= generateIncorrectPin
    >>= signWithPin 403

  -- Correct PIN is blocked after 3 bad attempts
  generatePin >>= signWithPin 403

  modifyTestTime (addUTCTime 3599)
  -- Correct PIN is still blocked after 59 minutes and 59 seconds
  generatePin >>= signWithPin 403

  modifyTestTime (addUTCTime 3600)
  -- Correct PIN is unblocked after 1 hour
  generatePin >>= signWithPin 200

testDocApiV2SigViewArchivedWithSmsPin :: TestEnv ()
testDocApiV2SigViewArchivedWithSmsPin = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user >>= addContextSession
  mockDoc <- testDocApiV2New' ctx
  let did    = getMockDocId mockDoc
  let slid   = getMockDocSigLinkId 1 mockDoc
  let mobile = "+46123456789"
  let updateDoc =
        setMockDocSigLinkAuthToViewArchivedMethod 1 SMSPinAuthenticationToView
          $ setMockDocSigLinkStandardField 1 "mobile" mobile mockDoc
  let generatePin =
        dbUpdate $ GetOrCreateSignatoryPin SMSPinToViewArchived slid (pack mobile)
  let identifyWithPin expectedRsCode pin = void $ testRequestHelper
        ctx
        POST
        [("sms_pin", inText pin)]
        (docApiV2SigIdentifyToViewWithSmsPin did slid)
        expectedRsCode

  void $ testDocApiV2Update' ctx updateDoc
  void $ testDocApiV2Start' ctx (getMockDocId mockDoc)

  -- Sign document to make it archived
  void $ testRequestHelper
    ctx
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did slid)
    200

  replicateM_ 2
  -- Generating new PIN does not reset bad attempts counter
    $   generatePin
    >>= generateIncorrectPin
    >>= identifyWithPin 400

  -- Correct PIN is not blocked after just 2 bad attempts
  generatePin >>= identifyWithPin 202

  -- Attempts are reset after successful attempts.
  generatePin >>= generateIncorrectPin >>= identifyWithPin 400
  -- 3rd bad attempts doesn't block PIN in this case.
  generatePin >>= identifyWithPin 202

testDocApiV2SigViewArchivedWithSmsPinBlocked :: TestEnv ()
testDocApiV2SigViewArchivedWithSmsPinBlocked = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user >>= addContextSession
  mockDoc <- testDocApiV2New' ctx
  let did    = getMockDocId mockDoc
  let slid   = getMockDocSigLinkId 1 mockDoc
  let mobile = "+46123456789"
  let updateDoc =
        setMockDocSigLinkAuthToViewArchivedMethod 1 SMSPinAuthenticationToView
          $ setMockDocSigLinkStandardField 1 "mobile" mobile mockDoc
  let generatePin =
        dbUpdate $ GetOrCreateSignatoryPin SMSPinToViewArchived slid (pack mobile)
  let identifyWithPin expectedRsCode pin = void $ testRequestHelper
        ctx
        POST
        [("sms_pin", inText pin)]
        (docApiV2SigIdentifyToViewWithSmsPin did slid)
        expectedRsCode

  void $ testDocApiV2Update' ctx updateDoc
  void $ testDocApiV2Start' ctx (getMockDocId mockDoc)

  -- Sign document to make it archived
  void $ testRequestHelper
    ctx
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did slid)
    200

  replicateM_ 3
    -- Generating new PIN does not reset bad attempts counter
    $   generatePin
    >>= generateIncorrectPin
    >>= identifyWithPin 400

  -- Correct PIN is blocked after 3 bad attempts
  generatePin >>= identifyWithPin 400

  modifyTestTime (addUTCTime 3599)
  -- Correct PIN is still blocked after 59 minutes and 59 seconds
  generatePin >>= identifyWithPin 400

  modifyTestTime (addUTCTime 3600)
  -- Correct PIN is unblocked after 1 hour
  generatePin >>= identifyWithPin 202

testDocApiV2SigSetAttachment :: TestEnv ()
testDocApiV2SigSetAttachment = do
  user    <- instantiateRandomUser
  ctx     <- mkContextWithUser defaultLang user
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


generateIncorrectPin :: CryptoRNG m => Text -> m Text
generateIncorrectPin pin = untilM (/= pin) (showt <$> randomR (1000 :: Int, 9999))
  where
    -- | Executes monadic action until result matches the predicate.
    untilM :: Monad m => (a -> Bool) -> m a -> m a
    untilM p m = m >>= \v -> if p v then pure v else untilM p m
