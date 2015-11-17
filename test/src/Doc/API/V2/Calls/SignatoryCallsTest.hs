module Doc.API.V2.Calls.SignatoryCallsTest (apiV2SignatoryCallsTests) where

import Data.Default
import Happstack.Server
import Test.Framework
import qualified Data.Vector as V

import Context
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.DocumentPostCallsTest (testDocApiV2New')
import Doc.API.V2.Calls.SignatoryCalls
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
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, docJSON) <- testDocApiV2New' ctx
  slid:_ <- signatoryLinkIDsFromValue docJSON

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  req <- mkRequest POST []
  (rsp,_) <- runTestKontra req ctx $ docApiV2SigReject did slid
  assertEqual "Successful `docApiV2SigReject` response code" 200 (rsCode rsp)
  docJSONRejected <- parseMockDocumentFromBS did (rsBody rsp)
  docStatus <- lookupObjectString "status" docJSONRejected
  assertEqual "Document status should match" "rejected" docStatus
  sigValue:_ <- liftM V.toList $ lookupObjectArray "parties" docJSONRejected
  _ <- lookupObjectString "rejected_time" sigValue
  return ()

testDocApiV2SigCheck :: TestEnv ()
testDocApiV2SigCheck = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, docJSON) <- testDocApiV2New' ctx
  slid:_ <- signatoryLinkIDsFromValue docJSON

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  req <- mkRequest POST [("fields", inText "[]"), ("accepted_author_attachments", inText "[]")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2SigCheck did slid
  assertEqual "Successful `docApiV2SigCheck` response code" 200 (rsCode rsp)

testDocApiV2SigSign :: TestEnv ()
testDocApiV2SigSign = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, docJSON) <- testDocApiV2New' ctx
  slid:_ <- signatoryLinkIDsFromValue docJSON

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  req <- mkRequest POST [("fields", inText "[]"), ("accepted_author_attachments", inText "[]")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2SigSign did slid
  assertEqual "Successful `docApiV2SigSign` response code" 200 (rsCode rsp)
  docJSONSigned <- parseMockDocumentFromBS did (rsBody rsp)
  docStatus <- lookupObjectString "status" docJSONSigned
  assertEqual "Document status after signing" "closed" docStatus
  sl:_ <- liftM V.toList $ lookupObjectArray "parties" docJSONSigned
  _ <- lookupObjectString "sign_time" sl
  return ()

testDocApiV2SigSendSmsPin :: TestEnv ()
testDocApiV2SigSendSmsPin = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, docJSON) <- testDocApiV2New' ctx
  slid:_ <- signatoryLinkIDsFromValue docJSON

  reqUpdate <- mkRequest POST [("document", inText
                          ("{\"parties\": ["
                          ++ "{\"id\":\"" ++ show slid ++ "\","
                          ++ " \"authentication_method_to_sign\":\"sms_pin\""
                          ++ "}]}")
                        )]
  _ <- runTestKontra reqUpdate ctx $ docApiV2Update did

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  req <- mkRequest POST [("phone", inText "+46123456789")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2SigSendSmsPin did slid
  assertEqual "Successful `docApiV2SigSendSmsPin` response code" 202 (rsCode rsp)

testDocApiV2SigSetAttachment :: TestEnv ()
testDocApiV2SigSetAttachment = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, docJSON) <- testDocApiV2New' ctx
  slid:_ <- signatoryLinkIDsFromValue docJSON

  reqUpdate <- mkRequest POST [("document", inText
                          ("{\"parties\": ["
                          ++ "{\"id\":\"" ++ show slid ++ "\","
                          ++ " \"attachments\": ["
                          ++ "    {\"name\":\"Foo2\", \"description\":\"FooDee\"}"
                          ++ "]}]}")
                        )]
  _ <- runTestKontra reqUpdate ctx $ docApiV2Update did

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  reqSet <- mkRequest POST [("name", inText "Foo2")
                           ,("attachment", inFile "test/pdfs/simple-rotate-90.pdf")]
  (rspSet,_) <- runTestKontra reqSet ctx $ docApiV2SigSetAttachment did slid
  assertEqual "Successful `docApiV2SigSetAttachment` response code" 200 (rsCode rspSet)
  docJSONSet <- parseMockDocumentFromBS did (rsBody rspSet)
  slSet:_ <- liftM V.toList $ lookupObjectArray "parties" docJSONSet
  slAttachment0Set:_ <- liftM V.toList $ lookupObjectArray "attachments" slSet
  _ <- lookupObjectString "file" slAttachment0Set

  reqUnset <- mkRequest POST [("name", inText "Foo2")]
  (rspUnset,_) <- runTestKontra reqUnset ctx $ docApiV2SigSetAttachment did slid
  assertEqual "Successful `docApiV2SigSetAttachment` response code" 200 (rsCode rspUnset)
  docJSONUnset <- parseMockDocumentFromBS did (rsBody rspUnset)
  slUnset:_ <- liftM V.toList $ lookupObjectArray "parties" docJSONUnset
  slAttachment0Unset:_ <- liftM V.toList $ lookupObjectArray "attachments" slUnset
  lookupObjectNull "file" slAttachment0Unset
