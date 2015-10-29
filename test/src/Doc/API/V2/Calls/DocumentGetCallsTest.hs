module Doc.API.V2.Calls.DocumentGetCallsTest (apiV2DocumentGetCallsTests) where

import Data.Aeson (Value(String))
import Data.Default
import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Vector as V

import Context
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.DocumentGetCalls
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.DocumentPostCallsTest (testDocApiV2New')
import Doc.API.V2.Calls.SignatoryCalls (docApiV2SigSign)
import KontraPrelude
import TestingUtil
import TestKontra

apiV2DocumentGetCallsTests :: TestEnvSt -> Test
apiV2DocumentGetCallsTests env = testGroup "APIv2DocumentGetCalls" $
  [ testThat "API v2 Available"             env testDocApiV2Available
  , testThat "API v2 List"                  env testDocApiV2List
  , testThat "API v2 Get"                   env testDocApiV2Get
  , testThat "API v2 History"               env testDocApiV2History
  , testThat "API v2 Evidence attachments"  env testDocApiV2EvidenceAttachments
  , testThat "API v2 Files - Main"          env testDocApiV2FilesMain
  , testThat "API v2 Files - Get"           env testDocApiV2FilesGet
  , testThat "API v2 Texts"                 env testDocApiV2Texts
  ]

testDocApiV2Available :: TestEnv ()
testDocApiV2Available = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def

  (did1,_) <- testDocApiV2New' ctx
  (did2,_) <- testDocApiV2New' ctx
  (did3,_) <- testDocApiV2New' ctx

  let dids = "[0,"
              ++ show did1 ++ ","
              ++ show did2 ++ ","
              ++ show did3 ++ "]"
  req <- mkRequest GET [("ids", inText dids)]
  (rsp,_) <- runTestKontra req ctx $ docApiV2Available
  assertEqual "Successful `docApiV2Available` response code" 200 (rsCode rsp)
  let expectedDids = "[" ++ show did1 ++ ","
                         ++ show did2 ++ ","
                         ++ show did3 ++ "]"
  assertEqual "Expected response for `docApiV2Available`" (BSLC.pack expectedDids) (rsBody rsp)

testDocApiV2List :: TestEnv ()
testDocApiV2List = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def

  _ <- testDocApiV2New' ctx
  _ <- testDocApiV2New' ctx
  _ <- testDocApiV2New' ctx

  req <- mkRequest GET []
  (rsp,_) <- runTestKontra req ctx $ docApiV2List
  assertEqual "Successful `docApiV2List` response code" 200 (rsCode rsp)
  listJSON <- valueFromBS (rsBody rsp)
  listArray <- lookupObjectArray "documents" listJSON
  assertEqual "`docApiV2List` should return same number of docs" 3 (V.length listArray)

testDocApiV2Get :: TestEnv ()
testDocApiV2Get = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, newJSON) <- testDocApiV2New' ctx

  reqGet <- mkRequest GET []
  (rspGet,_) <- runTestKontra reqGet ctx $ docApiV2Get did
  getJSON <- parseMockDocumentFromBS did (rsBody rspGet)
  assertEqual "JSON from `docApiV2Get` should match from `docApiV2New`" getJSON newJSON

testDocApiV2History :: TestEnv ()
testDocApiV2History = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  let checkHistoryHasNItems :: Int -> TestEnv ()
      checkHistoryHasNItems n = do
        req <- mkRequest GET []
        (rsp,_) <- runTestKontra req ctx $ docApiV2History did
        historyJSON <- valueFromBS (rsBody rsp)
        historyArray <- lookupObjectArray "events" historyJSON
        when (V.length historyArray /= n)
          ($unexpectedErrorM $ "History did not have " ++ show n ++ " items")

  checkHistoryHasNItems 0

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  checkHistoryHasNItems 1

  reqSign <- mkRequest POST []
  _ <- runTestKontra reqSign ctx $ docApiV2Cancel did

  checkHistoryHasNItems 2

testDocApiV2EvidenceAttachments :: TestEnv ()
testDocApiV2EvidenceAttachments = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did, docJSON) <- testDocApiV2New' ctx
  slid:_ <- signatoryLinkIDsFromValue docJSON

  reqStart <- mkRequest POST []
  _ <- runTestKontra reqStart ctx $ docApiV2Start did

  reqSign <- mkRequest POST [("fields", inText "[]")]
  _ <- runTestKontra reqSign ctx $ docApiV2SigSign did slid

  req <- mkRequest GET []
  (rsp,_) <- runTestKontra req ctx $ docApiV2EvidenceAttachments did
  assertEqual "Successful `docApiV2EvidenceAttachments` response code" 200 (rsCode rsp)
  eaJSON <- valueFromBS (rsBody rsp)
  eaList <- V.toList <$> lookupObjectArray "attachments" eaJSON
  attachmentNames <- forM eaList (lookupObjectString "name")
  assertEqual "Evidence attachment file names and order should match"
    [ "Evidence Quality of Scrive E-signed Documents.html"
    , "Appendix 1 Evidence Quality Framework.html"
    , "Appendix 2 Service Description.html"
    , "Appendix 3 Evidence Log.html"
    , "Appendix 4 Evidence of Time.html"
    , "Appendix 5 Evidence of Intent.html"
    , "Appendix 6 Digital Signature Documentation.html"
    ]
    attachmentNames

testDocApiV2FilesMain :: TestEnv ()
testDocApiV2FilesMain = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  req <- mkRequest GET []
  (rsp,_) <- runTestKontra req ctx $ docApiV2FilesMain did "filename.pdf"
  assertEqual "Successful `docApiV2FilesMain` response code" 200 (rsCode rsp)

testDocApiV2FilesGet :: TestEnv ()
testDocApiV2FilesGet = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqSet <- mkRequest POST [("attachment_0", inFile "test/pdfs/simple-rotate-90.pdf")]
  (rspSet,_) <- runTestKontra reqSet ctx $ docApiV2SetAttachments did
  docJSONSet <- parseMockDocumentFromBS did (rsBody rspSet)
  firstAttachmentValue <- liftM V.head $ lookupObjectArray "author_attachments" docJSONSet
  attachmentFid <- fileIDFromFileValue firstAttachmentValue

  req <- mkRequest GET []
  (rsp,_) <- runTestKontra req ctx $ docApiV2FilesGet did attachmentFid "somefile.pdf"
  assertEqual "Successful `docApiV2FilesGet` response code" 200 (rsCode rsp)

testDocApiV2Texts :: TestEnv ()
testDocApiV2Texts = do
  (Just user) <- addNewUser "N1" "N2" "n1n2@domain.tld"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  (did,_) <- testDocApiV2New' ctx

  reqSetFile <- mkRequest POST [("file", inFile "test/pdfs/simple.pdf")]
  (rspSetFile,_) <- runTestKontra reqSetFile ctx $ docApiV2SetFile did
  docJSONSetFile <- parseMockDocumentFromBS did (rsBody rspSetFile)
  fileJSON <- lookupObjectObjectValue "file" docJSONSetFile
  fid <- fileIDFromFileValue fileJSON

  req <- mkRequest GET [("json", inText "{ \"rects\":[{ \"rect\": [0,0,1,1] , \"page\": 1 }]}")]
  (rsp,_) <- runTestKontra req ctx $ docApiV2Texts did fid
  assertEqual "Successful `docApiV2Texts` response code" 200 (rsCode rsp)
  rspJSON <- valueFromBS (rsBody rsp)
  rectsArrayFirstValue <- liftM V.head $ lookupObjectArray "rects" rspJSON
  linesValueList <- liftM V.toList $ lookupObjectArray "lines" rectsArrayFirstValue
  let toText (String s) = s
      toText _ = $unexpectedError "Should have been 'String' constructor"
      line:[] = map toText linesValueList
  assertEqual "Lines in `docApiV2Texts` should match" "This is a test contract pdf." line
