module Doc.API.V2.Calls.DocumentGetCallsTest (apiV2DocumentGetCallsTests) where

import Data.Aeson (Value(String))
import Data.Default
import Happstack.Server
import Test.Framework
import qualified Data.Vector as V

import Company.Model
import Context
import DB.Query (dbUpdate)
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.DocumentGetCalls
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.DocumentPostCallsTest (testDocApiV2New', testDocApiV2Start')
import Doc.API.V2.Calls.SignatoryCalls (docApiV2SigSign)
import Doc.API.V2.MockTestUtils
import Doc.API.V2.MockUnjson
import Doc.DocumentMonad (withDocumentID)
import Doc.Model.Update (SetDocumentSharing(..))
import KontraPrelude
import TestingUtil
import TestKontra

apiV2DocumentGetCallsTests :: TestEnvSt -> Test
apiV2DocumentGetCallsTests env = testGroup "APIv2DocumentGetCalls" $
  [ testThat "API v2 List"                  env testDocApiV2List
  , testThat "API v2 Get"                   env testDocApiV2Get
  , testThat "API v2 Get by Company Admin"  env testDocApiV2GetByAdmin
  , testThat "API v2 Get for Shared doc"    env testDocApiV2GetShared
  , testThat "API v2 History"               env testDocApiV2History
  , testThat "API v2 Evidence attachments"  env testDocApiV2EvidenceAttachments
  , testThat "API v2 Files - Main"          env testDocApiV2FilesMain
  , testThat "API v2 Files - Get"           env testDocApiV2FilesGet
  , testThat "API v2 Texts"                 env testDocApiV2Texts
  ]

testDocApiV2List :: TestEnv ()
testDocApiV2List = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def

  _ <- testDocApiV2New' ctx
  _ <- testDocApiV2New' ctx
  _ <- testDocApiV2New' ctx

  listJSON <- jsonTestRequestHelper ctx GET [] docApiV2List 200
  listArray <- lookupObjectArray "documents" listJSON
  assertEqual "`docApiV2List` should return same number of docs" 3 (V.length listArray)
  let docs = map mockDocFromValue $ V.toList listArray
  forM_ docs $ \d -> assertEqual "Status should be" "preparation" (mockDocStatus d)

testDocApiV2Get :: TestEnv ()
testDocApiV2Get = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  newMockDoc <- testDocApiV2New' ctx
  let did = documentIDFromMockDoc newMockDoc

  getMockDoc <- mockDocTestRequestHelper ctx GET [] (docApiV2Get did) 200
  assertEqual "Mock Document from `docApiV2Get` should match from `docApiV2New`" getMockDoc newMockDoc
  assertEqual "Document viewer should be" "signatory" (mockDocViewerRole . mockDocViewer $ getMockDoc)

testDocApiV2GetByAdmin :: TestEnv ()
testDocApiV2GetByAdmin = do
  (Company {companyid}) <- addNewCompany
  author <- addNewRandomCompanyUser companyid False
  ctxauthor <- (\c -> c { ctxmaybeuser = Just author }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctxauthor

  admin <- addNewRandomCompanyUser companyid True
  ctx <- (\c -> c { ctxmaybeuser = Just admin }) <$> mkContext def
  getMockDoc <- mockDocTestRequestHelper ctx GET [] (docApiV2Get did) 200
  assertEqual "Document viewer should be" "company_admin" (mockDocViewerRole . mockDocViewer $ getMockDoc)

testDocApiV2GetShared :: TestEnv ()
testDocApiV2GetShared = do
  (Company {companyid}) <- addNewCompany
  author <- addNewRandomCompanyUser companyid False
  ctxauthor <- (\c -> c { ctxmaybeuser = Just author }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctxauthor

  _ <- mockDocTestRequestHelper ctxauthor POST [("document", inText "{\"is_template\":true}")] (docApiV2Update did) 200
  setshare <- withDocumentID did $ do
    dbUpdate $ SetDocumentSharing [did] True
  assert setshare

  user <- addNewRandomCompanyUser companyid False
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  getMockDoc <- mockDocTestRequestHelper ctx GET [] (docApiV2Get did) 200
  assertEqual "Document viewer should be" "company_shared" (mockDocViewerRole . mockDocViewer $ getMockDoc)
  assertEqual "Document should be template" True (mockDocIsTemplate getMockDoc)
  assertEqual "Document should be shared" True (mockDocIsShared getMockDoc)

testDocApiV2History :: TestEnv ()
testDocApiV2History = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  let checkHistoryHasNItems :: Int -> TestEnv ()
      checkHistoryHasNItems n = do
        historyJSON <- jsonTestRequestHelper ctx GET [] (docApiV2History did) 200
        historyArray <- lookupObjectArray "events" historyJSON
        assertEqual "History did not have same number of events" n (V.length historyArray)

  checkHistoryHasNItems 0

  _ <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
  checkHistoryHasNItems 1

  _ <- mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200
  checkHistoryHasNItems 2

testDocApiV2EvidenceAttachments :: TestEnv ()
testDocApiV2EvidenceAttachments = do
  (_, ctx, did, mockDoc) <- testDocApiV2Start'
  let slid = signatoryLinkIDFromMockDoc 1 mockDoc

  _ <- mockDocTestRequestHelper ctx
    POST [("fields", inText "[]"),("accepted_author_attachments", inText "[]")]
    (docApiV2SigSign did slid) 200

  eaJSON <- jsonTestRequestHelper ctx GET [] (docApiV2EvidenceAttachments did) 200
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
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  req <- mkRequest GET []
  (rsp,_) <- runTestKontra req ctx $ docApiV2FilesMain did "filename.pdf"
  assertEqual "Successful `docApiV2FilesMain` response code" 200 (rsCode rsp)

testDocApiV2FilesGet :: TestEnv ()
testDocApiV2FilesGet = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  mockDocSet <- mockDocTestRequestHelper ctx POST [
      ("attachments", inText $ "[{\"name\" : \"simple-rotate-90.pdf\", \"required\" : false, \"file_param\" : \"afile\"}]")
    , ("afile", inFile "test/pdfs/simple-rotate-90.pdf")
    ] (docApiV2SetAttachments did) 200

  let fid = fileIDFromMockAuthorAttachment . mockDocAuthorAttachmentNumber 1 $ mockDocSet
  req <- mkRequest GET []
  (rsp,_) <- runTestKontra req ctx $ docApiV2FilesGet did fid "somefile.pdf"
  assertEqual "Successful `docApiV2FilesGet` response code" 200 (rsCode rsp)

testDocApiV2Texts :: TestEnv ()
testDocApiV2Texts = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- documentIDFromMockDoc <$> testDocApiV2New' ctx

  mockDocSetFile <- mockDocTestRequestHelper ctx POST [("file", inFile "test/pdfs/simple.pdf")] (docApiV2SetFile did) 200
  let fid = fileIDFromMockDocFile mockDocSetFile

  rspJSON <- jsonTestRequestHelper ctx
    GET [("json", inText "{ \"rects\":[{ \"rect\": [0,0,1,1] , \"page\": 1 }]}")]
    (docApiV2Texts did fid) 200
  rectsArrayFirstValue <- liftM V.head $ lookupObjectArray "rects" rspJSON
  linesValueList <- liftM V.toList $ lookupObjectArray "lines" rectsArrayFirstValue
  let toText (String s) = s
      toText _ = $unexpectedError "Should have been 'String' constructor"
      line:[] = map toText linesValueList
  assertEqual "Lines in `docApiV2Texts` should match" "This is a test contract pdf." line
