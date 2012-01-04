module IntegrationAPITest (integrationAPITests) where

import Control.Applicative
import Happstack.Server
import Test.HUnit (Assertion)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Char8 as BS
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import Misc
import API.API
import API.Service.Model
import API.IntegrationAPI
import Context
import DB.Classes
import Text.JSON
import Data.Ratio
import Data.List
import Data.Maybe
import Database.HDBC.PostgreSQL
import Util.JSON
import Test.QuickCheck.Gen
import Control.Exception
import System.Timeout
--import Doc.DocStateData
--import qualified AppLogger as Log
--import Doc.Transitory

integrationAPITests :: Connection -> Test
integrationAPITests conn = testGroup "Integration API" [
    --  testCase "Test crazy exponent in JSON" $ testLargeExponent 
     testCase "Test creating a offer from template" $ testDocumentCreationFromTemplate conn      
    , testCase "Test send to friend delete scenario" $ testFriendDeleteScenario conn
    , testCase "Test connect to existing service user" $ testSignatoryNoCompany conn
    , testCase "Test creating a contract from template" $ testDocumentCreationFromTemplateContract conn
    , testCase "Test creating an order from template" $ testDocumentCreationFromTemplateOrder conn
    , testCase "Testing if we can create sample document" $ testDocumentCreation conn
    , testCase "Accessing documents created by the API" $ testDocumentsAccess conn
    , testCase "Accessing documents in embedded frame" $ testDocumentAccessEmbeddedPage conn
    , testCase "new_document example" $ testNewDocumentWithSpecificExample conn
    , testCase "new_document other example" $ testNewDocumentWithOtherSpecificExample conn
    , testCase "embed_document_frame exampel" $ testEmbedDocumentFrameFromExamples conn
    , testCase "remove_document example" $ testRemoveDocumentFromExamples conn
    , testCase "set_document_tag example" $ testSetDocumentTagFromExamples conn
    , testCase "personal number gets saved" $ testNewDocumentWithPersonalNr conn
    , testCase "company number gets saved" $ testNewDocumentWithCompanyNr conn
    , testCase "test that extra fields get added" $ testNewDocumentExtraFields conn
    , testCase "test that new document shows locale" $ testNewDocumentLocale conn
    , testCase "test that you can set region (and not language) on new document and it works!" $ testNewDocumentSetRegion conn
    , testCase "test that se works as a region" $ testNewDocumentSetRegionSE conn
    , testCase "test that gb/en works as a locale" $ testNewDocumentSetRegionGBEN conn
    , testCase "test that gb/sv works as a locale, but converts to gb/eb" $ testNewDocumentSetRegionGBSV conn
    , testCase "test that GB works as a region (all caps)" $ testNewDocumentSetRegionGB conn
    
      --, testCase "test that signatories have attachment array" $ testNewDocumentAttachments conn    
    , testCase "test create order" $ testNewDocumentOrder conn      
      
      
    , testCase "Test that you can set the relation for a signatory and read it back" $ testNewDocumentRelations conn
    , testCase "Test that we can create from templates" $ testCreateFromTemplate conn
    ]

-- Main tests

testFriendDeleteScenario :: Connection -> Assertion
testFriendDeleteScenario conn = withTestEnvironment conn $ do
  createTestService
  -- this will create a user for eric@scrive.com in test_company1
  x <- createDocumentJSON "test_company1" "eric@scrive.com"
  x' <- makeAPIRequest createDocument x
  let Right (JSString docid') = jsget "document_id" $ JSObject x'
  _ <- makeAPIRequest getDaveDoc $ fromRight $ jsset "document_id" docid' jsempty
  -- this will create a user mariusz@skrivapa.se in test_company1
  -- and also find the old user eric@scrive.com and set his company
  apiReq1 <- createDocumentJSONFriend "test_company1" "mariusz@skrivapa.se" "eric@scrive.com"
  apiRes1 <- makeAPIRequest createDocument $ apiReq1
  assertBool ("Failed to create document :" ++ show apiRes1)  $ not (isError apiRes1)
  let Right (JSString docid) = jsget "document_id" $ JSObject apiRes1
  _ <- makeAPIRequest getDaveDoc $ fromRight $ jsset "document_id" docid jsempty
  _rsp <- makeAPIRequest removeDocument $ fromRight $ jsset "document_id" docid jsempty
  _rsp' <- makeAPIRequest removeDocument $ fromRight $ jsset "document_id" docid' jsempty
  doclist <- makeAPIRequest getDocuments $ fromRight $ jsset "company_id" "test_company1" jsempty
  assertBool ("Should have no documents, but got: " ++ show doclist) $ Right (JSArray []) == jsget "documents" (JSObject doclist)

testSignatoryNoCompany :: Connection -> Assertion
testSignatoryNoCompany conn = withTestEnvironment conn $ do
  createTestService
  -- this will create a user for eric@scrive.com in test_company2
  x <- createDocumentJSON "test_company2" "eric@scrive.com"
  x' <- makeAPIRequest createDocument x
  let Right (JSString docid') = jsget "document_id" $ JSObject x'
  -- this will create a user mariusz@skrivapa.se in test_company1
  -- and also find the old user eric@scrive.com and set his company
  apiReq1 <- createDocumentJSONFriend "test_company1" "mariusz@skrivapa.se" "eric@scrive.com"
  apiRes1 <- makeAPIRequest createDocument $ apiReq1
  assertBool ("Failed to create document :" ++ show apiRes1)  $ not (isError apiRes1)
  _rsp' <- makeAPIRequest removeDocument $ fromRight $ jsset "document_id" docid' jsempty
  doclist <- makeAPIRequest getDocuments $ fromRight $ jsset "company_id" "test_company2" jsempty
  assertBool ("Should have no documents, but got: " ++ show doclist) $ Right (JSArray []) == jsget "documents" (JSObject doclist)


testNewDocumentOrder :: Connection -> Assertion
testNewDocumentOrder conn = withTestEnvironment conn $ do
  createTestService
  apiReq <- createOrderJSON "test_company1" "mariusz@skrivapa.se"
  apiRes <- makeAPIRequest createDocument $ apiReq
  assertBool ("Failed to create doc: " ++ show apiRes) $ not (isError apiRes)
  let Right did = jsget ["document_id"] (showJSON apiRes)
  let Right apiReq2 = jsset "document_id" did jsempty
  apiRes2 <- makeAPIRequest getDocument apiReq2
  assertBool ("Failed to get doc: " ++ show apiRes2) $ not (isError apiRes2)
  assertBool ("doctype is not order: " ++ show apiRes2) $ (Right (showJSON (5 :: Int))) == jsget ["document", "type"] (showJSON apiRes2)
  let ar1 = (jsget "relation" $ fromRight $ jsgetA 0 $ fromRight $ jsget "involved"  (showJSON apiReq))
  let ar2 = (jsget "relation" $ fromRight $ jsgetA 0 $ fromRight $ jsget "involved" $ fromRight $ jsget "document" (showJSON apiRes2))
  assertBool ("relation for author is as expected " ++ show (ar1,ar2)) (ar1 == ar2 || (isLeft ar1 && ar2 == Right (JSRational False (1%1))))
  
testDocumentCreation :: Connection -> Assertion
testDocumentCreation conn = withTestEnvironment conn $ do
    createTestService
    apiReq1 <- createDocumentJSON "test_company1" "mariusz@skrivapa.se"
    apiRes1 <- makeAPIRequest createDocument $ apiReq1
    assertBool ("Failed to create first document :" ++ show apiRes1)  $ not (isError apiRes1)
    apiReq2 <- createDocumentJSON "test_company2" "mariusz@skrivapa.se"
    apiRes2 <- makeAPIRequest createDocument $ apiReq2
    assertBool ("Failed to create second document" ++ show apiRes2) $ not (isError apiRes2)
    apiReq3 <- createDocumentJSON "test_company1" "mariusz+1@skrivapa.se"
    apiRes3 <- makeAPIRequest createDocument $ apiReq3
    assertBool ("Failed to create third document" ++ show apiRes3) $ not (isError apiRes3)

testDocumentsAccess :: Connection -> Assertion
testDocumentsAccess conn = withTestEnvironment conn $ do
    createTestService
    _ <- makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se" 
    _ <- makeAPIRequest createDocument =<< createDocumentJSON "test_company2" "mariusz@skrivapa.se" 
    _ <- makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz+1@skrivapa.se" 
    apiReqDocs1 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs1 <- makeAPIRequest getDocuments $ apiReqDocs1
    assertBool ("Two documents were created by this company but " ++ (show $ docsCount apiRespDocs1) ++ " were found") $ (docsCount apiRespDocs1) == 2
    apiReqDocs2 <- getDocumentsJSON "test_company2" "mariusz@skrivapa.se"
    apiRespDocs2 <- makeAPIRequest getDocuments $ apiReqDocs2
    assertBool ("One document was created by this company but " ++ (show $ docsCount apiRespDocs2) ++ " were found") $ (docsCount apiRespDocs2) == 1
    apiReqDocs3 <- getDocumentsJSON "test_company3" "mariusz+1@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest getDocuments $ apiReqDocs3
    assertBool ("No document was created for this company but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 0
    let Ok ard = decode "{\"company_id\":\"test_company1\"}"
    arspd <- makeAPIRequest getDocuments $ ard
    assertBool ("Example from doc. Two documents were created by this company but " ++ (show $ docsCount arspd) ++ " were found") $ (docsCount arspd) == 2


testDocumentAccessEmbeddedPage :: Connection -> Assertion
testDocumentAccessEmbeddedPage conn = withTestEnvironment conn $ do
    createTestService
    docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    docid2 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company2" "mariusz@skrivapa.se")
    docid3 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz+1@skrivapa.se")
    apiRespDocs1 <- makeAPIRequest embeddDocumentFrame =<< getEmbedDocumentaJSON docid1 "test_company1" "mariusz@skrivapa.se"
    assertBool ("User from right company could not access his document") $ containsUserEmbedLink apiRespDocs1
    apiRespDocs2 <- makeAPIRequest embeddDocumentFrame =<< getEmbedDocumentaJSON docid2 "test_company1" "mariusz@skrivapa.se"
    assertBool ("Company could access other company documents") $ isError apiRespDocs2
    apiRespDocs3 <- makeAPIRequest embeddDocumentFrame =<< getEmbedDocumentaJSON docid3 "test_company1" "mariusz@skrivapa.se"
    assertBool ("Company could not access its documents, with nonauthor") $ containsCompanyEmbedLink apiRespDocs3

testNewDocumentWithSpecificExample :: Connection -> Assertion
testNewDocumentWithSpecificExample conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",  \"title\":\"test\",  \"type\":3,  \"involved\":[{\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\"}],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  assertBool ("response json does not have document_id") $ isJust $ getJSONField "document_id" rsp

testNewDocumentWithOtherSpecificExample :: Connection -> Assertion
testNewDocumentWithOtherSpecificExample conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\"},    {\"email\":\"eric@scrive.com\",\"fstname\":\"Eric\",\"sndname\":\"Nordman\"},    {\"email\":\"mariusz+1@scrive.com\",\"fstname\":\"Mike\",\"sndname\":\"Other\"} ],\"tags\":[    {\"name\":\"year\",\"value\":\"2010\"},    {\"name\":\"month\",\"value\":\"6\"}],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  assertBool ("response json does not have document_id") $ isJust $ getJSONField "document_id" rsp

testEmbedDocumentFrameFromExamples :: Connection -> Assertion
testEmbedDocumentFrameFromExamples conn = withTestEnvironment conn $ do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"email\":\"mariusz@skrivapa.se\",\"company_id\":\"test_company1\",\"document_id\":" ++ show docid1 ++ ",\"location\":\"http://192.168.0.16:8080/Demo/index.jsp\"}"
  rsp <- makeAPIRequest embeddDocumentFrame rq
  assertBool ("response json does not have link field: " ++ show rsp) $ isJust $ getJSONField "link" rsp

testRemoveDocumentFromExamples :: Connection -> Assertion
testRemoveDocumentFromExamples conn = withTestEnvironment conn $ do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"document_id\":" ++ show docid1 ++ "}"
  _rsp <- makeAPIRequest removeDocument rq -- the response is empty
  assertBool "Done" True

testSetDocumentTagFromExamples :: Connection -> Assertion
testSetDocumentTagFromExamples conn = withTestEnvironment conn $ do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"document_id\":" ++ show docid1 ++ ",\"tag\"    : { \"name\": \"client\", \"value\": \"3213123\" }}"
  _rsp <- makeAPIRequest setDocumentTag rq -- response empty
  assertBool "Done" True

testNewDocumentWithPersonalNr :: Connection -> Assertion
testNewDocumentWithPersonalNr conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\"} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [JSObject m]) -> case getJSONField "personalnr" m of
            Just pn -> assertBool ("personal nr is not equal" ++ show pn) $ pn == showJSON "1234"
            _ -> assertBool ("Personal nr field does not exist") False
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentWithCompanyNr :: Connection -> Assertion
testNewDocumentWithCompanyNr conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"companynr\" : \"1234\"} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [(JSObject m)]) -> case getJSONField "companynr" m of
            Just pn -> assertBool ("company nr is not equal" ++ show pn) $ pn == showJSON "1234"
            _ -> assertBool ("company nr field does not exist") False
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document returned: " ++ show doc

-- Requests body
createDocumentJSON :: String -> String -> DB JSValue
createDocumentJSON company author = do
     dt <- rand 10 $  elements [1,3,5]
     randomCall $ \title fname sname -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString $ fromSNN title)
         ,("type" , JSRational True (dt%1))
         ,("involved" , JSArray [ JSObject $ toJSObject $
                                    [ ("fstname", JSString $ toJSString fname),
                                      ("sndname", JSString $ toJSString sname),
                                      ("email",   JSString $ toJSString author)
                                    ]
                                ]
        )]
        
createDocumentJSONFriend :: String -> String -> String -> DB JSValue
createDocumentJSONFriend company author friend = do
     dt <- rand 10 $  elements [1,3,5]
     randomCall $ \title fname sname fname2 sname2 -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString $ fromSNN title)
         ,("type" , JSRational True (dt%1))
         ,("involved" , JSArray [ JSObject $ toJSObject $
                                    [ ("fstname", JSString $ toJSString fname),
                                      ("sndname", JSString $ toJSString sname),
                                      ("email",   JSString $ toJSString author),
                                      ("companynr", JSString $ toJSString company)
                                    ],
                                  JSObject $ toJSObject $
                                  [ ("fstname", JSString $ toJSString fname2),
                                      ("sndname", JSString $ toJSString sname2),
                                      ("email",   JSString $ toJSString friend),
                                      ("companynr", JSString $ toJSString company)
                                    ]

                                ]
        )]


createOrderJSON :: String -> String -> DB JSValue
createOrderJSON company author = randomCall $ \title fname sname fname2 sname2 em2 -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString $ fromSNN title)
         ,("type" , JSRational True (5%1))
         ,("involved" , JSArray [ JSObject $ toJSObject $
                                    [ ("fstname", JSString $ toJSString fname),
                                      ("sndname", JSString $ toJSString sname),
                                      ("email",   JSString $ toJSString author)
                                    ],
                                  JSObject $ toJSObject $
                                  [ ("fstname", JSString $ toJSString fname2),
                                    ("sndname", JSString $ toJSString sname2),
                                    ("email",   JSString $ toJSString em2)
                                  ]
                                ]
        )]
        

getDocumentsJSON :: String -> String -> DB JSValue
getDocumentsJSON company email = randomCall $ JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("email" , JSString $ toJSString  email)
        ]

getEmbedDocumentaJSON :: String -> String -> String -> DB JSValue
getEmbedDocumentaJSON  documentid company email = randomCall $ JSObject $ toJSObject $
        [ ("document_id", JSString $ toJSString documentid)
         ,("company_id", JSString $ toJSString company)
         ,("email" , JSString $ toJSString  email)
        ]

-- Making requests
makeAPIRequest :: IntegrationAPIFunction TestKontra APIResponse -> APIRequestBody -> DB APIResponse
makeAPIRequest handler req = wrapDB $ \conn -> do
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbconn = conn })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    rq <- mkRequest POST [("service", inText "test_service"), ("password", inText "test_password") ,("body", inText $ encode req)]
    fmap fst $ runTestKontra rq ctx $ testAPI handler

-- A service to be used with API. We need one to use it.
createTestService :: DB ()
createTestService = do
  pwd <- createPassword $ BS.pack "test_password"
  muser <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "mariusz@skrivapa.se") (Just pwd) False Nothing Nothing (mkLocaleFromRegion defaultValue)
  case muser of
    Nothing -> error "can't create user"
    Just User{userid} ->
      ignore $ dbUpdate $ CreateService (ServiceID $ BS.pack "test_service") (Just pwd) userid

testNewDocumentExtraFields :: Connection -> Assertion
testNewDocumentExtraFields conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [JSObject m]) -> case getJSONField "fields" m of
            Just (JSArray [JSObject prs1, JSObject prs2]) -> do
              assertBool ("name of first field was not prs1, in " ++ show prs1) $ getJSONField "name" prs1 == Just (showJSON "prs1")
              assertBool ("value of first field was not val1, in " ++ show prs1) $ getJSONField "value" prs1 == Just (showJSON "val1")
              assertBool ("name of second field was not prs2, in " ++ show prs2) $ getJSONField "name" prs2 == Just (showJSON "prs2")
              assertBool ("value of second field was not val2, in " ++ show prs2) $ getJSONField "value" prs2 == Just (showJSON "val2")
            _ -> assertBool ("fields does not exist") False
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc


testNewDocumentRelations :: Connection -> Assertion
testNewDocumentRelations conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"relation\": 1,  \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [JSObject m]) -> case getJSONField "relation" m of
            Just x@(JSRational _ _) -> 
              assertBool ("Relation was not successfully set") $ x == showJSON (1::Int)
            _ -> assertBool ("relation does not exist") False
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc


-- actually, this is not the case right now
_testNewDocumentAttachments :: Connection -> Assertion
_testNewDocumentAttachments conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [JSObject m]) -> case getJSONField "attachments" m of
            Just (JSArray _) -> assertSuccess
            a -> assertFailure $ "attachments was wrong, looked for it, got " ++ show a
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentLocale :: Connection -> Assertion
testNewDocumentLocale conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ isJust $ getJSONField "region" l
            assertBool ("no language in locale, in " ++ show l) $ isJust $ getJSONField "language" l
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegion :: Connection -> Assertion
testNewDocumentSetRegion conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "gb")
            assertBool ("no language in locale, in " ++ show l) $ isJust $ getJSONField "language" l
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegionSE :: Connection -> Assertion
testNewDocumentSetRegionSE conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"SE\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "se")
            assertBool ("no language in locale, in " ++ show l) $ isJust $ getJSONField "language" l
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegionGB :: Connection -> Assertion
testNewDocumentSetRegionGB conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"GB\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "gb")
            assertBool ("no language in locale, in " ++ show l) $ isJust $ getJSONField "language" l
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegionGBEN :: Connection -> Assertion
testNewDocumentSetRegionGBEN conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"en\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "gb")
            assertBool ("no language in locale, in " ++ show l) $ getJSONField "language" l == Just (showJSON "en")
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegionGBSV :: Connection -> Assertion
testNewDocumentSetRegionGBSV conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "gb")
            assertBool ("no language in locale, in " ++ show l) $ getJSONField "language" l == Just (showJSON "en")
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

-- test template creation
testDocumentCreationFromTemplate :: Connection -> Assertion
testDocumentCreationFromTemplate conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":4,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just jsdid -> do
      let did = encode jsdid
      let Ok rq2 = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":3, \"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"}, \"template_id\" : " ++ did ++ "}"
      rsp2 <- makeAPIRequest createDocument rq2
      case getJSONField "document_id" rsp2 of
        Nothing -> error $ "Could not get document_id from response " ++ show rsp2
        Just d -> do
          res <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", d)]
          case getJSONField "document" res of
            Nothing -> error $ "What? document not created " ++ show res
            Just (JSObject doc) -> do
              assertBool ("Was not offer; was " ++ show (getJSONField "type" doc) ++ " json " ++ show doc ) $ getJSONField "type" doc == Just (showJSON (3::Int))
              case getJSONField "files" doc of
                Nothing -> assertFailure "Could not find any files"
                Just (JSArray [JSObject f]) -> assertBool ("Wrong filename " ++ show f) $ 
                                               getJSONField "name" f == Just (showJSON "Empty.pdf")
                a -> assertFailure $ "What the hell is this? " ++ show a
            x -> error $ "huh? " ++ show x


testDocumentCreationFromTemplateContract :: Connection -> Assertion
testDocumentCreationFromTemplateContract conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":2,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just jsdid -> do
      let did = encode jsdid
      let Ok rq2 = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1, \"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"}, \"template_id\" : " ++ did ++ "}"
      rsp2 <- makeAPIRequest createDocument rq2
      case getJSONField "document_id" rsp2 of
        Nothing -> error $ "Could not get document_id from response " ++ show rsp2
        Just d -> do
          res <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", d)]
          case getJSONField "document" res of
            Nothing -> error $ "What? document not created " ++ show res
            Just (JSObject doc) -> do
              assertBool ("Was not contract; was " ++ show (getJSONField "type" doc) ++ " json " ++ show doc ) $ getJSONField "type" doc == Just (showJSON (1::Int))
              case getJSONField "files" doc of
                Nothing -> assertFailure "Could not find any files"
                Just (JSArray [JSObject f]) -> assertBool ("Wrong filename " ++ show f) $ 
                                               getJSONField "name" f == Just (showJSON "Empty.pdf")
                a -> assertFailure $ "What the hell is this? " ++ show a
            x -> error $ "huh? " ++ show x

testDocumentCreationFromTemplateOrder :: Connection -> Assertion
testDocumentCreationFromTemplateOrder conn = withTestEnvironment conn $ do
  createTestService
  let Ok rq = decode "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":6,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9GbGF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAbSUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+PgplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+CmVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2IDAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZXZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvVHlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhbmcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3MjAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwNjUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5MDA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0ZShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwMDAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwMDAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwIG4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4IDAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEIFsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3MjQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwMEM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg==\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just jsdid -> do
      let did = encode jsdid
      let Ok rq2 = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":5, \"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"}, \"template_id\" : " ++ did ++ "}"
      rsp2 <- makeAPIRequest createDocument rq2
      case getJSONField "document_id" rsp2 of
        Nothing -> error $ "Could not get document_id from response " ++ show rsp2
        Just d -> do
          res <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", d)]
          case getJSONField "document" res of
            Nothing -> error $ "What? document not created " ++ show res
            Just (JSObject doc) -> do
              assertBool ("Was not contract; was " ++ show (getJSONField "type" doc) ++ " json " ++ show doc ) $ getJSONField "type" doc == Just (showJSON (5::Int))
              case getJSONField "files" doc of
                Nothing -> assertFailure "Could not find any files"
                Just (JSArray [JSObject f]) -> assertBool ("Wrong filename " ++ show f) $ 
                                               getJSONField "name" f == Just (showJSON "Empty.pdf")
                a -> assertFailure $ "What the hell is this? " ++ show a
            x -> error $ "huh? " ++ show x

-- Utils
isError :: JSObject JSValue -> Bool
isError = isJust . lookup "error" . fromJSObject

docsCount :: JSObject JSValue -> Int
docsCount obj = case getJSONField "documents" obj of
                   Just (JSArray docs) -> length docs
                   _ -> -1

containsUserEmbedLink :: JSObject JSValue -> Bool
containsUserEmbedLink obj =    "connectuser" `isInfixOf` (getJSONStringField "link" obj)

containsCompanyEmbedLink :: JSObject JSValue -> Bool
containsCompanyEmbedLink obj = "connectcompany" `isInfixOf` (getJSONStringField "link" obj)

  

createTemplateJSON :: String -> String -> DB JSValue
createTemplateJSON company author = randomCall $ \title fname sname -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString  title)
         ,("type" , JSRational True (2%1))
         ,("involved" , JSArray [ JSObject $ toJSObject $
                                    [ ("fstname", JSString $ toJSString fname),
                                      ("sndname", JSString $ toJSString sname),
                                      ("email",   JSString $ toJSString author)
                                    ]
                                ]
        )]


createFromTemplateJSON :: String -> String -> JSValue -> DB JSValue
createFromTemplateJSON company author did = randomCall $ \title fname sname -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString  title)
         ,("type" , JSRational True (1%1))
         ,("template_id", did)
         ,("involved" , JSArray [ JSObject $ toJSObject $
                                    [ ("fstname", JSString $ toJSString fname),
                                      ("sndname", JSString $ toJSString sname),
                                      ("email",   JSString $ toJSString author)
                                    ]
                                ]
        )]



testCreateFromTemplate :: Connection -> Assertion
testCreateFromTemplate conn = withTestEnvironment conn $ do
  createTestService
  apiReq <- createTemplateJSON "test_company1" "mariusz@skrivapa.se"
  apiRes <- makeAPIRequest createDocument $ apiReq
  assertBool ("Failed to create doc: " ++ show apiRes) $ not (isError apiRes)    
  let Right did = jsget ["document_id"] (showJSON apiRes)
  apiReq' <- createFromTemplateJSON "test_company1" "mariusz@skrivapa.se" did
  apiRes' <- makeAPIRequest createDocument apiReq'
  assertBool ("Failed to get doc: " ++ show apiRes') $ not (isError apiRes')  
  let Right did2 = jsget ["document_id"] (showJSON apiRes')
  let Right apiReq2 = (Right jsempty) >>= jsset "document_id" did2
  apiRes2 <- makeAPIRequest getDocument apiReq2
  assertBool ("Failed to get doc: " ++ show apiRes2) $ not (isError apiRes2)
  assertBool ("doctype is not contract: " ++ show apiRes2) $ (Right (showJSON (1 :: Int))) == jsget ["document", "type"] (showJSON apiRes2)
  
_testLargeExponent :: Assertion
_testLargeExponent = do
  res :: Maybe (Result Int) <- System.Timeout.timeout 1000000 $ evaluate $ decode "7e10000000000"
  case res of
    Nothing -> assertFailure "Should not timeout"
    Just (Error _) -> assertSuccess
    Just (Ok _) -> assertFailure $ "Should not parse bad json but I can't show you what I got (it takes too much memory)"
