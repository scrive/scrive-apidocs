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

integrationAPITests :: Connection -> Test
integrationAPITests conn = testGroup "Integration API" [
      testCase "Testing if we can create sample document" $ testDocumentCreation conn
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
    ]

-- Main tests

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
    assertBool ("Two document was created by  this company but " ++ (show $ docsCount apiRespDocs1) ++ " were found") $ (docsCount apiRespDocs1) == 2
    apiReqDocs2 <- getDocumentsJSON "test_company2" "mariusz@skrivapa.se"
    apiRespDocs2 <- makeAPIRequest getDocuments $ apiReqDocs2
    assertBool ("One document was created by  this company but " ++ (show $ docsCount apiRespDocs2) ++ " were found") $ (docsCount apiRespDocs2) == 1
    apiReqDocs3 <- getDocumentsJSON "test_company3" "mariusz+1@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest getDocuments $ apiReqDocs3
    assertBool ("No document was created for this company but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 0
    let Ok ard = decode "{\"company_id\":\"test_company1\"}"
    arspd <- makeAPIRequest getDocuments $ ard
    assertBool ("Example from doc. Two document was created by  this company but " ++ (show $ docsCount arspd) ++ " were found") $ (docsCount arspd) == 2


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
createDocumentJSON company author = randomCall $ \title fname sname -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString  title)
         ,("type" , JSRational True (1%1))
         ,("involved" , JSArray [ JSObject $ toJSObject $
                                    [ ("fstname", JSString $ toJSString fname),
                                      ("sndname", JSString $ toJSString sname),
                                      ("email",   JSString $ toJSString author)
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
    ctx <- (\c -> c { ctxdbconn = conn }) <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    rq <- mkRequest POST [("service", inText "test_service"), ("password", inText "test_password") ,("body", inText $ encode req)]
    fmap fst $ runTestKontra rq ctx $ testAPI handler

-- A service to be used with API. We need one to use it.
createTestService :: DB ()
createTestService = do
    pwd <- createPassword $ BS.pack "test_password"
    Just User{userid} <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "mariusz@skrivapa.se") (Just pwd) False Nothing Nothing defaultValue (mkLocaleFromRegion defaultValue)
    _ <- dbUpdate $ CreateService (ServiceID $ BS.pack "test_service") (Just pwd) userid
    return ()
    
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

