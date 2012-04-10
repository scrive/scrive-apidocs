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
import Kontra (Kontra)
import User.Model
import IPAddress
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
import Util.JSON
import Test.QuickCheck.Gen
import Control.Exception
import System.Timeout
import qualified Log
import MinutesTime
import Doc.Model
import Control.Monad
import Doc.DocStateData
import EvidenceLog.Model

import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

integrationAPITests :: DBEnv -> Test
integrationAPITests env = testGroup "Integration API" [
    --  testCase "Test crazy exponent in JSON" $ testLargeExponent 
     testCase "Test creating a offer from template" $ testDocumentCreationFromTemplate env      
    , testCase "Test from state filtering" $ testDocumentsFilteringFromState env      
    , testCase "Test to state filtering" $ testDocumentsFilteringToState env
    , testCase "Test to date filtering" $ testDocumentsFilteringToDate env
    , testCase "Test from date filtering" $ testDocumentsFilteringFromDate env      
    , testCase "Test from date filtering with viewing" $ testDocumentsFilteringFromDate2 env      
    , testCase "Test send to friend delete scenario" $ testFriendDeleteScenario env
    , testCase "Test envect to existing service user" $ testSignatoryNoCompany env
    , testCase "Test creating a contract from template" $ testDocumentCreationFromTemplateContract env
    , testCase "Test creating an order from template" $ testDocumentCreationFromTemplateOrder env
    , testCase "Testing if we can create sample document" $ testDocumentCreation env
    , testCase "Accessing documents created by the API" $ testDocumentsAccess env
    , testCase "Accessing documents in embedded frame" $ testDocumentAccessEmbeddedPage env
    , testCase "new_document example" $ testNewDocumentWithSpecificExample env
    , testCase "new_document other example" $ testNewDocumentWithOtherSpecificExample env
    , testCase "embed_document_frame exampel" $ testEmbedDocumentFrameFromExamples env
    , testCase "remove_document example" $ testRemoveDocumentFromExamples env
    , testCase "set_document_tag example" $ testSetDocumentTagFromExamples env
    , testCase "personal number gets saved" $ testNewDocumentWithPersonalNr env
    , testCase "company number gets saved" $ testNewDocumentWithCompanyNr env
    , testCase "test that extra fields get added" $ testNewDocumentExtraFields env
    , testCase "test that new document shows locale" $ testNewDocumentLocale env
    , testCase "test that you can set region (and not language) on new document and it works!" $ testNewDocumentSetRegion env
    , testCase "test that se works as a region" $ testNewDocumentSetRegionSE env
    , testCase "test that gb/en works as a locale" $ testNewDocumentSetRegionGBEN env
    , testCase "test that gb/sv works as a locale, but converts to gb/eb" $ testNewDocumentSetRegionGBSV env
    , testCase "test that GB works as a region (all caps)" $ testNewDocumentSetRegionGB env
    
      --, testCase "test that signatories have attachment array" $ testNewDocumentAttachments env    
    , testCase "test create order" $ testNewDocumentOrder env      
      
      
    , testCase "Test that you can set the relation for a signatory and read it back" $ testNewDocumentRelations env
    , testCase "Test that we can create from templates" $ testCreateFromTemplate env
    ]

-- Main tests

testFriendDeleteScenario :: DBEnv -> Assertion
testFriendDeleteScenario env = withTestEnvironment env $ do
  createTestService
  -- this will create a user for eric@scrive.com in test_company1
  x <- createDocumentJSON "test_company1" "eric@scrive.com"
  x' <- makeAPIRequest env createDocument x
  let Right (JSString docid') = jsget "document_id" $ JSObject x'
  _ <- makeAPIRequest env getDaveDoc $ fromRight $ jsset "document_id" docid' jsempty
  -- this will create a user mariusz@skrivapa.se in test_company1
  -- and also find the old user eric@scrive.com and set his company
  apiReq1 <- createDocumentJSONFriend "test_company1" "mariusz@skrivapa.se" "eric@scrive.com"
  apiRes1 <- makeAPIRequest env createDocument $ apiReq1
  assertBool ("Failed to create document :" ++ show apiRes1)  $ not (isError apiRes1)
  let Right (JSString docid) = jsget "document_id" $ JSObject apiRes1
  _ <- makeAPIRequest env getDaveDoc $ fromRight $ jsset "document_id" docid jsempty
  _rsp <- makeAPIRequest env removeDocument $ fromRight $ jsset "document_id" docid jsempty
  _rsp' <- makeAPIRequest env removeDocument $ fromRight $ jsset "document_id" docid' jsempty
  doclist <- makeAPIRequest env getDocuments $ fromRight $ jsset "company_id" "test_company1" jsempty
  assertBool ("Should have no documents, but got: " ++ show doclist) $ Right (JSArray []) == jsget "documents" (JSObject doclist)

testSignatoryNoCompany :: DBEnv -> Assertion
testSignatoryNoCompany env = withTestEnvironment env $ do
  createTestService
  -- this will create a user for eric@scrive.com in test_company2
  x <- createDocumentJSON "test_company2" "eric@scrive.com"
  x' <- makeAPIRequest env createDocument x
  let Right (JSString docid') = jsget "document_id" $ JSObject x'
  -- this will create a user mariusz@skrivapa.se in test_company1
  -- and also find the old user eric@scrive.com and set his company
  apiReq1 <- createDocumentJSONFriend "test_company1" "mariusz@skrivapa.se" "eric@scrive.com"
  apiRes1 <- makeAPIRequest env createDocument $ apiReq1
  assertBool ("Failed to create document :" ++ show apiRes1)  $ not (isError apiRes1)
  _rsp' <- makeAPIRequest env removeDocument $ fromRight $ jsset "document_id" docid' jsempty
  doclist <- makeAPIRequest env getDocuments $ fromRight $ jsset "company_id" "test_company2" jsempty
  assertBool ("Should have no documents, but got: " ++ show doclist) $ Right (JSArray []) == jsget "documents" (JSObject doclist)


testNewDocumentOrder :: DBEnv -> Assertion
testNewDocumentOrder env = withTestEnvironment env $ do
  createTestService
  apiReq <- createOrderJSON "test_company1" "mariusz@skrivapa.se"
  apiRes <- makeAPIRequest env createDocument $ apiReq
  assertBool ("Failed to create doc: " ++ show apiRes) $ not (isError apiRes)
  let Right did = jsget ["document_id"] (showJSON apiRes)
  let Right apiReq2 = jsset "document_id" did jsempty
  apiRes2 <- makeAPIRequest env getDocument apiReq2
  assertBool ("Failed to get doc: " ++ show apiRes2) $ not (isError apiRes2)
  assertBool ("doctype is not order: " ++ show apiRes2) $ (Right (showJSON (5 :: Int))) == jsget ["document", "type"] (showJSON apiRes2)
  --let ar1 = (jsget "relation" $ fromRight $ jsgetA 0 $ fromRight $ jsget "involved"  (showJSON apiReq)) | No idea what the problem could be
  --let ar2 = (jsget "relation" $ fromRight $ jsgetA 0 $ fromRight $ jsget "involved" $ fromRight $ jsget "document" (showJSON apiRes2))
  --assertBool ("relation for author is as expected " ++ show (ar1,ar2)) (ar1 == ar2 || (isLeft ar1 && ar2 == Right (JSRational False (5%1))))
  
testDocumentCreation :: DBEnv -> Assertion
testDocumentCreation env = withTestEnvironment env $ do
    createTestService
    apiReq1 <- createDocumentJSON "test_company1" "mariusz@skrivapa.se"
    apiRes1 <- makeAPIRequest env createDocument $ apiReq1
    assertBool ("Failed to create first document :" ++ show apiRes1)  $ not (isError apiRes1)
    apiReq2 <- createDocumentJSON "test_company2" "mariusz@skrivapa.se"
    apiRes2 <- makeAPIRequest env createDocument $ apiReq2
    assertBool ("Failed to create second document" ++ show apiRes2) $ not (isError apiRes2)
    apiReq3 <- createDocumentJSON "test_company1" "mariusz+1@skrivapa.se"
    apiRes3 <- makeAPIRequest env createDocument $ apiReq3
    assertBool ("Failed to create third document" ++ show apiRes3) $ not (isError apiRes3)

testDocumentsAccess :: DBEnv -> Assertion
testDocumentsAccess env = withTestEnvironment env $ do
    createTestService
    _ <- makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se"
    _ <- makeAPIRequest env createDocument =<< createDocumentJSON "test_company2" "mariusz@skrivapa.se"
    _ <- makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz+1@skrivapa.se"
    apiReqDocs1 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs1 <- makeAPIRequest env getDocuments $ apiReqDocs1
    assertBool ("Two documents were created by this company but " ++ (show $ docsCount apiRespDocs1) ++ " were found") $ (docsCount apiRespDocs1) == 2
    apiReqDocs2 <- getDocumentsJSON "test_company2" "mariusz@skrivapa.se"
    apiRespDocs2 <- makeAPIRequest env getDocuments $ apiReqDocs2
    assertBool ("One document was created by this company but " ++ (show $ docsCount apiRespDocs2) ++ " were found") $ (docsCount apiRespDocs2) == 1
    apiReqDocs3 <- getDocumentsJSON "test_company3" "mariusz+1@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest env getDocuments $ apiReqDocs3
    assertBool ("No document was created for this company but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 0
    let Ok ard = decode "{\"company_id\":\"test_company1\"}"
    arspd <- makeAPIRequest env getDocuments $ ard
    assertBool ("Example from doc. Two documents were created by this company but " ++ (show $ docsCount arspd) ++ " were found") $ (docsCount arspd) == 2


testDocumentAccessEmbeddedPage :: DBEnv -> Assertion
testDocumentAccessEmbeddedPage env = withTestEnvironment env $ do
    createTestService
    docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    docid2 <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company2" "mariusz@skrivapa.se")
    docid3 <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz+1@skrivapa.se")
    apiRespDocs1 <- makeAPIRequest env embeddDocumentFrame =<< getEmbedDocumentaJSON docid1 "test_company1" "mariusz@skrivapa.se"
    assertBool ("User from right company could not access his document") $ containsUserEmbedLink apiRespDocs1
    apiRespDocs2 <- makeAPIRequest env embeddDocumentFrame =<< getEmbedDocumentaJSON docid2 "test_company1" "mariusz@skrivapa.se"
    assertBool ("Company could access other company documents") $ isError apiRespDocs2
    apiRespDocs3 <- makeAPIRequest env embeddDocumentFrame =<< getEmbedDocumentaJSON docid3 "test_company1" "mariusz@skrivapa.se"
    assertBool ("Company could not access its documents, with nonauthor") $ containsCompanyEmbedLink apiRespDocs3

-- filtering based on status
testDocumentsFilteringToState :: DBEnv -> Assertion
testDocumentsFilteringToState env = withTestEnvironment env $ do
    createTestService
    _ <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest env getDocuments $ apiReqDocs3
    assertBool ("Should have 1 document but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 1
    Right apiReqDocsFilter <- jsset "to_state" (-1 :: Int) <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest env getDocuments apiReqDocsFilter
    assertBool ("All documents should be filtered out but " ++ (show $ docsCount apiRespDocsFilter) ++ " were found") $ (docsCount apiRespDocsFilter) == 0
    Right apiReqDocsFilter2 <- jsset "to_state" (100 :: Int) <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter2 <- makeAPIRequest env getDocuments apiReqDocsFilter2
    assertBool ("should have 1 doc but " ++ (show $ docsCount apiRespDocsFilter2) ++ " were found") $ (docsCount apiRespDocsFilter2) == 1

testDocumentsFilteringFromState :: DBEnv -> Assertion
testDocumentsFilteringFromState env = withTestEnvironment env $ do
    createTestService
    _ <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest env getDocuments $ apiReqDocs3
    assertBool ("Should have 1 document but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 1
    Right apiReqDocsFilter <- jsset "from_state" (20 :: Int) <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest env getDocuments apiReqDocsFilter
    if docsCount apiRespDocsFilter == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter
      else return ()
    assertBool ("All documents should be filtered out but " ++ (show $ docsCount apiRespDocsFilter) ++ " were found") $ (docsCount apiRespDocsFilter) == 0

    Right apiReqDocsFilter2 <- jsset "from_state" (0 :: Int) <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter2 <- makeAPIRequest env getDocuments apiReqDocsFilter2
    if docsCount apiRespDocsFilter2 == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter2
      else return ()
    assertBool ("should have 1 doc but " ++ (show $ docsCount apiRespDocsFilter2) ++ " were found") $ (docsCount apiRespDocsFilter2) == 1


testDocumentsFilteringToDate :: DBEnv -> Assertion
testDocumentsFilteringToDate env = withTestEnvironment env $ do
    createTestService
    _ <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest env getDocuments $ apiReqDocs3
    assertBool ("Should have 1 document but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 1
    Right apiReqDocsFilter <- jsset "to_date" "2005-01-01 00:00:00" <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest env getDocuments apiReqDocsFilter
    assertBool ("All documents should be filtered out but " ++ (show $ docsCount apiRespDocsFilter) ++ " were found") $ (docsCount apiRespDocsFilter) == 0
    ctxtime <- getMinutesTime
    let tm = minutesAfter 1000 ctxtime
        tms = showMinutesTimeForAPI tm
    Right apiReqDocsFilter2 <- jsset "to_date" tms <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    
    apiRespDocsFilter2 <- makeAPIRequest env getDocuments apiReqDocsFilter2
    assertBool ("should have 1 document but " ++ (show $ docsCount apiRespDocsFilter2) ++ " were found") $ (docsCount apiRespDocsFilter2) == 1

testDocumentsFilteringFromDate :: DBEnv -> Assertion
testDocumentsFilteringFromDate env = withTestEnvironment env $ do
    createTestService
    _ <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest env getDocuments $ apiReqDocs3
    assertBool ("Should have 1 document but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 1
    ctxtime <- getMinutesTime
    let tm = minutesAfter 1000 ctxtime
        tms = showMinutesTimeForAPI tm
        tmbefore = showMinutesTimeForAPI $ minutesBefore 1000 ctxtime
    Right apiReqDocsFilter <- jsset "from_date" tms <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest env getDocuments apiReqDocsFilter
    if docsCount apiRespDocsFilter == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter
      else return ()
    assertBool ("All documents should be filtered out but " ++ (show $ docsCount apiRespDocsFilter) ++ " were found") $ (docsCount apiRespDocsFilter) == 0

    Right apiReqDocsFilter2 <- jsset "from_date" tmbefore <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter2 <- makeAPIRequest env getDocuments apiReqDocsFilter2
    if docsCount apiRespDocsFilter2 == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter2
      else return ()
    assertBool ("Should be one document but got " ++ (show $ docsCount apiRespDocsFilter2) ++ " were found") $ (docsCount apiRespDocsFilter2) == 1

testDocumentsFilteringFromDate2 :: DBEnv -> Assertion
testDocumentsFilteringFromDate2 env = withTestEnvironment env $ do
    createTestService
    apiresult <- makeAPIRequest env createDocument =<< createDocumentJSONFriend "test_company1" "mariusz@skrivapa.se" "eric@scrive.com"
    let dids = getJSONStringField "document_id" apiresult
    let mdid = maybeRead dids
        Just did = mdid
    assertBool ("Document created successfully " ++ dids) (isJust mdid)
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest env getDocuments $ apiReqDocs3
    assertEqual ("getDocuments " ++ show apiReqDocs3 ++ " returned docs") 1 (docsCount apiRespDocs3)
    ctxtime <- getMinutesTime
    let tm = minutesAfter 1000 ctxtime
        tms = showMinutesTimeForAPI tm
        tmbefore = showMinutesTimeForAPI $ minutesBefore 1000 ctxtime    
    
    Right apiReqDocsFilter <- jsset "from_date" tms <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest env getDocuments apiReqDocsFilter
    if docsCount apiRespDocsFilter == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter
      else return ()
    assertEqual ("getDocuments " ++ encode apiReqDocsFilter ++ " returned wrong number of documents")
                 0 (docsCount apiRespDocsFilter)

    Right apiReqDocsFilter2 <- jsset "from_date" tmbefore <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter2 <- makeAPIRequest env getDocuments apiReqDocsFilter2
    if docsCount apiRespDocsFilter2 == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter2
      else return ()

    assertEqual ("getDocuments " ++ encode apiReqDocsFilter2 ++ " returned wrong numbers of documents")
                  1 (docsCount apiRespDocsFilter2)
    
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    _ <- dbUpdate $ PreparationToPending did (SystemActor ctxtime)

    _ <- forM (documentsignatorylinks doc) $ \sl ->
      if isAuthor sl 
      then dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl) 
           (AuthorActor (minutesAfter 1000 tm) noIP (fromJust $ maybesignatory sl) (getEmail sl))
      else dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl) 
           (SignatoryActor (minutesAfter 1000 tm) noIP (maybesignatory sl) (getEmail sl) (signatorylinkid sl))

    Just _doc' <- dbQuery $ GetDocumentByDocumentID did
    Right apiReqDocsFilter3 <- jsset "from_date" tms <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter3 <- makeAPIRequest env getDocuments apiReqDocsFilter3
    if docsCount apiRespDocsFilter3 == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter3
      else return ()
    assertEqual ("getDocuments " ++ encode apiReqDocsFilter3 ++ " returned wrong numbers of documents (after MarkDocumentSeen)")
                 1 (docsCount apiRespDocsFilter3)


testNewDocumentWithSpecificExample :: DBEnv -> Assertion
testNewDocumentWithSpecificExample env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",  \"title\":\"test\",  \"type\":3,  \"involved\":[{\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\"}],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  assertBool ("response json does not have document_id") $ isJust $ getJSONField "document_id" rsp

testNewDocumentWithOtherSpecificExample :: DBEnv -> Assertion
testNewDocumentWithOtherSpecificExample env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\"},    {\"email\":\"eric@scrive.com\",\"fstname\":\"Eric\",\"sndname\":\"Nordman\"},    {\"email\":\"mariusz+1@scrive.com\",\"fstname\":\"Mike\",\"sndname\":\"Other\"} ],\"tags\":[    {\"name\":\"year\",\"value\":\"2010\"},    {\"name\":\"month\",\"value\":\"6\"}],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  assertBool ("response json does not have document_id") $ isJust $ getJSONField "document_id" rsp

testEmbedDocumentFrameFromExamples :: DBEnv -> Assertion
testEmbedDocumentFrameFromExamples env = withTestEnvironment env $ do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"email\":\"mariusz@skrivapa.se\",\"company_id\":\"test_company1\",\"document_id\":" ++ show docid1 ++ ",\"location\":\"http://192.168.0.16:8080/Demo/index.jsp\"}"
  rsp <- makeAPIRequest env embeddDocumentFrame rq
  assertBool ("response json does not have link field: " ++ show rsp) $ isJust $ getJSONField "link" rsp

testRemoveDocumentFromExamples :: DBEnv -> Assertion
testRemoveDocumentFromExamples env = withTestEnvironment env $ do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"document_id\":" ++ show docid1 ++ "}"
  _rsp <- makeAPIRequest env removeDocument rq -- the response is empty
  assertBool "Done" True

testSetDocumentTagFromExamples :: DBEnv -> Assertion
testSetDocumentTagFromExamples env = withTestEnvironment env $ do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest env createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"document_id\":" ++ show docid1 ++ ",\"tag\"    : { \"name\": \"client\", \"value\": \"3213123\" }}"
  _rsp <- makeAPIRequest env setDocumentTag rq -- response empty
  assertBool "Done" True

testNewDocumentWithPersonalNr :: DBEnv -> Assertion
testNewDocumentWithPersonalNr env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\"} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [JSObject m]) -> case getJSONField "personalnr" m of
            Just pn -> assertBool ("personal nr is not equal" ++ show pn) $ pn == showJSON "1234"
            _ -> assertBool ("Personal nr field does not exist") False
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentWithCompanyNr :: DBEnv -> Assertion
testNewDocumentWithCompanyNr env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"companynr\" : \"1234\"} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
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
         ,("title" , JSString $ toJSString $ title)
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
     title <- rand 10 $ arbString 1 10
     randomCall $ \fname sname fname2 sname2 -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString $ title)
         ,("type" , JSRational True (dt%1))
         ,("files", JSArray [JSObject $ toJSObject $
                             [("name", JSString $ toJSString "file.pdf")
                             ,("content", JSString $ toJSString testFileBase64Content)
                             ]
                            ])
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
createOrderJSON company author = do
    title <- rand 10 $ arbString 1 10
    randomCall $ \fname sname fname2 sname2 em2 -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString $ title)
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
makeAPIRequest :: DBEnv -> IntegrationAPIFunction Kontra APIResponse -> APIRequestBody -> DB APIResponse
makeAPIRequest env handler req = do
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    rq <- mkRequest POST [("service", inText "test_service"), ("password", inText "test_password") ,("body", inText $ encode req)]
    fmap fst $ runTestKontra rq ctx $ testAPI handler

-- A service to be used with API. We need one to use it.
createTestService :: DB ()
createTestService = do
  pwd <- createPassword "test_password"
  muser <- dbUpdate $ AddUser ("", "") "mariusz@skrivapa.se" (Just pwd) False Nothing Nothing (mkLocaleFromRegion defaultValue)
  case muser of
    Nothing -> error "can't create user"
    Just User{userid} ->
      ignore $ dbUpdate $ CreateService (ServiceID $ BS.pack "test_service") (Just pwd) userid

testNewDocumentExtraFields :: DBEnv -> Assertion
testNewDocumentExtraFields env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
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


testNewDocumentRelations :: DBEnv -> Assertion
testNewDocumentRelations env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"relation\": 1,  \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [JSObject m]) -> case getJSONField "relation" m of
            Just x@(JSRational _ _) -> 
              assertBool ("Relation was not successfully set") $ x == showJSON (1::Int)
            _ -> assertBool ("relation does not exist") False
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc


-- actually, this is not the case right now
_testNewDocumentAttachments :: DBEnv -> Assertion
_testNewDocumentAttachments env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [JSObject m]) -> case getJSONField "attachments" m of
            Just (JSArray _) -> assertSuccess
            a -> assertFailure $ "attachments was wrong, looked for it, got " ++ show a
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentLocale :: DBEnv -> Assertion
testNewDocumentLocale env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ isJust $ getJSONField "region" l
            assertBool ("no language in locale, in " ++ show l) $ isJust $ getJSONField "language" l
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegion :: DBEnv -> Assertion
testNewDocumentSetRegion env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "gb")
            assertBool ("no language in locale, in " ++ show l) $ isJust $ getJSONField "language" l
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegionSE :: DBEnv -> Assertion
testNewDocumentSetRegionSE env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"SE\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "se")
            assertBool ("no language in locale, in " ++ show l) $ isJust $ getJSONField "language" l
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegionGB :: DBEnv -> Assertion
testNewDocumentSetRegionGB env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"GB\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "gb")
            assertBool ("no language in locale, in " ++ show l) $ isJust $ getJSONField "language" l
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegionGBEN :: DBEnv -> Assertion
testNewDocumentSetRegionGBEN env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"en\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "gb")
            assertBool ("no language in locale, in " ++ show l) $ getJSONField "language" l == Just (showJSON "en")
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

testNewDocumentSetRegionGBSV :: DBEnv -> Assertion
testNewDocumentSetRegionGBSV env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "locale" d of
          Just (JSObject l) -> do
            assertBool ("no region in locale, in " ++ show l) $ getJSONField "region" l == Just (showJSON "gb")
            assertBool ("no language in locale, in " ++ show l) $ getJSONField "language" l == Just (showJSON "en")
          _ -> error $ "No locale in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc

-- test template creation
testDocumentCreationFromTemplate :: DBEnv -> Assertion
testDocumentCreationFromTemplate env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":4,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just jsdid -> do
      let did = encode jsdid
      let Ok rq2 = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":3, \"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"}, \"template_id\" : " ++ did ++ "}"
      rsp2 <- makeAPIRequest env createDocument rq2
      case getJSONField "document_id" rsp2 of
        Nothing -> error $ "Could not get document_id from response " ++ show rsp2
        Just d -> do
          res <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", d)]
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


testDocumentCreationFromTemplateContract :: DBEnv -> Assertion
testDocumentCreationFromTemplateContract env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":2,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just jsdid -> do
      let did = encode jsdid
      let Ok rq2 = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1, \"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"}, \"template_id\" : " ++ did ++ "}"
      rsp2 <- makeAPIRequest env createDocument rq2
      case getJSONField "document_id" rsp2 of
        Nothing -> error $ "Could not get document_id from response " ++ show rsp2
        Just d -> do
          res <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", d)]
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

testDocumentCreationFromTemplateOrder :: DBEnv -> Assertion
testDocumentCreationFromTemplateOrder env = withTestEnvironment env $ do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":6,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest env createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just jsdid -> do
      let did = encode jsdid
      let Ok rq2 = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":5, \"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"}, \"template_id\" : " ++ did ++ "}"
      rsp2 <- makeAPIRequest env createDocument rq2
      case getJSONField "document_id" rsp2 of
        Nothing -> error $ "Could not get document_id from response " ++ show rsp2
        Just d -> do
          res <- makeAPIRequest env getDocument $ JSObject $ toJSObject [("document_id", d)]
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
createTemplateJSON company author = do
    title <- rand 10 $ arbString 1 10
    randomCall $ \fname sname -> JSObject $ toJSObject $
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



testCreateFromTemplate :: DBEnv -> Assertion
testCreateFromTemplate env = withTestEnvironment env $ do
  createTestService
  apiReq <- createTemplateJSON "test_company1" "mariusz@skrivapa.se"
  apiRes <- makeAPIRequest env createDocument $ apiReq
  assertBool ("Failed to create doc: " ++ show apiRes) $ not (isError apiRes)    
  let Right did = jsget ["document_id"] (showJSON apiRes)
  apiReq' <- createFromTemplateJSON "test_company1" "mariusz@skrivapa.se" did
  apiRes' <- makeAPIRequest env createDocument apiReq'
  assertBool ("Failed to get doc: " ++ show apiRes') $ not (isError apiRes')  
  let Right did2 = jsget ["document_id"] (showJSON apiRes')
  let Right apiReq2 = (Right jsempty) >>= jsset "document_id" did2
  apiRes2 <- makeAPIRequest env getDocument apiReq2
  assertBool ("Failed to get doc: " ++ show apiRes2) $ not (isError apiRes2)
  assertBool ("doctype is not contract: " ++ show apiRes2) $ (Right (showJSON (1 :: Int))) == jsget ["document", "type"] (showJSON apiRes2)
  
_testLargeExponent :: Assertion
_testLargeExponent = do
  res :: Maybe (Result Int) <- System.Timeout.timeout 1000000 $ evaluate $ decode "7e10000000000"
  case res of
    Nothing -> assertFailure "Should not timeout"
    Just (Error _) -> assertSuccess
    Just (Ok _) -> assertFailure $ "Should not parse bad json but I can't show you what I got (it takes too much memory)"

testFileBase64Content :: String
testFileBase64Content = concat [
    "JVBERi0xLjQKJcOkw7zDtsOfCjIgMCBvYmoKPDwvTGVuZ3RoIDMgMCBSL0ZpbHRlci9Gb"
  , "GF0ZURlY29kZT4+CnN0cmVhbQp4nDPQM1Qo5ypUMABCU0tTPRMFCxMjhaJUhXAthTyuQAUAb"
  , "SUGxgplbmRzdHJlYW0KZW5kb2JqCgozIDAgb2JqCjM4CmVuZG9iagoKNSAwIG9iago8PAo+P"
  , "gplbmRvYmoKCjYgMCBvYmoKPDwvRm9udCA1IDAgUgovUHJvY1NldFsvUERGL1RleHRdCj4+C"
  , "mVuZG9iagoKMSAwIG9iago8PC9UeXBlL1BhZ2UvUGFyZW50IDQgMCBSL1Jlc291cmNlcyA2I"
  , "DAgUi9NZWRpYUJveFswIDAgNTk1IDg0Ml0vR3JvdXA8PC9TL1RyYW5zcGFyZW5jeS9DUy9EZ"
  , "XZpY2VSR0IvSSB0cnVlPj4vQ29udGVudHMgMiAwIFI+PgplbmRvYmoKCjQgMCBvYmoKPDwvV"
  , "HlwZS9QYWdlcwovUmVzb3VyY2VzIDYgMCBSCi9NZWRpYUJveFsgMCAwIDU5NSA4NDIgXQovS"
  , "2lkc1sgMSAwIFIgXQovQ291bnQgMT4+CmVuZG9iagoKNyAwIG9iago8PC9UeXBlL0NhdGFsb"
  , "2cvUGFnZXMgNCAwIFIKL09wZW5BY3Rpb25bMSAwIFIgL1hZWiBudWxsIG51bGwgMF0KL0xhb"
  , "mcocGwtUEwpCj4+CmVuZG9iagoKOCAwIG9iago8PC9BdXRob3I8RkVGRjAwNkQwMDYxMDA3M"
  , "jAwNjkwMDc1MDA3MzAwN0EwMDIwPgovQ3JlYXRvcjxGRUZGMDA1NzAwNzIwMDY5MDA3NDAwN"
  , "jUwMDcyPgovUHJvZHVjZXI8RkVGRjAwNEYwMDcwMDA2NTAwNkUwMDRGMDA2NjAwNjYwMDY5M"
  , "DA2MzAwNjUwMDJFMDA2RjAwNzIwMDY3MDAyMDAwMzMwMDJFMDAzMj4KL0NyZWF0aW9uRGF0Z"
  , "ShEOjIwMTEwNzAxMTIwMTQwKzAyJzAwJyk+PgplbmRvYmoKCnhyZWYKMCA5CjAwMDAwMDAwM"
  , "DAgNjU1MzUgZiAKMDAwMDAwMDIyMiAwMDAwMCBuIAowMDAwMDAwMDE5IDAwMDAwIG4gCjAwM"
  , "DAwMDAxMjggMDAwMDAgbiAKMDAwMDAwMDM2NCAwMDAwMCBuIAowMDAwMDAwMTQ3IDAwMDAwI"
  , "G4gCjAwMDAwMDAxNjkgMDAwMDAgbiAKMDAwMDAwMDQ2MiAwMDAwMCBuIAowMDAwMDAwNTU4I"
  , "DAwMDAwIG4gCnRyYWlsZXIKPDwvU2l6ZSA5L1Jvb3QgNyAwIFIKL0luZm8gOCAwIFIKL0lEI"
  , "FsgPEYxRDY1NzQ3RTIyOEVGNzI0OTQ0OTIwMkFERjI3NDQ2Pgo8RjFENjU3NDdFMjI4RUY3M"
  , "jQ5NDQ5MjAyQURGMjc0NDY+IF0KL0RvY0NoZWNrc3VtIC9DNjgxNEY1QTVCRjgwQjJEOEYwM"
  , "EM4MUQyRDA5RUMxRAo+PgpzdGFydHhyZWYKNzkwCiUlRU9GCg=="
  ]
