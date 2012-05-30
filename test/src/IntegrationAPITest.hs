module IntegrationAPITest (integrationAPITests) where

import Control.Applicative
import Happstack.Server
import Test.HUnit (Assertion)
import Test.Framework
import qualified Data.ByteString.Char8 as BS
import TestingUtil
import TestKontra as T
import Kontra (Kontra)
import User.Model
import IPAddress
import Misc
import API.API
import API.Service.Model
import API.IntegrationAPI
import DB
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
import Util.Actor

import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

integrationAPITests :: TestEnvSt -> Test
integrationAPITests env = testGroup "Integration API" [
      testThat "Test separate companies" env testSeparateCompanies
    , testThat "Test creating a offer from template" env testDocumentCreationFromTemplate
    , testThat "Test from state filtering" env testDocumentsFilteringFromState
    , testThat "Test to state filtering" env testDocumentsFilteringToState
    , testThat "Test to date filtering" env testDocumentsFilteringToDate
    , testThat "Test from date filtering" env testDocumentsFilteringFromDate
    , testThat "Test from date filtering with viewing" env testDocumentsFilteringFromDate2
    , testThat "Test send to friend delete scenario" env testFriendDeleteScenario
    , testThat "Test envect to existing service user" env testSignatoryNoCompany
    , testThat "Test creating a contract from template" env testDocumentCreationFromTemplateContract
    , testThat "Test creating an order from template" env testDocumentCreationFromTemplateOrder
    , testThat "Testing if we can create sample document" env testDocumentCreation
    , testThat "Accessing documents created by the API" env testDocumentsAccess
    , testThat "Accessing documents in embedded frame" env testDocumentAccessEmbeddedPage
    , testThat "new_document example" env testNewDocumentWithSpecificExample
    , testThat "new_document other example" env testNewDocumentWithOtherSpecificExample
    , testThat "embed_document_frame exampel" env testEmbedDocumentFrameFromExamples
    , testThat "remove_document example" env testRemoveDocumentFromExamples
    , testThat "set_document_tag example" env testSetDocumentTagFromExamples
    , testThat "personal number gets saved" env testNewDocumentWithPersonalNr
    , testThat "company number gets saved" env testNewDocumentWithCompanyNr
    , testThat "test that extra fields get added" env testNewDocumentExtraFields
    , testThat "test that new document shows locale" env testNewDocumentLocale
    , testThat "test that you can set region (and not language) on new document and it works!" env testNewDocumentSetRegion
    , testThat "test that se works as a region" env testNewDocumentSetRegionSE
    , testThat "test that gb/en works as a locale" env testNewDocumentSetRegionGBEN
    , testThat "test that gb/sv works as a locale, but converts to gb/eb" env testNewDocumentSetRegionGBSV
    , testThat "test that GB works as a region (all caps)" env testNewDocumentSetRegionGB

    , testThat "test create order" env testNewDocumentOrder

    , testThat "Test that you can set the relation for a signatory and read it back" env testNewDocumentRelations
    , testThat "Test that we can create from templates" env testCreateFromTemplate
    
    ]

-- Main tests

-- I am trying to test a problem that Upsales is having where getDocuments is crossing
-- over company boundaries. It must have been some old migration that broke their boundaries
-- because I cannot reproduce it. Anyway, I have fixed it.
testSeparateCompanies :: TestEnv ()
testSeparateCompanies = do
  createTestService
  x1 <- createDocumentJSONFriend "test_company1" "eric@scrive.com" "erik@scrive.com"
  y1 <- createDocumentJSONFriend "test_company2" "erik@scrive.com" "eric@scrive.com"

  djs <- forM [x1,y1] (makeAPIRequest createDocument)

  let dids = catMaybes $ map (maybeRead) $ map (getJSONStringField "document_id") djs
  
  _dcs <- catMaybes <$> mapM (dbQuery . GetDocumentByDocumentID) dids
  let actor = systemActor $ fromMinutes 0
  _docs <- forM dids $ \did -> do
    ed <- dbUpdate (PreparationToPending did  actor)
    case ed of
      Right d -> do
        let Just sl = getAuthorSigLink d
        _ <- dbUpdate $ SetDocumentInviteTime did (fromMinutes 0) actor
        _ <- dbUpdate $ MarkInvitationRead did (signatorylinkid sl)  actor
        _ <- dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl) actor
        _ <- dbUpdate $ (SignDocument did (signatorylinkid sl) (signatorymagichash sl) Nothing actor)
        return $ Just d
      Left _ -> return Nothing

  x1' <- createDocumentJSONFriend "test_company1" "eric@scrive.com" "erik@scrive.com"
  y1' <- createDocumentJSONFriend "test_company2" "erik@scrive.com" "eric@scrive.com"

  djs' <- forM [x1',y1'] (makeAPIRequest createDocument)

  let dids' = catMaybes $ map (maybeRead) $ map (getJSONStringField "document_id") djs'
  
  _dcs <- catMaybes <$> mapM (dbQuery . GetDocumentByDocumentID) dids'
  _docs <- forM dids' $ \did -> do
    ed <- dbUpdate (PreparationToPending did  actor)
    case ed of
      Right d -> do
        let Just sl = getAuthorSigLink d
        _ <- dbUpdate $ SetDocumentInviteTime did (fromMinutes 0) actor
        _ <- dbUpdate $ MarkInvitationRead did (signatorylinkid sl)  actor
        _ <- dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl) actor
        _ <- dbUpdate $ (SignDocument did (signatorylinkid sl) (signatorymagichash sl) Nothing actor)
        return $ Just d
      Left _ -> return Nothing

  apiReqDocs1 <- getDocumentsJSONC "test_company1"
  apiRespDocs1 <- makeAPIRequest getDocuments $ apiReqDocs1
  assertBool ("Two documents were created by eric@scrive.com but " ++ (show $ docsCount apiRespDocs1) ++ " were found") $ (docsCount apiRespDocs1) == 2
  apiReqDocs2 <- getDocumentsJSONC "test_company2"
  apiRespDocs2 <- makeAPIRequest getDocuments $ apiReqDocs2
  assertBool ("Two documents were created by erik@scrive.com but " ++ (show $ docsCount apiRespDocs2) ++ " were found") $ (docsCount apiRespDocs2) == 2
  
  

testFriendDeleteScenario :: TestEnv ()
testFriendDeleteScenario = do
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

testSignatoryNoCompany :: TestEnv ()
testSignatoryNoCompany = do
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


testNewDocumentOrder :: TestEnv ()
testNewDocumentOrder = do
  createTestService
  apiReq <- createOrderJSON "test_company1" "mariusz@skrivapa.se"
  apiRes <- makeAPIRequest createDocument $ apiReq
  assertBool ("Failed to create doc: " ++ show apiRes) $ not (isError apiRes)
  let Right did = jsget ["document_id"] (showJSON apiRes)
  let Right apiReq2 = jsset "document_id" did jsempty
  apiRes2 <- makeAPIRequest getDocument apiReq2
  assertBool ("Failed to get doc: " ++ show apiRes2) $ not (isError apiRes2)
  assertBool ("doctype is not order: " ++ show apiRes2) $ (Right (showJSON (5 :: Int))) == jsget ["document", "type"] (showJSON apiRes2)
  --let ar1 = (jsget "relation" $ fromRight $ jsgetA 0 $ fromRight $ jsget "involved"  (showJSON apiReq)) | No idea what the problem could be
  --let ar2 = (jsget "relation" $ fromRight $ jsgetA 0 $ fromRight $ jsget "involved" $ fromRight $ jsget "document" (showJSON apiRes2))
  --assertBool ("relation for author is as expected " ++ show (ar1,ar2)) (ar1 == ar2 || (isLeft ar1 && ar2 == Right (JSRational False (5%1))))
  
testDocumentCreation :: TestEnv ()
testDocumentCreation = do
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

testDocumentsAccess :: TestEnv ()
testDocumentsAccess = do
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


testDocumentAccessEmbeddedPage :: TestEnv ()
testDocumentAccessEmbeddedPage = do
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

-- filtering based on status
testDocumentsFilteringToState :: TestEnv ()
testDocumentsFilteringToState = do
    createTestService
    _ <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest getDocuments $ apiReqDocs3
    assertBool ("Should have 1 document but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 1
    Right apiReqDocsFilter <- jsset "to_state" (-1 :: Int) <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest getDocuments apiReqDocsFilter
    assertBool ("All documents should be filtered out but " ++ (show $ docsCount apiRespDocsFilter) ++ " were found") $ (docsCount apiRespDocsFilter) == 0
    Right apiReqDocsFilter2 <- jsset "to_state" (100 :: Int) <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter2 <- makeAPIRequest getDocuments apiReqDocsFilter2
    assertBool ("should have 1 doc but " ++ (show $ docsCount apiRespDocsFilter2) ++ " were found") $ (docsCount apiRespDocsFilter2) == 1

testDocumentsFilteringFromState :: TestEnv ()
testDocumentsFilteringFromState = do
    createTestService
    _ <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest getDocuments $ apiReqDocs3
    assertBool ("Should have 1 document but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 1
    Right apiReqDocsFilter <- jsset "from_state" (20 :: Int) <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest getDocuments apiReqDocsFilter
    if docsCount apiRespDocsFilter == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter
      else return ()
    assertBool ("All documents should be filtered out but " ++ (show $ docsCount apiRespDocsFilter) ++ " were found") $ (docsCount apiRespDocsFilter) == 0

    Right apiReqDocsFilter2 <- jsset "from_state" (0 :: Int) <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter2 <- makeAPIRequest getDocuments apiReqDocsFilter2
    if docsCount apiRespDocsFilter2 == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter2
      else return ()
    assertBool ("should have 1 doc but " ++ (show $ docsCount apiRespDocsFilter2) ++ " were found") $ (docsCount apiRespDocsFilter2) == 1


testDocumentsFilteringToDate :: TestEnv ()
testDocumentsFilteringToDate = do
    createTestService
    _ <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest getDocuments $ apiReqDocs3
    assertBool ("Should have 1 document but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 1
    Right apiReqDocsFilter <- jsset "to_date" "2005-01-01 00:00:00" <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest getDocuments apiReqDocsFilter
    assertBool ("All documents should be filtered out but " ++ (show $ docsCount apiRespDocsFilter) ++ " were found") $ (docsCount apiRespDocsFilter) == 0
    ctxtime <- getMinutesTime
    let tm = minutesAfter 10 ctxtime
        tms = showMinutesTimeForAPI tm
    Right apiReqDocsFilter2 <- jsset "to_date" tms <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    
    apiRespDocsFilter2 <- makeAPIRequest getDocuments apiReqDocsFilter2
    assertBool ("should have 1 document but " ++ (show $ docsCount apiRespDocsFilter2) ++ " were found") $ (docsCount apiRespDocsFilter2) == 1

testDocumentsFilteringFromDate :: TestEnv ()
testDocumentsFilteringFromDate = do
    createTestService
    _ <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest getDocuments $ apiReqDocs3
    assertBool ("Should have 1 document but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 1
    ctxtime <- getMinutesTime
    let tm = minutesAfter 10 ctxtime
        tms = showMinutesTimeForAPI tm
        tmbefore = showMinutesTimeForAPI $ minutesBefore 10 ctxtime
    Right apiReqDocsFilter <- jsset "from_date" tms <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest getDocuments apiReqDocsFilter
    if docsCount apiRespDocsFilter == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter
      else return ()
    assertBool ("All documents should be filtered out but " ++ (show $ docsCount apiRespDocsFilter) ++ " were found") $ (docsCount apiRespDocsFilter) == 0

    Right apiReqDocsFilter2 <- jsset "from_date" tmbefore <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter2 <- makeAPIRequest getDocuments apiReqDocsFilter2
    if docsCount apiRespDocsFilter2 == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter2
      else return ()
    assertBool ("Should be one document but got " ++ (show $ docsCount apiRespDocsFilter2) ++ " were found") $ (docsCount apiRespDocsFilter2) == 1

testDocumentsFilteringFromDate2 :: TestEnv ()
testDocumentsFilteringFromDate2 = do
    createTestService
    apiresult <- makeAPIRequest createDocument =<< createDocumentJSONFriend "test_company1" "mariusz@skrivapa.se" "eric@scrive.com"
    let dids = getJSONStringField "document_id" apiresult
    let mdid = maybeRead dids
        Just did = mdid
    assertBool ("Document created successfully " ++ dids) (isJust mdid)
    apiReqDocs3 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest getDocuments $ apiReqDocs3
    assertEqual ("getDocuments " ++ show apiReqDocs3 ++ " returned docs") 1 (docsCount apiRespDocs3)
    ctxtime <- getMinutesTime
    let tm = minutesAfter 10 ctxtime
        tms = showMinutesTimeForAPI tm
        tmbefore = showMinutesTimeForAPI $ minutesBefore 10 ctxtime    
    
    Right apiReqDocsFilter <- jsset "from_date" tms <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter <- makeAPIRequest getDocuments apiReqDocsFilter
    if docsCount apiRespDocsFilter == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter
      else return ()
    assertEqual ("getDocuments " ++ encode apiReqDocsFilter ++ " returned wrong number of documents")
                 0 (docsCount apiRespDocsFilter)

    Right apiReqDocsFilter2 <- jsset "from_date" tmbefore <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter2 <- makeAPIRequest getDocuments apiReqDocsFilter2
    if docsCount apiRespDocsFilter2 == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter2
      else return ()

    assertEqual ("getDocuments " ++ encode apiReqDocsFilter2 ++ " returned wrong numbers of documents")
                  1 (docsCount apiRespDocsFilter2)
    
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    _ <- dbUpdate $ PreparationToPending did (systemActor ctxtime)

    _ <- forM (documentsignatorylinks doc) $ \sl ->
      if isAuthor sl 
      then dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl) 
           (authorActor (minutesAfter 10 tm) noIP (fromJust $ maybesignatory sl) (getEmail sl))
      else dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl) 
           (signatoryActor (minutesAfter 10 tm) noIP (maybesignatory sl) (getEmail sl) (signatorylinkid sl))

    Just _doc' <- dbQuery $ GetDocumentByDocumentID did
    Right apiReqDocsFilter3 <- jsset "from_date" tms <$> getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocsFilter3 <- makeAPIRequest getDocuments apiReqDocsFilter3
    if docsCount apiRespDocsFilter3 == -1 
      then Log.debug $ "got a weird response: " ++ show apiRespDocsFilter3
      else return ()
    assertEqual ("getDocuments " ++ encode apiReqDocsFilter3 ++ " returned wrong numbers of documents (after MarkDocumentSeen)")
                 1 (docsCount apiRespDocsFilter3)


testNewDocumentWithSpecificExample :: TestEnv ()
testNewDocumentWithSpecificExample = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",  \"title\":\"test\",  \"type\":3,  \"involved\":[{\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\"}],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest createDocument rq
  assertBool ("response json does not have document_id") $ isJust $ getJSONField "document_id" rsp

testNewDocumentWithOtherSpecificExample :: TestEnv ()
testNewDocumentWithOtherSpecificExample = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\"},    {\"email\":\"eric@scrive.com\",\"fstname\":\"Eric\",\"sndname\":\"Nordman\"},    {\"email\":\"mariusz+1@scrive.com\",\"fstname\":\"Mike\",\"sndname\":\"Other\"} ],\"tags\":[    {\"name\":\"year\",\"value\":\"2010\"},    {\"name\":\"month\",\"value\":\"6\"}],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest createDocument rq
  assertBool ("response json does not have document_id") $ isJust $ getJSONField "document_id" rsp

testEmbedDocumentFrameFromExamples :: TestEnv ()
testEmbedDocumentFrameFromExamples = do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"email\":\"mariusz@skrivapa.se\",\"company_id\":\"test_company1\",\"document_id\":" ++ show docid1 ++ ",\"location\":\"http://192.168.0.16:8080/Demo/index.jsp\"}"
  rsp <- makeAPIRequest embeddDocumentFrame rq
  assertBool ("response json does not have link field: " ++ show rsp) $ isJust $ getJSONField "link" rsp

testRemoveDocumentFromExamples :: TestEnv ()
testRemoveDocumentFromExamples = do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"document_id\":" ++ show docid1 ++ "}"
  _rsp <- makeAPIRequest removeDocument rq -- the response is empty
  assertBool "Done" True

testSetDocumentTagFromExamples :: TestEnv ()
testSetDocumentTagFromExamples = do
  createTestService
  docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
  let Ok rq = decode $ "{\"document_id\":" ++ show docid1 ++ ",\"tag\"    : { \"name\": \"client\", \"value\": \"3213123\" }}"
  _rsp <- makeAPIRequest setDocumentTag rq -- response empty
  assertBool "Done" True

testNewDocumentWithPersonalNr :: TestEnv ()
testNewDocumentWithPersonalNr = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\"} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

testNewDocumentWithCompanyNr :: TestEnv ()
testNewDocumentWithCompanyNr = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"companynr\" : \"1234\"} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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
createDocumentJSON :: String -> String -> TestEnv JSValue
createDocumentJSON company author = do
     dt <- rand 10 $  elements [1,3,5]
     title <- rand 10 $ arbString 1 10
     randomCall $ \fname sname -> JSObject $ toJSObject $
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
        
createDocumentJSONFriend :: String -> String -> String -> TestEnv JSValue
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
                                      ("email",   JSString $ toJSString author)
                                    ],
                                  JSObject $ toJSObject $
                                  [ ("fstname", JSString $ toJSString fname2),
                                      ("sndname", JSString $ toJSString sname2),
                                      ("email",   JSString $ toJSString friend)
                                    ]

                                ]
        )]

_createDocumentJSONWithCompany :: String -> String -> Int -> String -> Int -> TestEnv JSValue
_createDocumentJSONWithCompany company author c1 friend c2 = do
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
                                      ("companynr", JSString $ toJSString $ show c1)
                                    ],
                                  JSObject $ toJSObject $
                                  [ ("fstname", JSString $ toJSString fname2),
                                      ("sndname", JSString $ toJSString sname2),
                                      ("email",   JSString $ toJSString friend),
                                      ("companynr", JSString $ toJSString $ show c2)
                                    ]

                                ]
        )]


createOrderJSON :: String -> String -> TestEnv JSValue
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
        

getDocumentsJSON :: String -> String -> TestEnv JSValue
getDocumentsJSON company email = randomCall $ JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("email" , JSString $ toJSString  email)
        ]

getDocumentsJSONC :: String -> TestEnv JSValue
getDocumentsJSONC company = randomCall $ JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
        ]

getEmbedDocumentaJSON :: String -> String -> String -> TestEnv JSValue
getEmbedDocumentaJSON  documentid company email = randomCall $ JSObject $ toJSObject $
        [ ("document_id", JSString $ toJSString documentid)
         ,("company_id", JSString $ toJSString company)
         ,("email" , JSString $ toJSString  email)
        ]

-- Making requests
makeAPIRequest :: IntegrationAPIFunction Kontra APIResponse -> APIRequestBody -> TestEnv APIResponse
makeAPIRequest handler req = do
    ctx <- mkContext (mkLocaleFromRegion defaultValue)
    rq <- mkRequest POST [("service", inText "test_service"), ("password", inText "test_password") ,("body", inText $ encode req)]
    fmap fst $ runTestKontra rq ctx $ testAPI handler

-- A service to be used with API. We need one to use it.
createTestService :: TestEnv ()
createTestService = do
  pwd <- createPassword "test_password"
  muser <- dbUpdate $ AddUser ("", "") "mariusz@skrivapa.se" (Just pwd) False Nothing Nothing (mkLocaleFromRegion defaultValue)
  case muser of
    Nothing -> error "can't create user"
    Just User{userid} ->
      void $ dbUpdate $ CreateService (ServiceID $ BS.pack "test_service") (Just pwd) userid

testNewDocumentExtraFields :: TestEnv ()
testNewDocumentExtraFields = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
  rsp <- makeAPIRequest createDocument rq
  case getJSONField "document_id" rsp of
    Nothing -> error $ "Could not get document_id from response " ++ show rsp
    Just did -> do
      doc <- makeAPIRequest getDocument $ JSObject $ toJSObject [("document_id", did)]
      case getJSONField "document" doc of
        Just (JSObject d) -> case getJSONField "involved" d of
          Just (JSArray [JSObject m]) -> case getJSONField "fields" m of
            Just (JSArray [JSObject prs1, JSObject prs2]) -> do
              assertEqual ("name of first field was not prs1, in " ++ encode prs1) (Just (showJSON "prs1")) (getJSONField "name" prs1)
              assertEqual ("value of first field was not val1, in " ++ encode prs1) (Just (showJSON "val1")) (getJSONField "value" prs1)
              assertEqual ("name of second field was not prs2, in " ++ encode prs2) (Just (showJSON "prs2")) (getJSONField "name" prs2)
              assertEqual ("value of second field was not val2, in " ++ encode prs2) (Just (showJSON "val2")) (getJSONField "value" prs2)
            _ -> assertBool ("fields does not exist") False
          _ -> error $ "No involved in document: " ++ show d
        _ -> error $ "No document in return: " ++ show doc


testNewDocumentRelations :: TestEnv ()
testNewDocumentRelations = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"relation\": 1,  \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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
_testNewDocumentAttachments :: TestEnv ()
_testNewDocumentAttachments = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

testNewDocumentLocale :: TestEnv ()
testNewDocumentLocale = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ],  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

testNewDocumentSetRegion :: TestEnv ()
testNewDocumentSetRegion = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

testNewDocumentSetRegionSE :: TestEnv ()
testNewDocumentSetRegionSE = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"SE\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

testNewDocumentSetRegionGB :: TestEnv ()
testNewDocumentSetRegionGB = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"GB\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

testNewDocumentSetRegionGBEN :: TestEnv ()
testNewDocumentSetRegionGBEN = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"en\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

testNewDocumentSetRegionGBSV :: TestEnv ()
testNewDocumentSetRegionGBSV = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":1,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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
testDocumentCreationFromTemplate :: TestEnv ()
testDocumentCreationFromTemplate = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":4,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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


testDocumentCreationFromTemplateContract :: TestEnv ()
testDocumentCreationFromTemplateContract = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":2,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

testDocumentCreationFromTemplateOrder :: TestEnv ()
testDocumentCreationFromTemplateOrder = do
  createTestService
  let Ok rq = decode $ "{\"company_id\":\"Scrive\",\"title\":\"test_complex\",\"type\":6,\"involved\":[    {\"email\":\"mariusz@scrive.com\",\"fstname\":\"Mariusz\",\"sndname\":\"Rak\", \"personalnr\" : \"1234\", \"fields\" : [{\"name\" : \"prs1\", \"value\" : \"val1\"}, {\"name\" : \"prs2\", \"value\" : \"val2\"}]} ], \"locale\" : {\"region\" : \"gb\", \"language\" : \"sv\"},  \"files\": [{\"name\":\"Empty.pdf\",\"content\":\"" ++ testFileBase64Content ++ "\"}]}"
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

  

createTemplateJSON :: String -> String -> TestEnv JSValue
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


createFromTemplateJSON :: String -> String -> JSValue -> TestEnv JSValue
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



testCreateFromTemplate :: TestEnv ()
testCreateFromTemplate = do
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
