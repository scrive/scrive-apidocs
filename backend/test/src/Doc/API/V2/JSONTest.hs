{-# LANGUAGE OverloadedStrings #-}
module Doc.API.V2.JSONTest (
  apiV2JSONTests
-- Only used for V1 ForwardsCompatibilityTest
, runApiJSONTest
, testJSONCtx
) where

import Control.Monad.Trans
import Data.Aeson
import Happstack.Server
import Test.Framework
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as H

import Context
import DB
import Doc.API.V2.Calls
import Doc.API.V2.Mock.TestUtils
import Doc.DocumentID
import Doc.SignatoryLinkID ()
import TestingUtil
import TestingUtil.JSON
import TestKontra as T
import User.Lang (defaultLang)
import User.Types.User
import UserGroup.Model
import UserGroup.Types

apiV2JSONTests :: TestEnvSt -> Test
apiV2JSONTests env = testGroup "DocAPIV2JSON"
  [ testThat "Test API v2 'new' and 'get' call response structure"
    env testDocNewGet
  , testThat "Test API v2 'new', 'update' and 'newfromtemplate' \
             \call response structure"
    env testDocNewFromTemplate
  , testThat "Test API v2 'new' and 'start' call response structure"
    env testDocNewAndStart
  , testThat "Test API v2 'new', 'update' and 'start' call \
             \with metadata on some fields"
    env testDocNewAndStartWithMetadataInFields
  , testThat "Test API v2 'update' with single empty signatory object"
    env testDocUpdateEmptySignatory
  , testThat "Test API v2 'update' with new empty signatory object"
    env testDocUpdateNewSignatory
  , testThat "Test API v2 'update' changing all non read-only fields"
    env testDocUpdateAll
  , testThat "Test API v2 'update' with invalid radiogroup definition \
             \in json fails"
    env testDocUpdateInvalidRadioGroup
  , testThat "Test API v2 'update' with new signatory and empty fields"
    env testDocUpdateNewFields
  , testThat "Test API v2 'setfile'"
    env testDocSetFile
  , testThat "Test API v2 'setfile' with base64 encoded param"
    env testDocSetFileB64
  , testThat "Test API v2 'list' response structure using old style list calls"
    env (testDocList False)
  , testThat "Test API v2 'list' response structure using new style list calls"
    env (testDocList True)
  , testThat "Test API v2 'removepages' with placements \
             \on many diffferent pages"
    env testDocRemovePages
  ]

testJSONCtx :: TestEnv Context
testJSONCtx = do
  (Just user)  <- addNewUser "BobTest" "JonesTest" "test@scrive.com"
  (set ctxmaybeuser (Just user)) <$> mkContext defaultLang

runApiJSONTest :: Context             -- ^ Context to run the test in
               -> Method              -- ^ HTTP Method to use for API Call
               -> KontraTest Response -- ^ The API call to use
               -> [(Text,Input)]    -- ^ List of API call parameters
               -> Int                 -- ^ Expected response code
               -> FilePath            -- ^ FilePath to JSON file to match against
               -> TestEnv (DocumentID, Value)
runApiJSONTest ctx httpMethod apiCall httpHeaders expectedRsCode jsonFile = do
  req <- mkRequestWithHeaders httpMethod
    httpHeaders
    []
  (res,_) <- runTestKontra req ctx $ apiCall
  assertEqual ("We should get a " ++ show expectedRsCode ++ " response") expectedRsCode (rsCode res)
  testJSONWith jsonFile (rsBody res)
  let Just docJSON = decode (rsBody res) :: Maybe Value
      Object docObj = docJSON
      Just (String didS) = H.lookup "id" docObj
      Just did = maybeRead didS
  return (did, docJSON)

runApiTest :: Context             -- ^ Context to run the test in
           -> Method              -- ^ HTTP Method to use for API Call
           -> KontraTest Response -- ^ The API call to use
           -> [(Text,Input)]    -- ^ List of API call parameters
           -> Int                 -- ^ Expected response code
           -> TestEnv ()
runApiTest ctx httpMethod apiCall httpHeaders expectedRsCode = do
  req <- mkRequestWithHeaders httpMethod httpHeaders []
  (res,_) <- runTestKontra req ctx $ apiCall
  assertEqual ("We should get a " ++ show expectedRsCode ++ " response") expectedRsCode (rsCode res)

-- FilePath for  common JSONs re-used over and over in these tests
jsonFP_new_file :: FilePath
jsonFP_new_file = inTestDir "json/api_v2/result-new-file-saved-false.json"
jsonFP_new_file_saved :: FilePath
jsonFP_new_file_saved = inTestDir "json/api_v2/result-new-file-saved-true.json"
jsonFP_new_no_params :: FilePath
jsonFP_new_no_params = inTestDir "json/api_v2/result-new-no-params.json"

testDocNewGet :: TestEnv ()
testDocNewGet = do
  ctx <- testJSONCtx

  -- File and saved = false
  let rq_new_1_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf")
                        , ("saved", inText "false")
                        ]
  (did_1,_) <- runApiJSONTest ctx POST docApiV2New rq_new_1_params 201 jsonFP_new_file
  void $ runApiJSONTest ctx POST (docApiV2Get did_1) [] 200 jsonFP_new_file

  -- File and saved = true
  let rq_new_2_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf")
                        , ("saved", inText "true")
                        ]
  (did_2,_) <- runApiJSONTest ctx POST docApiV2New rq_new_2_params 201 jsonFP_new_file_saved
  void $ runApiJSONTest ctx POST (docApiV2Get did_2) [] 200 jsonFP_new_file_saved

  -- File and no saved parameter (should default to true)
  let rq_new_2'_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf")]
  (did_2',_) <- runApiJSONTest ctx POST docApiV2New rq_new_2'_params 201 jsonFP_new_file_saved
  void $ runApiJSONTest ctx POST (docApiV2Get did_2') [] 200 jsonFP_new_file_saved

  -- No file and no saved parameters
  (did_3,_) <- runApiJSONTest ctx POST docApiV2New [] 201 jsonFP_new_no_params
  void $ runApiJSONTest ctx POST (docApiV2Get did_3) [] 200 jsonFP_new_no_params

  return ()

testDocNewFromTemplate :: TestEnv ()
testDocNewFromTemplate = do
  ctx <- testJSONCtx

  let rq_new_1_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf")
                        , ("saved", inText "false")
                        ]
      rq_new_1_code = 201
  (did, docJSON) <- runApiJSONTest ctx POST docApiV2New rq_new_1_params rq_new_1_code jsonFP_new_file

  let value = setDocKey "is_template" (Bool True) docJSON
      docTemplate = encode value

  let rq_update_params = [ ("document", inTextBS docTemplate) ]
      rq_update_code = 200
      rq_update_json = inTestDir "json/api_v2/test-DocNewFromTemplate-update.json"
  (didTemplate, _) <- runApiJSONTest ctx POST (docApiV2Update did) rq_update_params rq_update_code rq_update_json

  let rq_newfromtemplate_code = 201
      rq_newfromtemplate_json = inTestDir "json/api_v2/test-DocNewFromTemplate-newfromtemplate.json"
  void $ runApiJSONTest ctx POST (docApiV2NewFromTemplate didTemplate) [] rq_newfromtemplate_code rq_newfromtemplate_json

  return ()

testDocNewAndStart :: TestEnv ()
testDocNewAndStart = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved
  void $ runApiJSONTest ctx POST (docApiV2Start did) [] 200 $ inTestDir "json/api_v2/test-DocNewAndStart.json"
  return ()

testDocNewAndStartWithMetadataInFields :: TestEnv ()
testDocNewAndStartWithMetadataInFields = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved
  documentWithMetaInFields <- readTestFile "json/api_v2/param-update-with-metatata-in-fields.json"
  req <- mkRequestWithHeaders POST [ ("document", inTextBS documentWithMetaInFields) ] []
  (_,_) <- runTestKontra req ctx $ docApiV2Update did
  void $ runApiJSONTest ctx POST (docApiV2Start did) [] 200 $ inTestDir "json/api_v2/test-DocStartedWithMetadataInFields.json"
  return ()


testDocUpdateEmptySignatory :: TestEnv ()
testDocUpdateEmptySignatory = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  update1SigBS <- readTestFile "json/api_v2/param-update-1emptysig.json"
  let rq_update_params = [ ("document", inTextBS update1SigBS) ]
      rq_update_json = inTestDir "json/api_v2/test-DocUpdateEmptySignatory.json"
  void $ runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  return ()

testDocUpdateNewSignatory :: TestEnv ()
testDocUpdateNewSignatory = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  update2SigBS <- readTestFile "json/api_v2/param-update-2emptysig.json"
  let rq_update_params = [ ("document", inTextBS update2SigBS) ]
      rq_update_json = inTestDir "json/api_v2/test-DocUpdateNewSignatory.json"
  void $ runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  return ()

testDocUpdateAll :: TestEnv ()
testDocUpdateAll = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  updateAllBS <- readTestFile "json/api_v2/param-update-all.json"
  let rq_update_params = [ ("document", inTextBS updateAllBS) ]
      rq_update_json = inTestDir "json/api_v2/test-DocUpdateAll.json"
  void $ runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  return ()

testDocUpdateInvalidRadioGroup :: TestEnv ()
testDocUpdateInvalidRadioGroup = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  updateAllBS1 <- readTestFile "json/api_v2/param-update-invalid-radiogroup-1.json"
  let rq_update_params1 = [ ("document", inTextBS updateAllBS1) ]
  runApiTest ctx POST (docApiV2Update did) rq_update_params1 400

  updateAllBS2 <- readTestFile "json/api_v2/param-update-invalid-radiogroup-2.json"
  let rq_update_params2 = [ ("document", inTextBS updateAllBS2) ]
  runApiTest ctx POST (docApiV2Update did) rq_update_params2 400

  return ()


testDocUpdateNewFields :: TestEnv ()
testDocUpdateNewFields = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  updateNewFieldsBS <- readTestFile "json/api_v2/param-update-fields.json"
  let rq_update_params = [ ("document", inTextBS updateNewFieldsBS) ]
      rq_update_json = inTestDir "json/api_v2/test-DocUpdateNewFields.json"
  void $ runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  return ()

testDocSetFile :: TestEnv ()
testDocSetFile = do
  ctx <- testJSONCtx
  (did,_) <- runApiJSONTest ctx POST docApiV2New [] 201 jsonFP_new_no_params

  let rq_setfile1_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  void $ runApiJSONTest ctx POST (docApiV2SetFile did) rq_setfile1_params 200 jsonFP_new_file_saved

  let rq_setfile2_params = [ ("file", inFile $ inTestDir "pdfs/simple-rotate-90.pdf") ]
      rq_setfile2_json = inTestDir "json/api_v2/test-DocSetFile2.json"
  void $ runApiJSONTest ctx POST (docApiV2SetFile did) rq_setfile2_params 200 rq_setfile2_json

  return ()

testDocSetFileB64 :: TestEnv ()
testDocSetFileB64 = do
  ctx <- testJSONCtx
  (did,_) <- runApiJSONTest ctx POST docApiV2New [] 201 jsonFP_new_no_params
  cont <- liftIO $ BS.readFile $ inTestDir "pdfs/simple.pdf"

  let params = [ ("file", inTextBS $ BSL.fromChunks [B64.encode cont]) ]
  mockDoc <- mockDocTestRequestHelper ctx POST params (docApiV2SetFile did) 200
  assertBool "New document has file now" $ getMockDocHasFile mockDoc
  return ()


testDocList :: Bool -> TestEnv ()
testDocList useFolderListCalls = do
  ctx <- testJSONCtx

  -- test with new list feature as well as old
  let (Just user) = get ctxmaybeuser ctx
      ugid = usergroupid user
  (Just ug) <- dbQuery . UserGroupGet $ ugid
  let new_ugsettings = set ugsUseFolderListCalls useFolderListCalls (fromJust $ get ugSettings ug)

  void $ dbUpdate . UserGroupUpdate $ set ugSettings (Just new_ugsettings) ug

  reqEmpty <- mkRequestWithHeaders GET [("offset", inText "0")] []
  (resEmpty,_) <- runTestKontra reqEmpty ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resEmpty)
  testJSONWith (inTestDir "json/api_v2/test-DocListEmpty.json") (rsBody resEmpty)

  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  void $ runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  reqOne <- mkRequestWithHeaders GET [("offset", inText "0")] []
  (resOne,_) <- runTestKontra reqOne ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resOne)
  let expectedJson = if useFolderListCalls
                     then "test-DocListOne-By-Folder.json"
                     else "test-DocListOne.json"
  testJSONWith (inTestDir ("json/api_v2/" <> expectedJson)) (rsBody resOne)

  reqFilterPrep <- mkRequestWithHeaders GET [("offset", inText "0")
                                            ,("filter", inText "[{\"filter_by\":\"status\",\"statuses\": [\"preparation\"]}]")
                                            ] []
  (resFilterPrep,_) <- runTestKontra reqFilterPrep ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resFilterPrep)
  testJSONWith (inTestDir ("json/api_v2/" <> expectedJson)) (rsBody resFilterPrep)

  reqFilterPending <- mkRequestWithHeaders GET [("offset", inText "0")
                                               ,("filter", inText "[{\"filter_by\":\"status\",\"statuses\": [\"pending\"]}]")
                                               ] []
  (resFilterPending,_) <- runTestKontra reqFilterPending ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resFilterPending)
  testJSONWith (inTestDir "json/api_v2/test-DocListEmpty.json") (rsBody resFilterPending)

  allFiltersJSONBS <- readTestFile "json/api_v2/param-list-all.json"
  let rq_all_filters_param = [ ("filter", inTextBS allFiltersJSONBS) ]
  reqAllFilters <- mkRequestWithHeaders GET rq_all_filters_param []
  (resAllFilters,_) <- runTestKontra reqAllFilters ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resAllFilters)

testDocRemovePages :: TestEnv ()
testDocRemovePages = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/50page.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 (inTestDir "json/api_v2/result-new-50page.json")

  update1SigBS <- readTestFile "json/api_v2/param-update-removepages.json"
  let rq_update_params = [ ("document", inTextBS update1SigBS) ]
      rq_update_res_json = inTestDir "json/api_v2/test-DocRemovePagesUpdateResult.json"
  void $ runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_res_json

  let rq_remove_pages_params = [ ("pages", inText "[5,33,44,46]") ]
      rq_remove_pages_res_json = inTestDir "json/api_v2/test-DocRemovePagesAfterRemoval.json"
  void $ runApiJSONTest ctx POST (docApiV2RemovePages did) rq_remove_pages_params 200 rq_remove_pages_res_json
  return ()

testJSONWith :: FilePath -> BSL.ByteString -> TestEnv ()
testJSONWith = testJSONWithDynamicKeys [ "id"
                                       , "access_token"
                                       , "api_delivery_url"
                                       , "ctime"
                                       , "mtime"
                                       , "object_version"
                                       , "signatory_id"
                                       , "timeout_time"
                                       , "title"
                                       , "user_id"
                                       , "template_id"
                                       ]
