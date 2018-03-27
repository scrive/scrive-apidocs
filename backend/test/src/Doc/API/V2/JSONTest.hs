{-# LANGUAGE OverloadedStrings #-}
module Doc.API.V2.JSONTest (
  apiV2JSONTests
-- Only used for V1 ForwardsCompatibilityTest
, runApiJSONTest
, testJSONCtx
) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text, unpack)
import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Context
import DB
import Doc.API.V2.Calls
import Doc.DocumentID
import Doc.SignatoryLinkID ()
import Kontra
import TestingUtil
import TestKontra as T

apiV2JSONTests :: TestEnvSt -> Test
apiV2JSONTests env = testGroup "DocAPIV2JSON"
  [ testThat "Test API v2 'new' and 'get' call response structure" env testDocNewGet
  , testThat "Test API v2 'new', 'update' and 'newfromtemplate' call response structure" env testDocNewFromTemplate
  , testThat "Test API v2 'new' and 'start' call response structure" env testDocNewAndStart
  , testThat "Test API v2 'new', 'update' and 'start' call with metadata on some fields" env testDocNewAndStartWithMetadataInFields
  , testThat "Test API v2 'update' with single empty signatory object" env testDocUpdateEmptySignatory
  , testThat "Test API v2 'update' with new empty signatory object" env testDocUpdateNewSignatory
  , testThat "Test API v2 'update' changing all non read-only fields" env testDocUpdateAll
  , testThat "Test API v2 'update' with invalid radiogroup definition in json fails" env testDocUpdateInvalidRadioGroup
  , testThat "Test API v2 'update' with new signatory and empty fields" env testDocUpdateNewFields
  , testThat "Test API v2 'setfile'" env testDocSetFile
  , testThat "Test API v2 'list' response structure" env testDocList
  , testThat "Test API v2 'removepages' with placements on many diffferent pages" env testDocRemovePages
  ]

testJSONCtx :: TestEnv Context
testJSONCtx = do
  (Just user)  <- addNewUser "BobTest" "JonesTest" "test@scrive.com"
  (set ctxmaybeuser (Just user)) <$> mkContext def

runApiJSONTest :: Context          -- ^ Context to run the test in
               -> Method           -- ^ HTTP Method to use for API Call
               -> Kontra Response  -- ^ The API call to use
               -> [(String,Input)] -- ^ List of API call parameters
               -> Int              -- ^ Expected response code
               -> FilePath         -- ^ FilePath to JSON file to match against
               -> TestEnv (DocumentID, Value)
runApiJSONTest ctx httpMethod apiCall httpHeaders expectedRsCode jsonFile = do
  req <- mkRequestWithHeaders httpMethod httpHeaders []
  (res,_) <- runTestKontra req ctx $ apiCall
  assertEqual ("We should get a " ++ show expectedRsCode ++ " response") expectedRsCode (rsCode res)
  testJSONWith jsonFile (rsBody res)
  let Just docJSON = decode (rsBody res) :: Maybe Value
      Object docObj = docJSON
      Just (String didS) = H.lookup "id" docObj
      Just did = maybeRead $ unpack didS
  return (did, docJSON)

runApiTest :: Context          -- ^ Context to run the test in
               -> Method           -- ^ HTTP Method to use for API Call
               -> Kontra Response  -- ^ The API call to use
               -> [(String,Input)] -- ^ List of API call parameters
               -> Int              -- ^ Expected response code
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
  _ <- runApiJSONTest ctx POST (docApiV2Get did_1) [] 200 jsonFP_new_file

  -- File and saved = true
  let rq_new_2_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf")
                        , ("saved", inText "true")
                        ]
  (did_2,_) <- runApiJSONTest ctx POST docApiV2New rq_new_2_params 201 jsonFP_new_file_saved
  _ <- runApiJSONTest ctx POST (docApiV2Get did_2) [] 200 jsonFP_new_file_saved

  -- File and no saved parameter (should default to true)
  let rq_new_2'_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf")]
  (did_2',_) <- runApiJSONTest ctx POST docApiV2New rq_new_2'_params 201 jsonFP_new_file_saved
  _ <- runApiJSONTest ctx POST (docApiV2Get did_2') [] 200 jsonFP_new_file_saved

  -- No file and no saved parameters
  (did_3,_) <- runApiJSONTest ctx POST docApiV2New [] 201 jsonFP_new_no_params
  _ <- runApiJSONTest ctx POST (docApiV2Get did_3) [] 200 jsonFP_new_no_params

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
  _ <- runApiJSONTest ctx POST (docApiV2NewFromTemplate didTemplate) [] rq_newfromtemplate_code rq_newfromtemplate_json

  return ()

testDocNewAndStart :: TestEnv ()
testDocNewAndStart = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved
  _ <- runApiJSONTest ctx POST (docApiV2Start did) [] 200 $ inTestDir "json/api_v2/test-DocNewAndStart.json"
  return ()

testDocNewAndStartWithMetadataInFields :: TestEnv ()
testDocNewAndStartWithMetadataInFields = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved
  documentWithMetaInFields <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-update-with-metatata-in-fields.json"
  req <- mkRequestWithHeaders POST [ ("document", inTextBS documentWithMetaInFields) ] []
  (_,_) <- runTestKontra req ctx $ docApiV2Update did
  _ <- runApiJSONTest ctx POST (docApiV2Start did) [] 200 $ inTestDir "json/api_v2/test-DocStartedWithMetadataInFields.json"
  return ()


testDocUpdateEmptySignatory :: TestEnv ()
testDocUpdateEmptySignatory = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  update1SigBS <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-update-1emptysig.json"
  let rq_update_params = [ ("document", inTextBS update1SigBS) ]
      rq_update_json = inTestDir "json/api_v2/test-DocUpdateEmptySignatory.json"
  _ <- runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  return ()

testDocUpdateNewSignatory :: TestEnv ()
testDocUpdateNewSignatory = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  update2SigBS <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-update-2emptysig.json"
  let rq_update_params = [ ("document", inTextBS update2SigBS) ]
      rq_update_json = inTestDir "json/api_v2/test-DocUpdateNewSignatory.json"
  _ <- runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  return ()

testDocUpdateAll :: TestEnv ()
testDocUpdateAll = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  updateAllBS <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-update-all.json"
  let rq_update_params = [ ("document", inTextBS updateAllBS) ]
      rq_update_json = inTestDir "json/api_v2/test-DocUpdateAll.json"
  _ <- runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  return ()

testDocUpdateInvalidRadioGroup :: TestEnv ()
testDocUpdateInvalidRadioGroup = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  updateAllBS1 <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-update-invalid-radiogroup-1.json"
  let rq_update_params1 = [ ("document", inTextBS updateAllBS1) ]
  runApiTest ctx POST (docApiV2Update did) rq_update_params1 400

  updateAllBS2 <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-update-invalid-radiogroup-2.json"
  let rq_update_params2 = [ ("document", inTextBS updateAllBS2) ]
  runApiTest ctx POST (docApiV2Update did) rq_update_params2 400

  return ()


testDocUpdateNewFields :: TestEnv ()
testDocUpdateNewFields = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  updateNewFieldsBS <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-update-fields.json"
  let rq_update_params = [ ("document", inTextBS updateNewFieldsBS) ]
      rq_update_json = inTestDir "json/api_v2/test-DocUpdateNewFields.json"
  _ <- runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  return ()

testDocSetFile :: TestEnv ()
testDocSetFile = do
  ctx <- testJSONCtx
  (did,_) <- runApiJSONTest ctx POST docApiV2New [] 201 jsonFP_new_no_params

  let rq_setfile1_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  _ <- runApiJSONTest ctx POST (docApiV2SetFile did) rq_setfile1_params 200 jsonFP_new_file_saved

  let rq_setfile2_params = [ ("file", inFile $ inTestDir "pdfs/simple-rotate-90.pdf") ]
      rq_setfile2_json = inTestDir "json/api_v2/test-DocSetFile2.json"
  _ <- runApiJSONTest ctx POST (docApiV2SetFile did) rq_setfile2_params 200 rq_setfile2_json

  return ()

testDocList :: TestEnv ()
testDocList = do
  ctx <- testJSONCtx

  reqEmpty <- mkRequestWithHeaders GET [("offset", inText "0")] []
  (resEmpty,_) <- runTestKontra reqEmpty ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resEmpty)
  testJSONWith (inTestDir "json/api_v2/test-DocListEmpty.json") (rsBody resEmpty)

  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  _ <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  reqOne <- mkRequestWithHeaders GET [("offset", inText "0")] []
  (resOne,_) <- runTestKontra reqOne ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resOne)
  testJSONWith (inTestDir "json/api_v2/test-DocListOne.json") (rsBody resOne)

  reqFilterPrep <- mkRequestWithHeaders GET [("offset", inText "0")
                                            ,("filter", inText "[{\"filter_by\":\"status\",\"statuses\": [\"preparation\"]}]")
                                            ] []
  (resFilterPrep,_) <- runTestKontra reqFilterPrep ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resFilterPrep)
  testJSONWith (inTestDir "json/api_v2/test-DocListOne.json") (rsBody resFilterPrep)

  reqFilterPending <- mkRequestWithHeaders GET [("offset", inText "0")
                                               ,("filter", inText "[{\"filter_by\":\"status\",\"statuses\": [\"pending\"]}]")
                                               ] []
  (resFilterPending,_) <- runTestKontra reqFilterPending ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resFilterPending)
  testJSONWith (inTestDir "json/api_v2/test-DocListEmpty.json") (rsBody resFilterPending)

  allFiltersJSONBS <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-list-all.json"
  let rq_all_filters_param = [ ("filter", inTextBS allFiltersJSONBS) ]
  reqAllFilters <- mkRequestWithHeaders GET rq_all_filters_param []
  (resAllFilters,_) <- runTestKontra reqAllFilters ctx $ docApiV2List
  assertEqual ("We should get a " ++ show 200 ++ " response") 200 (rsCode resAllFilters)

testDocRemovePages :: TestEnv ()
testDocRemovePages = do
  ctx <- testJSONCtx
  let rq_new_params = [ ("file", inFile $ inTestDir "pdfs/50page.pdf") ]
  (did,_) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 (inTestDir "json/api_v2/result-new-50page.json")

  update1SigBS <- liftIO $ B.readFile $ inTestDir "json/api_v2/param-update-removepages.json"
  let rq_update_params = [ ("document", inTextBS update1SigBS) ]
      rq_update_res_json = inTestDir "json/api_v2/test-DocRemovePagesUpdateResult.json"
  _ <- runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_res_json

  let rq_remove_pages_params = [ ("pages", inText "[5,33,44,46]") ]
      rq_remove_pages_res_json = inTestDir "json/api_v2/test-DocRemovePagesAfterRemoval.json"
  _ <- runApiJSONTest ctx POST (docApiV2RemovePages did) rq_remove_pages_params 200 rq_remove_pages_res_json
  return ()

-- Compare JSON sesults from API calls
testJSONWith :: FilePath -> BS.ByteString -> TestEnv ()
testJSONWith fp jsonBS = do
  jsonFileBS <- liftIO $ B.readFile fp
  let Just value    = decode jsonBS
      Just jsonFile = decode jsonFileBS
  assertEqual ("JSON structure and types (including 'null') should match that in " ++ fp)
              (removeValues jsonFile) (removeValues value)
  assertEqual ("JSON structure and values should match if we will remove dynamic values (like documentid or mtime) " ++ fp)
              (removeDynamicValues jsonFile) (removeDynamicValues value)
  return ()

removeValues :: Value -> Value
removeValues (Object m) = Object (H.map removeValues m)
removeValues (Array v)  = Array  (V.map removeValues v)
removeValues (String _) = String ""
removeValues (Number _) = Number 0
removeValues (Bool _)   = Bool False
removeValues Null       = Null

removeDynamicValues :: Value -> Value
removeDynamicValues (Object m) = Object $ H.map removeDynamicValues $ filterOutDynamicKeys m
  where
    filterOutDynamicKeys hm = H.filterWithKey (\k _ -> not $ k `elem` dynamicKeys) hm
    dynamicKeys = ["id", "title", "user_id", "mtime", "ctime", "timeout_time", "object_version", "access_token", "signatory_id", "api_delivery_url"]
removeDynamicValues (Array v)  = Array  (V.map removeDynamicValues v)
removeDynamicValues v = v

setDocKey :: Text -> Value -> Value -> Value
setDocKey k n v = overDocKey k (const n) v

overDocKey :: Text -> (Value -> Value) -> Value -> Value
overDocKey k f (Object doc) = Object $ H.adjust f k doc
overDocKey _ _ v = v
