{-# LANGUAGE OverloadedStrings #-}
module Doc.API.V1.ForwardsCompatibilityTest (apiV1ForwardsCompatibilityTests) where

import Happstack.Server
import Test.Framework

import Doc.API.V1.Calls
import Doc.API.V2.Calls
import Doc.API.V2.JSONTest (runApiJSONTest, testJSONCtx)
import TestingUtil
import TestKontra as T

apiV1ForwardsCompatibilityTests :: TestEnvSt -> Test
apiV1ForwardsCompatibilityTests env = testGroup
  "apiV1ForwardsCompatibilityTests"
  [ testThat "Using API V1 doesn't make unexpected changes in a V2 document"
             env
             testApiV1DoesNotBreakApiV2
  ]

jsonFP_new_file_saved :: FilePath
jsonFP_new_file_saved = inTestDir "json/api_v1/forwards_comp_new_file_saved.json"

testApiV1DoesNotBreakApiV2 :: TestEnv ()
testApiV1DoesNotBreakApiV2 = do
  ctx <- testJSONCtx
  let rq_new_params = [("file", inFile $ inTestDir "pdfs/simple.pdf")]
  (did, _) <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFP_new_file_saved

  updateAllBS <- readTestFile "json/api_v1/forwards_comp_req_update_all.json"
  let rq_update_params = [("document", inTextBS updateAllBS)]
      rq_update_json   = inTestDir "json/api_v1/forwards_comp_res_update_all.json"
  void $ runApiJSONTest ctx POST (docApiV2Update did) rq_update_params 200 rq_update_json

  reqGet        <- mkRequestWithHeaders GET [] []
  (apiV1Doc, _) <- runTestKontra reqGet ctx $ apiCallV1Get did

  reqUpdate     <- mkRequestWithHeaders POST [("json", inTextBS (rsBody apiV1Doc))] []
  (_, _)        <- runTestKontra reqUpdate ctx $ apiCallV1Update did

  -- API v1 DocumentToJson sorts signatory fields when generating JSON
  -- API v2 does not, so results of update calls are different
  let rq_update_json2 = inTestDir "json/api_v1/forwards_comp_res_update_all2.json"
  void $ runApiJSONTest ctx POST (docApiV2Get did) [] 200 rq_update_json2

  return ()
