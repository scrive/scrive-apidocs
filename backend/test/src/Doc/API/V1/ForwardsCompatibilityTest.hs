{-# LANGUAGE OverloadedStrings #-}
module Doc.API.V1.ForwardsCompatibilityTest (apiV1ForwardsCompatibilityTests) where

import Happstack.Server
import Test.Framework

import AccessControl.Model
  ( AccessControlCreateForUser(..), AccessControlRemoveRole(..)
  )
import AccessControl.Types (AccessRole(..), AccessRoleTarget(..))
import DB.Query (dbUpdate)
import Doc.API.V1.Calls
import Doc.API.V2.Calls
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.JSONTest (runApiJSONTest, testJSONCtx)
import Doc.API.V2.Mock.TestUtils
import TestingUtil
import TestKontra as T
import User.Lang (defaultLang)

apiV1ForwardsCompatibilityTests :: TestEnvSt -> Test
apiV1ForwardsCompatibilityTests env = testGroup
  "apiV1ForwardsCompatibilityTests"
  [ testThat "Using API V1 doesn't make unexpected changes in a V2 document"
             env
             testApiV1DoesNotBreakApiV2
  , testThat
    "Using API V1 to create a document from template when impersonating another user group"
    env
    testDocApiV1FromTemplateImpersonateEID
  , testThat "Using API V1 to ready a document when impersonating another user group"
             env
             testDocApiV1ReadyImpersonateEID
  ]

jsonFpNewFileSaved :: FilePath
jsonFpNewFileSaved = inTestDir "json/api_v1/forwards_comp_new_file_saved.json"

testApiV1DoesNotBreakApiV2 :: TestEnv ()
testApiV1DoesNotBreakApiV2 = do
  ctx <- testJSONCtx
  let rq_new_params = [("file", inFile $ inTestDir "pdfs/simple.pdf")]
  (did, _)    <- runApiJSONTest ctx POST docApiV2New rq_new_params 201 jsonFpNewFileSaved

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

testDocApiV1FromTemplateImpersonateEID :: TestEnv ()
testDocApiV1FromTemplateImpersonateEID = do
  user <- instantiateRandomUser
  let uid = user ^. #id
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  did <- getMockDocId <$> testDocApiV2New' ctx

  -- User group to impersonate
  ug  <- instantiateRandomUserGroup
  let ugid = ug ^. #id

  -- grant impersonate role
  Just (AccessRoleUser roleid _ _) <-
    dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  do  -- set 'user_group_to_impersonate_for_eid' field and make template
    let update =
          "{\"experimental_features\": {\"user_group_to_impersonate_for_eid\":\""
            <> showt ugid
            <> "\"}"
            <> ", \"is_template\" : true}"
    let params = [("document", inText update)]

    mdoc <- mockDocTestRequestHelper ctx POST params (docApiV2Update did) 200
    assertEqual "user_group_to_impersonate_for_eid should match"
                (Just (show ugid))
                (getMockDocUserGroupForEid mdoc)

  -- remove impersonate role again
  void . dbUpdate $ AccessControlRemoveRole roleid

  emptyPOSTRequest <- mkRequestWithHeaders POST [] []
  (resFail, _)     <- runTestKontra emptyPOSTRequest ctx $ apiCallV1CreateFromTemplate did
  assertEqual
    "Starting a document from template impersonating another user group without\
    \ sufficient permissions should fail, even when using the V1 API"
    403
    (rsCode resFail)

  -- grant impersonate role again
  void . dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  (resSucc, _) <- runTestKontra emptyPOSTRequest ctx $ apiCallV1CreateFromTemplate did
  assertEqual
    "Starting a document from template impersonating another user group with\
    \ sufficient permissions should succeed, even when using the V1 API"
    201
    (rsCode resSucc)

testDocApiV1ReadyImpersonateEID :: TestEnv ()
testDocApiV1ReadyImpersonateEID = do
  user <- instantiateRandomUser
  let uid = user ^. #id
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  did <- getMockDocId <$> testDocApiV2New' ctx

  -- User group to impersonate
  ug  <- instantiateRandomUserGroup
  let ugid = ug ^. #id

  -- grant impersonate role
  Just (AccessRoleUser roleid _ _) <-
    dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  do  -- set 'user_group_to_impersonate_for_eid' field
    let update =
          "{\"experimental_features\": {\"user_group_to_impersonate_for_eid\":\""
            <> showt ugid
            <> "\"}}"
    let params = [("document", inText update)]

    mdoc <- mockDocTestRequestHelper ctx POST params (docApiV2Update did) 200
    assertEqual "user_group_to_impersonate_for_eid should match"
                (Just (show ugid))
                (getMockDocUserGroupForEid mdoc)

  -- remove impersonate role again
  void . dbUpdate $ AccessControlRemoveRole roleid

  emptyPOSTRequest <- mkRequestWithHeaders POST [] []
  (resFail, _)     <- runTestKontra emptyPOSTRequest ctx $ apiCallV1Ready did
  assertEqual
    "Readying a document impersonating another user group without sufficient\
    \ permissions should fail, even when using the V1 API"
    403
    (rsCode resFail)

  -- grant impersonate role again
  void . dbUpdate . AccessControlCreateForUser uid $ EidImpersonatorAR ugid

  (resSucc, _) <- runTestKontra emptyPOSTRequest ctx $ apiCallV1Ready did
  assertEqual
    "Readying a document impersonating another user group with sufficient\
    \ permissions should succeed, even when using the V1 API"
    202
    (rsCode resSucc)
