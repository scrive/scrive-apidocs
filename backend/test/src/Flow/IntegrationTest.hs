{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Flow.IntegrationTest where

import Test.Framework
import Text.RawString.QQ
import qualified Data.Map as Map

import Flow.Api
import Flow.Client
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.TestUtil
import TestEnvSt.Internal
import TestingUtil hiding (assertRight)
import TestKontra

tests :: TestEnvSt -> Test
tests env = testGroup
  "Integration"
  [ testThat "Template CRUD happy path" env testTemplateHappyCrud
  , testThat "From zero to instance"    env testZeroToInstance
  , testThat "List template endpoint"   env testTemplateListEndpoint
  , testThat "List instance endpoint"   env testInstanceListEndpoint
  ]

testTemplateHappyCrud :: TestEnv ()
testTemplateHappyCrud = do
  user  <- instantiateRandomUser
  oauth <- getToken (user ^. #id)
  let ApiClient {..} = mkApiClient (Left oauth) (Nothing, Nothing)
  env <- getEnv

  -- TODO nicer check
  let createTemplateData = CreateTemplate "name" "process"
  template1 <- assertRight "create response" . request env $ createTemplate
    createTemplateData

  let tid = id (template1 :: GetCreateTemplate)
  template2 <- assertRight "get response" . request env $ getTemplate tid

  let patchTemplateData = PatchTemplate (Just "new name") (Just "new process")
  template3 <- assertRight "patch response" . request env $ patchTemplate
    tid
    patchTemplateData
  assertEqual "patch response equality"
              (template2 :: GetTemplate) { name = "new name", process = "new process" }
              template3

  void . assertRight "deleteResponse" . request env $ deleteTemplate tid

process1 :: Process
process1 = Process [r|
dsl-version: "1"
stages:
  - initial:
      actions: []
      expect:
        approved-by:
          users: [approver1]
          documents: [doc1]
  - get-data:
      actions:
        - notify:
            users: [party1, party2]
            message: get-data
      expect:
        received-data:
          fields: [first-name]
          users: [party1, party2]
          documents: [doc1, doc2]
|]

testZeroToInstance :: TestEnv ()
testZeroToInstance = do
  user  <- instantiateRandomUser
  oauth <- getToken (user ^. #id)
  let ApiClient {..} = mkApiClient (Left oauth) (Nothing, Nothing)
  env <- getEnv

  let createTemplateData = CreateTemplate "name" process1
  template1 <- assertRight "create template" . request env $ createTemplate
    createTemplateData
  let tid = id (template1 :: GetCreateTemplate)

  void . assertRight "commit template response" . request env $ commitTemplate tid
  void . assertRight "validate response" . request env $ validateTemplate process1

  startTemplateResponse1 <-
    assertRight "start template response" . request env $ startTemplate
      tid
      (mapping (user ^. #id))

  let iid = id (startTemplateResponse1 :: StartTemplate)

  instance2 <- assertRight "get instance" . request env $ getInstance iid
  assertEqual "get after start" iid $ id (instance2 :: GetInstance)

  -- TODO: Fix the test below (need to call instanceOverviewMagicHash first to get the auth cookies)
  -- instanceView <- assertRight "view instance response" . request env $ getInstanceView iid
  -- assertEqual "view instance: id in response" iid $ id (instanceView :: GetInstanceView)

  pure () -- Makes brittany happy

  where
    mapping uid = InstanceToTemplateMapping { documents = Map.empty
                                            , users = Map.fromList [("approver1", uid)]
                                            , messages = Map.empty
                                            }

{-

  , commitTemplate   :: TemplateId -> ClientM NoContent
  , startTemplate    :: TemplateId -> InstanceToTemplateMapping -> ClientM GetInstance
  , getInstance      :: InstanceId -> ClientM GetInstance
  , validateTemplate :: FlowDSL -> ClientM [ValidationError]

--}

testTemplateListEndpoint :: TestEnv ()
testTemplateListEndpoint = do
  user  <- instantiateRandomUser
  oauth <- getToken $ user ^. #id
  let ApiClient {..}     = mkApiClient (Left oauth) (Nothing, Nothing)
      createTemplateData = CreateTemplate "name" process1
  env <- getEnv

  ts1 <-
    assertRight "template list endpoint works when no templates"
    . request env
    $ listTemplates
  assertBool "first template list call should be empty" $ null ts1

  void . assertRight "create template" . request env $ createTemplate createTemplateData

  ts2 <-
    assertRight "template list endpoint works when 1 template"
    . request env
    $ listTemplates
  assertBool "second template list call should have 1 item" $ length ts2 == 1

  void . assertRight "create template" . request env $ createTemplate createTemplateData

  ts3 <-
    assertRight "template list endpoint works when 2 templates"
    . request env
    $ listTemplates
  assertBool "third template list call should have 2 items" $ length ts3 == 2

testInstanceListEndpoint :: TestEnv ()
testInstanceListEndpoint = do
  user  <- instantiateRandomUser
  oauth <- getToken $ user ^. #id
  let ApiClient {..}     = mkApiClient (Left oauth) (Nothing, Nothing)
      createTemplateData = CreateTemplate "name" process1
  env <- getEnv

  is1 <-
    assertRight "instance list endpoint works when no instances"
    . request env
    $ listInstances
  assertBool "first instance list call should be empty" $ null is1

  template <- assertRight "create template" . request env $ createTemplate
    createTemplateData
  let tid = id (template :: GetCreateTemplate)
  void . assertRight "commit template response" . request env $ commitTemplate tid

  void . assertRight "start template response" . request env $ startTemplate
    tid
    (mapping (user ^. #id))
  is2 <-
    assertRight "instance list endpoint works when 1 instance"
    . request env
    $ listInstances
  assertBool "second instance list call should have 1 item" $ length is2 == 1

  void . assertRight "start template response" . request env $ startTemplate
    tid
    (mapping (user ^. #id))
  is3 <-
    assertRight "instance list endpoint works when 2 instances"
    . request env
    $ listInstances
  assertBool "third instance list call should have 2 items" $ length is3 == 2

  where
    mapping uid = InstanceToTemplateMapping { documents = Map.empty
                                            , users = Map.fromList [("approver1", uid)]
                                            , messages = Map.empty
                                            }
