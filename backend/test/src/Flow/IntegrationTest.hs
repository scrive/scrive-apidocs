{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Flow.IntegrationTest where

import Data.Aeson
import Servant.Client
import Test.Framework
import Text.RawString.QQ
import qualified Data.Map as Map

import Auth.Session
import DB
import Doc.Model.Update
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.Api
import Flow.Client
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.TestUtil
import MinutesTime
import TestEnvSt.Internal ()
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import User.Types.User
import Util.Actor
import qualified Auth.Model as AuthModel
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model

tests :: TestEnvSt -> Test
tests env = testGroup
  "Integration"
  [ testThat "Template CRUD happy path" env testTemplateHappyCrud
  , testThat "From zero to instance"    env testZeroToInstance
  , testThat "Instance failure"         env testInstanceFailure
  , testThat "List template endpoint"   env testTemplateListEndpoint
  , testThat "List instance endpoint"   env testInstanceListEndpoint
  ]

testTemplateHappyCrud :: TestEnv ()
testTemplateHappyCrud = do
  user  <- instantiateRandomUser
  oauth <- getToken (user ^. #id)
  let ApiClient {..}     = mkApiClient (Left oauth)

  -- TODO nicer check
  let createTemplateData = CreateTemplate "name" "process"
  template1 <- assertRight "create response" . request $ createTemplate createTemplateData

  let tid = id (template1 :: GetCreateTemplate)
  template2 <- assertRight "get response" . request $ getTemplate tid

  let patchTemplateData = PatchTemplate (Just "new name") (Just "new process")
  template3 <- assertRight "patch response" . request $ patchTemplate tid
                                                                      patchTemplateData
  assertEqual "patch response equality"
              (template2 :: GetTemplate) { name = "new name", process = "new process" }
              template3

  void . assertRight "deleteResponse" . request $ deleteTemplate tid


addDocument :: User -> TestEnv Document
addDocument user = addRandomDocument (rdaDefault user)
  { rdaStatuses    = OneOf [Preparation]
  , rdaTypes       = OneOf [Signable]
  , rdaSignatories = let signatory = OneOf
                           [ [ RSC_IsSignatory
                             , RSC_DeliveryMethodIs EmailDelivery
                             , RSC_AuthToViewIs StandardAuthenticationToView
                             , RSC_AuthToSignIs StandardAuthenticationToSign
                             ]
                           ]
                     in  OneOf [[signatory]]
  }

processZero :: Process
processZero = Process [r|
dsl-version: "0.1.0"
stages:
  - initial:
      actions: []
      expect:
        signed-by:
          users: [signatory]
          documents: [doc1, doc2]
  - notification:
      actions:
        - notify:
            users: [watcher]
            message: was-signed
      expect: {}
|]

testZeroToInstance :: TestEnv ()
testZeroToInstance = do
  user             <- instantiateRandomUser
  oauth            <- getToken (user ^. #id)
  AuthCookies {..} <- AuthModel.insertNewSession "localhost"

  let ac@ApiClient {..} = mkApiClient (Left oauth)

  doc1        <- addDocument user
  now         <- currentTime
  Just doc2id <- dbUpdate
    $ CloneDocumentWithUpdatedAuthor Nothing doc1 (systemActor now) identity

  commit

  let mapping = InstanceKeyValues documents users messages
        where
          documents = Map.fromList [("doc1", documentid doc1), ("doc2", doc2id)]
          users     = Map.fromList
            [("signatory", UserId $ user ^. #id), ("watcher", Email "foo@bar.com")]
          messages = Map.fromList [("was-signed", "Documents were signed")]

  instance1 <- assertRight "start template response"
    $ createInstance ac "name" processZero mapping

  let iid = id (instance1 :: GetInstance)
  keyValues <- Model.selectInstanceKeyValues iid
  assertEqual "key values" mapping keyValues

  instance2 <- assertRight "get instance" . request $ getInstance iid
  assertEqual "get after start" iid $ id (instance2 :: GetInstance)
  assertEqual "links"
              (Map.keys $ mapping ^. #users)
              (Map.keys $ access_links (instance2 :: GetInstance))

  -- View instance as "signatory"
  let ParticipantApiClient {..} =
        mkParticipantApiClient (Just authCookieSession, Just authCookieXToken)
  Model.upsertInstanceSession (cookieSessionID authCookieSession) iid "signatory"
  commit
  instanceView <- assertRight "view instance response" . request $ getInstanceView iid
  assertEqual "view instance: id in response" iid $ id (instanceView :: GetInstanceView)

processFailure :: Process
processFailure = Process [r|
dsl-version: "0.1.0"
stages:
  - initial:
      actions: []
      expect:
        signed-by:
          users: [signatory]
          documents: [doc1]
|]

testInstanceFailure :: TestEnv ()
testInstanceFailure = do
  user  <- instantiateRandomUser
  oauth <- getToken (user ^. #id)
  let ac@ApiClient {..} = mkApiClient (Left oauth)

  doc1 <- addDocument user
  commit

  let mapping = InstanceKeyValues documents users messages
        where
          documents = Map.fromList [("doc1", documentid doc1)]
          users     = Map.fromList [("signatory", UserId $ user ^. #id)]
          messages  = Map.empty

  void $ createInstance ac "name" processFailure mapping
  clientError <- assertLeft "creating second instance"
    $ createInstance ac "name" processFailure mapping
  assert $ hasJsonBody clientError
  where
    isJustObject :: Maybe Value -> Bool
    isJustObject = \case
      Just (Object _) -> True
      _               -> False
    hasJsonBody = \case
      FailureResponse _ resp -> isJustObject . decode $ responseBody resp
      _ -> False


simpleProcess :: Process
simpleProcess = Process [r|
dsl-version: "0.1.0"
stages:
  - initial:
      actions: []
      expect: {}
|]

testTemplateListEndpoint :: TestEnv ()
testTemplateListEndpoint = do
  user  <- instantiateRandomUser
  oauth <- getToken $ user ^. #id
  let ApiClient {..}     = mkApiClient (Left oauth)
      createTemplateData = CreateTemplate "name" "process"

  ts1 <- assertRight "template list endpoint works when no templates"
    $ request listTemplates
  assertBool "first template list call should be empty" $ null ts1

  void . assertRight "create template" . request $ createTemplate createTemplateData

  ts2 <- assertRight "template list endpoint works when 1 template"
    $ request listTemplates
  assertBool "second template list call should have 1 item" $ length ts2 == 1

  void . assertRight "create template" . request $ createTemplate createTemplateData

  ts3 <- assertRight "template list endpoint works when 2 templates"
    $ request listTemplates
  assertBool "third template list call should have 2 items" $ length ts3 == 2

testInstanceListEndpoint :: TestEnv ()
testInstanceListEndpoint = do
  user  <- instantiateRandomUser
  oauth <- getToken $ user ^. #id
  let ApiClient {..}     = mkApiClient (Left oauth)
      createTemplateData = CreateTemplate "name" simpleProcess

  is1 <- assertRight "instance list endpoint works when no instances"
    $ request listInstances
  assertBool "first instance list call should be empty" $ null is1

  template <- assertRight "create template" . request $ createTemplate createTemplateData
  let tid = id (template :: GetCreateTemplate)
  void . assertRight "commit template response" . request $ commitTemplate tid

  void . assertRight "start template response" . request $ startTemplate tid mapping
  is2 <- assertRight "instance list endpoint works when 1 instance"
    $ request listInstances
  assertBool "second instance list call should have 1 item" $ length is2 == 1

  void . assertRight "start template response" . request $ startTemplate tid mapping
  is3 <- assertRight "instance list endpoint works when 2 instances"
    $ request listInstances
  assertBool "third instance list call should have 2 items" $ length is3 == 2
  where mapping = InstanceKeyValues Map.empty Map.empty Map.empty
