{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}

module Flow.IntegrationTest where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Text.Lazy (unpack)
import Network.HTTP.Client
import Servant.Client
import Test.Framework
import Text.Pretty.Simple
import Text.RawString.QQ
import qualified Data.Map as Map

import Auth.Model
import DB
import Flow.Api
import Flow.Client
import OAuth.Model
import TestEnvSt.Internal
import TestingUtil hiding (assertRight)
import TestKontra
import User.UserID

tests :: TestEnvSt -> Test
tests env = testGroup
  "Integration"
  [ testThat "Template CRUD happy path" env testTemplateHappyCrud
  , testThat "From zero to instance"    env testZeroToInstance
  , testThat "List template endpoint"   env testListEndpoint
  ]

testTemplateHappyCrud :: TestEnv ()
testTemplateHappyCrud = do
  user  <- instantiateRandomUser
  oauth <- getToken (user ^. #id)
  let TemplateClient {..} = mkTemplateClient oauth
  env <- getEnv

  -- TODO nicer check
  let createTemplateData = CreateTemplate "name" "process"
  template1 <- assertRight "create response" . request env $ createTemplate
    createTemplateData

  {- HLINT ignore "Redundant id" -}
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

assertRight :: String -> TestEnv (Either ClientError b) -> TestEnv b
assertRight msg req = do
  res <- req
  case res of
    Right v   -> pure v
    Left  err -> fail $ msg <> ": " <> unpack (pShow err)

process1 :: Text
process1 = [r|
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
  let TemplateClient {..} = mkTemplateClient oauth
  env <- getEnv

  let createTemplateData = CreateTemplate "name" process1
  template1 <- assertRight "create template" . request env $ createTemplate
    createTemplateData
  {- HLINT ignore "Redundant id" -}
  let tid = id (template1 :: GetCreateTemplate)

  void . assertRight "commit template response" . request env $ commitTemplate tid
  void . assertRight "validate response" . request env $ validateTemplate process1

  startTemplateResponse1 <-
    assertRight "start template response" . request env $ startTemplate
      tid
      (mapping (user ^. #id))

  {- HLINT ignore "Redundant id" -}
  let iid = id (startTemplateResponse1 :: StartTemplate)

  instance2 <- assertRight "get instance" . request env $ getInstance iid
  assertEqual "get after start" iid $ id (instance2 :: GetInstance)

  instanceView <- assertRight "view instance response" . request env $ getInstanceView iid
  assertEqual "view instance: id in response" iid $ id (instanceView :: GetInstanceView)

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

testListEndpoint :: TestEnv ()
testListEndpoint = do
  user  <- instantiateRandomUser
  oauth <- getToken $ user ^. #id
  let TemplateClient {..} = mkTemplateClient oauth
      createTemplateData  = CreateTemplate "name" process1
  env <- getEnv

  ts1 <- assertRight "list endpoint works when no templates" . request env $ listTemplates
  assertBool "first list call should be empty" $ null ts1

  void . assertRight "create template" . request env $ createTemplate createTemplateData

  ts2 <- assertRight "list endpoint works when 1 template" . request env $ listTemplates
  assertBool "second list call should have 1 item" $ length ts2 == 1

  void . assertRight "create template" . request env $ createTemplate createTemplateData

  ts3 <- assertRight "list endpoint works when 2 templates" . request env $ listTemplates
  assertBool "third list call should have 2 items" $ length ts3 == 2

getToken :: UserID -> TestEnv OAuthAuthorization
getToken uid = do
  void . dbUpdate $ CreatePersonalToken uid
  commit
  fmap fromJust . dbQuery $ GetPersonalToken uid

getEnv :: TestEnv ClientEnv
getEnv = do
  TestEnvSt {..} <- ask
  mgr            <- liftIO $ newManager defaultManagerSettings
  url            <- parseBaseUrl "localhost"
  pure . mkClientEnv mgr $ url { baseUrlPort = flowPort }

request :: ClientEnv -> ClientM a -> TestEnv (Either ClientError a)
request env req = liftIO $ runClientM req env
