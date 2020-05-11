{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

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

tests :: TestEnvSt -> Test
tests env = testGroup
  "Integration"
  [ testThat "Template CRUD happy path" env testTemplateHappyCrud
  , testThat "From zero to instance"    env testZeroToInstance
  ]

testTemplateHappyCrud :: TestEnv ()
testTemplateHappyCrud = do
  oauth <- getToken
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
  oauth <- getToken
  let TemplateClient {..} = mkTemplateClient oauth
  env <- getEnv

  let createTemplateData = CreateTemplate "name" process1
  template1 <- assertRight "create template" . request env $ createTemplate
    createTemplateData
  {- HLINT ignore "Redundant id" -}
  let tid = id (template1 :: GetCreateTemplate)

  void . assertRight "commit template response" . request env $ commitTemplate tid
  void . assertRight "validate response" . request env $ validateTemplate process1

  instance1 <- assertRight "start template response" . request env $ startTemplate
    tid
    mapping
  {- HLINT ignore "Redundant id" -}
  let iid = id (instance1 :: GetInstance)
  instance2 <- assertRight "get instance" . request env $ getInstnace iid
  assertEqual "get after start" instance1 instance2
  where mapping = InstanceToTemplateMapping Map.empty Map.empty Map.empty


{-

  , commitTemplate   :: TemplateId -> ClientM NoContent
  , startTemplate    :: TemplateId -> InstanceToTemplateMapping -> ClientM GetInstance
  , getInstnace      :: InstanceId -> ClientM GetInstance
  , validateTemplate :: FlowDSL -> ClientM [ValidationError]

--}

getToken :: TestEnv OAuthAuthorization
getToken = do
  user <- instantiateRandomUser

  let uid = user ^. #id
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
