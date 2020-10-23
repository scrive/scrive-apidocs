{-# LANGUAGE QuasiQuotes #-}
module Flow.IntegrationTest.Cancellation (tests) where

import Data.Tuple.Extra
import Happstack.Server
import Test.Framework
import Text.RawString.QQ
import qualified Data.Map as Map

import DB hiding (JSON)
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Model.Query
import Doc.Types.Document
import Doc.Types.SignatoryLink
import Flow.Client
import Flow.Core.Type.Url
import Flow.IntegrationTest.Common
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Model.Types.Internal
import Flow.Names
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.Routes.Api
import Flow.TestUtil
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import User.Lang
import User.Types.User
import Util.HasSomeUserInfo
import qualified Doc.Types.DocumentStatus as DocStatus

-- TODO: When we start sealing docs upon completion of signing, these tests will fail
-- TODO: since we assume that all docs in the flow will become cancelled.
tests :: TestEnvSt -> Test
tests env = testGroup
  "Flow Cancellation Tests"
  [ testThat "Freshly created, unstarted flow can be cancelled"
             env
             testCanCancelAFreshlyStartedFlowWithNoOpsPerformed
  , testThat "Flow can be cancelled during second stage" env testCanCancelInSecondStage
  , testThat "Flow cannot be cancelled after it has been completed"
             env
             testCannotCancelCompletedFlow
  , testThat "Flow cannot be cancelled multiple times"
             env
             testCannotCancelFlowMultipleTimes
  ]

flow1 :: Process
flow1 = Process [r|
dsl-version: "0.2.0"
stages:
  - signatory-sign-doc1:
      actions: []
      expect:
        signed-by:
          users: [signatory1]
          documents: [doc1]
  - author-sign-doc1:
      actions: []
      expect:
        signed-by:
          users: [author]
          documents: [doc1]
  - signatory-sign-doc2:
      actions: []
      expect:
        signed-by:
          users: [signatory2]
          documents: [doc2]
  - author-sign-doc2:
      actions: []
      expect:
        signed-by:
          users: [author]
          documents: [doc2]
|]

createFlowInstance1
  :: User -> Document -> Document -> TestEnv (GetInstance, SignatoryLink, SignatoryLink)
createFlowInstance1 author doc1 doc2 = do
  authorClient <- fmap (mkApiClient . Left) . getToken $ author ^. #id

  let [_, signatorySigLink1] = documentsignatorylinks doc1
  let [_, signatorySigLink2] = documentsignatorylinks doc2

  let mapping = InstanceKeyValues
        { documents = Map.fromList [("doc1", documentid doc1), ("doc2", documentid doc2)]
        , users     = Map.fromList
                        [ ("author"    , Email $ getEmail author)
                        , ("signatory1", Email $ getEmail signatorySigLink1)
                        , ("signatory2", Email $ getEmail signatorySigLink2)
                        ]
        , messages  = Map.fromList []
        }

  fmap (, signatorySigLink1, signatorySigLink2)
    . assertRight "start template response"
    $ createInstance authorClient "flow1" flow1 (toTemplateParameters mapping)

doSign :: Document -> SignatoryLink -> GetInstance -> UserName -> TestEnv ()
doSign doc sigLink flowInstance templateUserName = do
  signatoryFlowLink <-
    assertJust' (show templateUserName <> "'s access link should be present")
    $      (flowInstance ^. #accessLinks)
    Map.!? templateUserName

  signatoryEnv <- mkEnvForUser

  void
    . assertRight "authenticate with signatory flow link"
    . callFlowMagicHashLink signatoryEnv
    $ fromUrl signatoryFlowLink

  signatorySignContext <- authenticateContext signatoryEnv =<< mkContext defaultLang

  let docid = documentid doc
      slid  = signatorylinkid sigLink
  void $ mockDocTestRequestHelper
    signatorySignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign docid slid)
    200

flowStateCheck :: GetInstance -> ApiClient -> Status -> TestEnv ()
flowStateCheck flowInstance client status =
  assertEqual ("The instance should now be in " <> show status <> " state") status =<< do
    fmap (view #status)
      . assertRight "Flow instance data is still readable"
      . request
      . getInstance client
      $ (flowInstance ^. #id)

testCanCancelAFreshlyStartedFlowWithNoOpsPerformed :: TestEnv ()
testCanCancelAFreshlyStartedFlowWithNoOpsPerformed = do
  author       <- instantiateRandomUser
  authorClient <- fmap (mkApiClient . Left) . getToken $ author ^. #id

  doc1         <- addRandomFlowDocumentWithSignatory author
  doc2         <- addRandomFlowDocumentWithSignatory author

  commit

  flowInstance <- fst3 <$> createFlowInstance1 author doc1 doc2

  flowStateCheck flowInstance authorClient InProgress

  void . assertRight "author cancels flow" . request $ cancelInstance
    authorClient
    (flowInstance ^. #id)

  assertEqual "doc1 should be in canceled state" DocStatus.Canceled
    .   documentstatus
    =<< dbQuery (GetDocumentByDocumentID $ documentid doc1)

  assertEqual "doc2 should be in canceled state" DocStatus.Canceled
    .   documentstatus
    =<< dbQuery (GetDocumentByDocumentID $ documentid doc2)

  flowStateCheck flowInstance authorClient Failed

testCanCancelInSecondStage :: TestEnv ()
testCanCancelInSecondStage = do
  author       <- instantiateRandomUser
  authorClient <- fmap (mkApiClient . Left) . getToken $ author ^. #id

  doc1         <- addRandomFlowDocumentWithSignatory author
  doc2         <- addRandomFlowDocumentWithSignatory author

  commit

  (flowInstance, sl1, _sl2) <- createFlowInstance1 author doc1 doc2

  doSign doc1 sl1 flowInstance "signatory1"

  flowStateCheck flowInstance authorClient InProgress

  void . assertRight "author cancels flow" . request $ cancelInstance
    authorClient
    (flowInstance ^. #id)

  assertEqual "doc1 should be in canceled state" DocStatus.Canceled
    .   documentstatus
    =<< dbQuery (GetDocumentByDocumentID $ documentid doc1)

  assertEqual "doc2 should be in canceled state" DocStatus.Canceled
    .   documentstatus
    =<< dbQuery (GetDocumentByDocumentID $ documentid doc2)

  flowStateCheck flowInstance authorClient Failed

flow2 :: Process
flow2 = Process [r|
dsl-version: "0.2.0"
stages:
  - only-stage:
      actions: []
      expect:
        signed-by:
          users: [author, signatory1]
          documents: [doc1]
|]

testCannotCancelCompletedFlow :: TestEnv ()
testCannotCancelCompletedFlow = do
  author      <- instantiateRandomUser
  authorToken <- getToken $ author ^. #id
  let authorClient    = mkApiClient $ Left authorToken


  let templateRequest = CreateTemplate "flow2" flow2
  GetCreateTemplate templateId <- assertRight "create template" . request $ createTemplate
    authorClient
    templateRequest

  void . assertRight "commit template response" . request $ commitTemplate authorClient
                                                                           templateId

  doc1 <- addRandomFlowDocumentWithSignatory author
  commit

  let [authorSigLink, signatorySigLink] = documentsignatorylinks doc1
      mapping = InstanceKeyValues
        { documents = Map.fromList [("doc1", documentid doc1)]
        , users     = Map.fromList
                        [ ("author"    , Email $ getEmail author)
                        , ("signatory1", Email $ getEmail signatorySigLink)
                        ]
        , messages  = Map.fromList []
        }

  flowInstance <-
    assertRight "start template response"
    . request
    . startTemplate authorClient templateId
    $ CreateInstance Nothing (toTemplateParameters mapping) Nothing

  flowStateCheck flowInstance authorClient InProgress

  doSign doc1 authorSigLink    flowInstance "author"
  doSign doc1 signatorySigLink flowInstance "signatory1"

  flowStateCheck flowInstance authorClient Completed

  void . assertLeft "Author tries to cancel flow and fails" . request $ cancelInstance
    authorClient
    (flowInstance ^. #id)

  flowStateCheck flowInstance authorClient Completed

testCannotCancelFlowMultipleTimes :: TestEnv ()
testCannotCancelFlowMultipleTimes = do
  author       <- instantiateRandomUser
  authorClient <- fmap (mkApiClient . Left) . getToken $ author ^. #id

  doc1         <- addRandomFlowDocumentWithSignatory author
  doc2         <- addRandomFlowDocumentWithSignatory author

  commit

  flowInstance <- fst3 <$> createFlowInstance1 author doc1 doc2

  flowStateCheck flowInstance authorClient InProgress

  void . assertRight "Author cancels flow" . request $ cancelInstance
    authorClient
    (flowInstance ^. #id)

  void . assertLeft "Author cannot cancel flow a second time" . request $ cancelInstance
    authorClient
    (flowInstance ^. #id)

  flowStateCheck flowInstance authorClient Failed
