{-# LANGUAGE QuasiQuotes #-}
module Flow.IntegrationTest.Rejection (tests) where

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
import Flow.CallbackPayload
import Flow.CallbackTest (withTestCallbackProcessor)
import Flow.Client
import Flow.Core.Type.Callback
import Flow.Core.Type.Url
import Flow.IntegrationTest.Common
import Flow.Machinize
import Flow.Model
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Model.Types.Internal
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
import qualified Flow.CallbackPayload as Callback

tests :: TestEnvSt -> Test
tests env = testGroup
  "Flow Rejection Tests"
  [ testThat "First signatory in 1st stage can reject flow"
             env
             testSignatoryInFirstStageCanRejectFlow
  , testThat "Second signatory in 3rd stage can reject flow"
             env
             testSignatoryInThirdStageCanRejectFlow
  , testThat "Second signatory in 3rd stage can reject flow after first signatory signs"
             env
             testSignatoryInFirstStageCanRejectFlowAfterFirstSignatorySigns
  , testThat "Flow cannot be rejected multiple times"
             env
             testCannotRejectFlowMultipleTimes
  , testThat "Signatory can still reject flow after signing all documents"
             env
             testSignatoryCanRejectFlowAfterSigning
  , testThat "Test rejection callback" env testRejectionCallback
  , testThat "First signatory in 1st stage can reject document"
             env
             testSignatoryInFirstStageCanRejectDocument
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
  :: User -> Document -> Document -> Maybe Callback -> TestEnv GetInstance
createFlowInstance1 author doc1 doc2 mCallback = do
  authorToken <- getToken (author ^. #id)
  let authorClient           = mkApiClient (Left authorToken)

  let [_, signatorySigLink1] = documentsignatorylinks doc1
  let [_, signatorySigLink2] = documentsignatorylinks doc2

  let createTemplateData     = CreateTemplate "flow1" flow1
  GetCreateTemplate templateId <- assertRight "create template" . request $ createTemplate
    authorClient
    createTemplateData

  void . assertRight "commit template response" . request $ commitTemplate authorClient
                                                                           templateId

  let mapping = InstanceKeyValues
        { documents = Map.fromList [("doc1", documentid doc1), ("doc2", documentid doc2)]
        , users     = Map.fromList
                        [ ("author"    , Email $ getEmail author)
                        , ("signatory1", Email $ getEmail signatorySigLink1)
                        , ("signatory2", Email $ getEmail signatorySigLink2)
                        ]
        , messages  = Map.fromList []
        }

  assertRight "start template response"
    . request
    . startTemplate authorClient templateId
    $ CreateInstance Nothing (toTemplateParameters mapping) mCallback

testSignatoryInFirstStageCanRejectFlow :: TestEnv ()
testSignatoryInFirstStageCanRejectFlow = do
  author <- instantiateRandomUser

  doc1   <- addRandomFlowDocumentWithSignatory author
  doc2   <- addRandomFlowDocumentWithSignatory author

  commit

  flowInstance <- createFlowInstance1 author doc1 doc2 Nothing
  let rejectMessage = "test reject"

  do
    signatoryEnv1 <- createClientEnvFromFlowInstance "signatory1" flowInstance
    let participantClient = mkParticipantApiClient (Nothing, Nothing)

    void
      . assertRight "signatory 1 rejects flow"
      . requestWithEnv signatoryEnv1
      $ rejectInstance participantClient
                       (flowInstance ^. #id)
                       (RejectParam $ Just rejectMessage)

  do
    doc1a <- dbQuery $ GetDocumentByDocumentID (documentid doc1)
    assertEqual "doc1 should be in canceled state"
                DocStatus.Canceled
                (documentstatus doc1a)

  do
    doc2a <- dbQuery $ GetDocumentByDocumentID (documentid doc2)
    assertEqual "doc2 should be in pending state" DocStatus.Pending (documentstatus doc2a)

  do
    events <- selectInstanceEvents (flowInstance ^. #id) False

    assertEqual "one event should be recorded" 1 $ length events

    let Event { instanceId, userName, documentName, userAction, eventDetails } =
          head events

    assertEqual "instance id should match"             (flowInstance ^. #id) instanceId
    assertEqual "rejector name should match"           "signatory1"          userName
    assertEqual "should have no document"              Nothing               documentName
    assertEqual "user action should be flow rejection" FlowRejection         userAction
    assertEqual "event details should match"
                (Just . RejectionEventDetails $ RejectionDetails rejectMessage)
                eventDetails

    return ()

testSignatoryInThirdStageCanRejectFlow :: TestEnv ()
testSignatoryInThirdStageCanRejectFlow = do
  author <- instantiateRandomUser

  doc1   <- addRandomFlowDocumentWithSignatory author
  doc2   <- addRandomFlowDocumentWithSignatory author

  commit

  flowInstance <- createFlowInstance1 author doc1 doc2 Nothing

  do
    signatoryEnv2 <- createClientEnvFromFlowInstance "signatory2" flowInstance
    let participantClient = mkParticipantApiClient (Nothing, Nothing)

    void
      . assertRight "signatory 2 rejects flow"
      . requestWithEnv signatoryEnv2
      $ rejectInstance participantClient
                       (flowInstance ^. #id)
                       (RejectParam $ Just "Testing rejection")

  do
    doc1a <- dbQuery $ GetDocumentByDocumentID (documentid doc1)
    assertEqual "doc1 should be in pending state" DocStatus.Pending (documentstatus doc1a)

  do
    doc2a <- dbQuery $ GetDocumentByDocumentID (documentid doc2)
    assertEqual "doc2 should be in canceled state"
                DocStatus.Canceled
                (documentstatus doc2a)

testSignatoryInFirstStageCanRejectFlowAfterFirstSignatorySigns :: TestEnv ()
testSignatoryInFirstStageCanRejectFlowAfterFirstSignatorySigns = do
  author <- instantiateRandomUser

  doc1   <- addRandomFlowDocumentWithSignatory author
  doc2   <- addRandomFlowDocumentWithSignatory author

  commit

  flowInstance <- createFlowInstance1 author doc1 doc2 Nothing

  do
    -- Signatory1 signs document first
    signatoryEnv1 <- createClientEnvFromFlowInstance "signatory1" flowInstance
    let [_, signatorySigLink1] = documentsignatorylinks doc1
    signatorySignContext <- authenticateContext signatoryEnv1 =<< mkContext defaultLang

    void $ mockDocTestRequestHelper
      signatorySignContext
      POST
      [ ("fields"           , inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
      (docApiV2SigSign (documentid doc1) (signatorylinkid signatorySigLink1))
      200

  do
    signatoryEnv2 <- createClientEnvFromFlowInstance "signatory2" flowInstance
    let participantClient = mkParticipantApiClient (Nothing, Nothing)

    void
      . assertRight "signatory 2 rejects flow"
      . requestWithEnv signatoryEnv2
      $ rejectInstance participantClient (flowInstance ^. #id) (RejectParam Nothing)

  do
    doc1a <- dbQuery $ GetDocumentByDocumentID (documentid doc1)
    assertEqual "doc1 should be in pending state" DocStatus.Pending (documentstatus doc1a)

  do
    doc2a <- dbQuery $ GetDocumentByDocumentID (documentid doc2)
    assertEqual "doc2 should be in canceled state"
                DocStatus.Canceled
                (documentstatus doc2a)

testCannotRejectFlowMultipleTimes :: TestEnv ()
testCannotRejectFlowMultipleTimes = do
  author <- instantiateRandomUser

  doc1   <- addRandomFlowDocumentWithSignatory author
  doc2   <- addRandomFlowDocumentWithSignatory author

  commit

  flowInstance <- createFlowInstance1 author doc1 doc2 Nothing

  do
    signatoryEnv1 <- createClientEnvFromFlowInstance "signatory1" flowInstance
    let participantClient = mkParticipantApiClient (Nothing, Nothing)

    void
      . assertRight "signatory 1 rejects flow"
      . requestWithEnv signatoryEnv1
      $ rejectInstance participantClient (flowInstance ^. #id) (RejectParam Nothing)

  do
    signatoryEnv2 <- createClientEnvFromFlowInstance "signatory2" flowInstance
    let participantClient = mkParticipantApiClient (Nothing, Nothing)

    void
      . assertLeft "signatory 2 cannot reject flow"
      . requestWithEnv signatoryEnv2
      $ rejectInstance participantClient (flowInstance ^. #id) (RejectParam Nothing)

testSignatoryCanRejectFlowAfterSigning :: TestEnv ()
testSignatoryCanRejectFlowAfterSigning = do
  author <- instantiateRandomUser

  doc1   <- addRandomFlowDocumentWithSignatory author
  doc2   <- addRandomFlowDocumentWithSignatory author

  commit

  flowInstance <- createFlowInstance1 author doc1 doc2 Nothing

  do
    -- Signatory1 signs document first
    signatoryEnv1 <- createClientEnvFromFlowInstance "signatory1" flowInstance
    let [_, signatorySigLink1] = documentsignatorylinks doc1
    signatorySignContext <- authenticateContext signatoryEnv1 =<< mkContext defaultLang

    void $ mockDocTestRequestHelper
      signatorySignContext
      POST
      [ ("fields"           , inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
      (docApiV2SigSign (documentid doc1) (signatorylinkid signatorySigLink1))
      200

  do
    -- Author signs document next
    authorEnv <- createClientEnvFromFlowInstance "author" flowInstance
    let [authorSigLink, _] = documentsignatorylinks doc1
    authorSignContext <- authenticateContext authorEnv =<< mkContext defaultLang

    void $ mockDocTestRequestHelper
      authorSignContext
      POST
      [ ("fields"           , inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
      (docApiV2SigSign (documentid doc1) (signatorylinkid authorSigLink))
      200

  do
    signatoryEnv1 <- createClientEnvFromFlowInstance "signatory1" flowInstance
    let participantClient = mkParticipantApiClient (Nothing, Nothing)

    void
      . assertRight "signatory 1 can still reject flow"
      . requestWithEnv signatoryEnv1
      $ rejectInstance participantClient (flowInstance ^. #id) (RejectParam Nothing)

  do
    doc1a <- dbQuery $ GetDocumentByDocumentID (documentid doc1)
    assertEqual "doc1 should be in canceled state"
                DocStatus.Canceled
                (documentstatus doc1a)

  do
    doc2a <- dbQuery $ GetDocumentByDocumentID (documentid doc2)
    assertEqual "doc2 should be in pending state" DocStatus.Pending (documentstatus doc2a)

  do
    -- Signatory 2 should not be able to sign document
    signatoryEnv2 <- createClientEnvFromFlowInstance "signatory2" flowInstance
    let [_, signatorySigLink2] = documentsignatorylinks doc2
    signatorySignContext <- authenticateContext signatoryEnv2 =<< mkContext defaultLang

    void $ mockDocTestRequestHelper
      signatorySignContext
      POST
      [ ("fields"           , inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
      (docApiV2SigSign (documentid doc1) (signatorylinkid signatorySigLink2))
      409

receiverPort :: Int
receiverPort = 31846

receiverAddress :: Url
receiverAddress = Url $ "http://localhost:" <> showt receiverPort

testRejectionCallback :: TestEnv ()
testRejectionCallback = do
  let rejectMessage = "test reject"

  (flowInstance, receivedPayloads) <- withTestCallbackProcessor $ do
    author <- instantiateRandomUser

    doc1   <- addRandomFlowDocumentWithSignatory author
    doc2   <- addRandomFlowDocumentWithSignatory author

    commit

    flowInstance <- createFlowInstance1 author doc1 doc2 . Just $ Callback
      receiverAddress
      V1

    do
      signatoryEnv1 <- createClientEnvFromFlowInstance "signatory1" flowInstance
      let participantClient = mkParticipantApiClient (Nothing, Nothing)

      let param             = RejectParam $ Just rejectMessage

      void
        . assertRight "signatory 1 rejects flow"
        . requestWithEnv signatoryEnv1
        $ rejectInstance participantClient (flowInstance ^. #id) param

    return flowInstance

  assertEqual "Only one callback should be received" 1 $ length receivedPayloads

  let FlowCallbackEventV1Envelope { flowInstanceId, event } = head receivedPayloads

  let event2 = RejectedEvent { userName       = "signatory1"
                             , mDocumentName  = Nothing
                             , mRejectMessage = Just rejectMessage
                             }

  assertEqual "Event should be rejected event" (Callback.Rejected event2) event

  assertEqual "Instance Id should match"       (flowInstance ^. #id)      flowInstanceId

testSignatoryInFirstStageCanRejectDocument :: TestEnv ()
testSignatoryInFirstStageCanRejectDocument = do
  let rejectMessage = "test reject"
  (flowInstance, receivedPayloads) <- withTestCallbackProcessor $ do
    author <- instantiateRandomUser

    doc1   <- addRandomFlowDocumentWithSignatory author
    doc2   <- addRandomFlowDocumentWithSignatory author

    commit

    flowInstance <- createFlowInstance1 author doc1 doc2 . Just $ Callback
      receiverAddress
      V1

    do
      let [_, signatorySigLink1] = documentsignatorylinks doc1
      signatoryEnv1         <- createClientEnvFromFlowInstance "signatory1" flowInstance
      signatorySignContext1 <- mkContext defaultLang >>= authenticateContext signatoryEnv1

      void $ mockDocTestRequestHelper
        signatorySignContext1
        POST
        [ ("fields"           , inText "[]")
        , ("accepted_author_attachments", inText "[]")
        , ("consent_responses", inText "[]")
        , ("reason"           , inText rejectMessage)
        ]
        (docApiV2SigReject (documentid doc1) $ signatorylinkid signatorySigLink1)
        200

    do
      doc1a <- dbQuery $ GetDocumentByDocumentID (documentid doc1)
      assertEqual "doc1 should be in rejected state"
                  DocStatus.Rejected
                  (documentstatus doc1a)

    do
      doc2a <- dbQuery $ GetDocumentByDocumentID (documentid doc1)
      assertEqual "doc2 should be in pending state"
                  DocStatus.Rejected
                  (documentstatus doc2a)

    do
      events <- selectInstanceEvents (flowInstance ^. #id) False

      assertEqual "one event should be recorded" 1 $ length events

      let Event { instanceId, userName, documentName, userAction, eventDetails } =
            head events

      assertEqual "instance id should match"             (flowInstance ^. #id) instanceId
      assertEqual "rejector name should match"           "signatory1"          userName
      assertEqual "should have document 1" (Just "doc1") documentName
      assertEqual "user action should be flow rejection" DocumentRejection     userAction
      assertEqual "event details should match"
                  (Just . RejectionEventDetails $ RejectionDetails rejectMessage)
                  eventDetails

    return flowInstance

  assertEqual "Only one callback should be received" 1 $ length receivedPayloads

  let FlowCallbackEventV1Envelope { flowInstanceId, event } = head receivedPayloads

  let event2 = RejectedEvent { userName       = "signatory1"
                             , mDocumentName  = Just "doc1"
                             , mRejectMessage = Just rejectMessage
                             }

  assertEqual "Event should be rejected event" (Callback.Rejected event2) event

  assertEqual "Instance Id should match"       (flowInstance ^. #id)      flowInstanceId

  return ()
