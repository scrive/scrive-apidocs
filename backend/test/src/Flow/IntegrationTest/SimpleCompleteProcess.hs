{-# LANGUAGE QuasiQuotes #-}
module Flow.IntegrationTest.SimpleCompleteProcess where

import Control.Monad.Reader.Class
import Happstack.Server hiding (Cookie(..), Request(..), resp)
import Optics hiding (mapping)
import Text.RawString.QQ
import qualified Data.Map as Map

import DB hiding (JSON)
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Model.Query
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.Client
import Flow.Core.Type.Url
import Flow.IntegrationTest.Common
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Model.Types.Internal
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.Routes.Api
import Flow.TestUtil
import TestEnvSt.Internal (flowPort)
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import User.Lang
import Util.HasSomeUserInfo
import qualified Auth.Model as AuthModel
import qualified Doc.Types.DocumentStatus as DocStatus

processCompleteFlow :: Process
processCompleteFlow = Process [r|
dsl-version: "0.2.0"
stages:
  - initial:
      actions: []
      expect:
        signed-by:
          users: [author, signatory]
          documents: [doc]
      actions:
        - notify:
            users: [signatory]
            methods:
              email: msg1
|]

testCompleteFlowProcess :: TestEnv ()
testCompleteFlowProcess = do
  TestEnvSt { flowPort } <- ask
  -- Prepare flow process author.
  user                   <- instantiateRandomUser
  let authorEmail = getEmail user

  -- Prepare flow authentication sessions.
  oauth <- getToken (user ^. #id)
  void $ AuthModel.insertNewSession "localhost" Nothing

  let apiClient         = mkApiClient (Left oauth)
  let participantClient = mkParticipantApiClient (Nothing, Nothing)

  -- Prepare document with two signatories (one of them is author).
  -- Consent module is disable so we don't need to care about it when signing.
  doc <- addRandomDocument (rdaDefault user)
    { rdaTypes       = OneOf [Signable]
    , rdaStatuses    = OneOf [Preparation]
    , rdaSignatories = let signatory = OneOf
                             [ [ RSC_IsSignatoryThatHasntSigned
                               , RSC_DeliveryMethodIs EmailDelivery
                               , RSC_AuthToViewIs StandardAuthenticationToView
                               , RSC_AuthToSignIs StandardAuthenticationToSign
                               , RSC_HasConsentModule False
                               ]
                             ]
                       in  OneOf [[signatory, signatory]]
    }

  -- Retrieve flow participant emails and signatory IDs.
  let [authorSigLink, signatorySigLink] = documentsignatorylinks doc
  let authorSigLinkId    = signatorylinkid authorSigLink
  let signatorySigLinkId = signatorylinkid signatorySigLink
  let signatoryEmail     = getEmail signatorySigLink

  let createTemplateData = CreateTemplate "name" processCompleteFlow
  let did                = documentid doc
  let mapping = InstanceKeyValues
        { documents = Map.fromList [("doc", did)]
        , users     = Map.fromList
                        [("author", Email authorEmail), ("signatory", Email signatoryEmail)]
        , messages  = Map.fromList [("msg1", "email notification")]
        }

  -- Flow stuff runs in another DB session so we need to commit before calling
  -- any flow related functions.
  commit

  template <- assertRight "create template" . request $ createTemplate
    apiClient
    createTemplateData

  let tid = id (template :: GetCreateTemplate)
  void . assertRight "commit template response" . request $ commitTemplate apiClient tid

  startedInstance <-
    assertRight "start template response"
    . request
    . startTemplate apiClient tid
    $ CreateInstance Nothing (toTemplateParameters mapping) Nothing

  -- Saving the lastEvent time to ensure that it increases after the sign event
  let lastEventAtStart = startedInstance ^. #lastEvent
  assertBool "Instance last_event field is equal to the started time"
    $  startedInstance
    ^. #started
    == lastEventAtStart

  authorFlowLink <-
    assertJust' "author's access link should be present"
    $      (startedInstance ^. #accessLinks)
    Map.!? "author"
  signatoryFlowLink <-
    assertJust' "signatory's access link should be present"
    $      (startedInstance ^. #accessLinks)
    Map.!? "signatory"

  authorEnv    <- mkEnvForUser
  signatoryEnv <- mkEnvForUser

  -- Authenticate servant client environment via flow magic hash link.
  void
    . assertRight "authenticate with author flow link"
    . callFlowMagicHashLink authorEnv
    $ fromUrl authorFlowLink

  void
    . assertRight "authenticate with signatory flow link"
    . callFlowMagicHashLink signatoryEnv
    $ fromUrl signatoryFlowLink

  authorViewBeforeSign <-
    assertRight "get author instance view data before signing"
    . requestWithEnv authorEnv
    $ getInstanceView participantClient (startedInstance ^. #id) Nothing
  signatoryViewBeforeSign <-
    assertRight "get signatory instance view data before signing"
    . requestWithEnv signatoryEnv
    $ getInstanceView participantClient (startedInstance ^. #id) Nothing

  assertEqual "author's state should be empty at the beginning" []
    $ view (#state % #documents) authorViewBeforeSign
  assertEqual "signatory's state should be empty at the beginning" []
    $ view (#state % #documents) signatoryViewBeforeSign

  assertEqual
      "check author's action before signing"
      (  Just
      .  InstanceUserAction Sign did authorSigLinkId
      .  Url
      $  "http://localhost:"
      <> showt flowPort
      <> "/s/"
      <> showt did
      <> "/"
      <> showt authorSigLinkId
      )
    $ preview (#actions % ix 0) authorViewBeforeSign
  assertEqual
      "check signatory's action before signing"
      (  Just
      .  InstanceUserAction Sign did signatorySigLinkId
      .  Url
      $  "http://localhost:"
      <> showt flowPort
      <> "/s/"
      <> showt did
      <> "/"
      <> showt signatorySigLinkId
      )
    $ preview (#actions % ix 0) signatoryViewBeforeSign

  -- Create kontrakcja authenticated context from authenticated servant client
  -- environment.
  authorSignContext    <- mkContext defaultLang >>= authenticateContext authorEnv
  signatorySignContext <- mkContext defaultLang >>= authenticateContext signatoryEnv

  mocDocPending        <- mockDocTestRequestHelper
    authorSignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did authorSigLinkId)
    200

  authorViewAfterSign <-
    assertRight "get author instance view data after signing"
    . requestWithEnv authorEnv
    $ getInstanceView participantClient (startedInstance ^. #id) Nothing

  assertEqual "author's state should contain one document"
              (Just $ InstanceUserDocument did Signed authorSigLinkId)
    $ preview (#state % #documents % ix 0) authorViewAfterSign

  assertEqual "there should be no left action for author" []
    $ view #actions authorViewAfterSign

  assertEqual "document status after signing should match"
              Pending
              (getMockDocStatus mocDocPending)

  -- Document can't be signed for second time.
  void $ mockDocTestRequestHelper
    authorSignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did authorSigLinkId)
    409

  void $ mockDocTestRequestHelper
    signatorySignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did signatorySigLinkId)
    200

  -- Check that the last_updated is greater than it was after making the instance
  signatoryViewAfterSign <-
    assertRight "get signatory instance view data after signing"
    . requestWithEnv signatoryEnv
    $ getInstanceView participantClient (startedInstance ^. #id) Nothing

  assertEqual "signatory's state should contain one document"
              (Just $ InstanceUserDocument did Signed signatorySigLinkId)
    $ preview (#state % #documents % ix 0) signatoryViewAfterSign

  assertEqual "there should be no left action for signatory" []
    $ view #actions signatoryViewAfterSign

  toBeSealed <- getDocumentsToBeSealed [did]
  assertEqual "Document is scheduled for sealing" [did] toBeSealed

  let lastEventAfterSign = signatoryViewAfterSign ^. #lastEvent
  assertBool "Instance last_event field is updated after signing"
    $ lastEventAtStart
    < lastEventAfterSign

  -- Document can't be signed for second time.
  void $ mockDocTestRequestHelper
    signatorySignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did signatorySigLinkId)
    409


flow1 :: Process
flow1 = Process [r|
dsl-version: "0.2.0"
stages:
  - stage-1:
      actions: []
      expect:
        signed-by:
          users: [author]
          documents: [doc1]
  - stage-2:
      actions: []
      expect:
        signed-by:
          users: [signatory1]
          documents: [doc1]
  - stage-3:
      actions: []
      expect:
        signed-by:
          users: [signatory2]
          documents: [doc2]
  - stage-4:
      actions: []
      expect:
        signed-by:
          users: [author]
          documents: [doc2]
|]

testScenario1 :: TestEnv ()
testScenario1 = do
  author <- instantiateRandomUser
  let authorEmail = getEmail author

  authorToken <- getToken (author ^. #id)

  let authorClient = mkApiClient (Left authorToken)

  doc1 <- addRandomDocument (rdaDefault author)
    { rdaTypes       = OneOf [Signable]
    , rdaStatuses    = OneOf [Preparation]
    , rdaTimeoutTime = False
    , rdaDaysToSign  = Just 90
    , rdaSignatories = let signatory = OneOf
                             [ [ RSC_IsSignatoryThatHasntSigned
                               , RSC_DeliveryMethodIs EmailDelivery
                               , RSC_AuthToViewIs StandardAuthenticationToView
                               , RSC_AuthToSignIs StandardAuthenticationToSign
                               , RSC_HasConsentModule False
                               ]
                             ]
                       in  OneOf [[signatory, signatory]]
    }

  doc2 <- addRandomDocument (rdaDefault author)
    { rdaTypes       = OneOf [Signable]
    , rdaStatuses    = OneOf [Preparation]
    , rdaTimeoutTime = False
    , rdaDaysToSign  = Just 90
    , rdaSignatories = let signatory = OneOf
                             [ [ RSC_IsSignatoryThatHasntSigned
                               , RSC_DeliveryMethodIs EmailDelivery
                               , RSC_AuthToViewIs StandardAuthenticationToView
                               , RSC_AuthToSignIs StandardAuthenticationToSign
                               , RSC_HasConsentModule False
                               ]
                             ]
                       in  OneOf [[signatory, signatory]]
    }

  let [authorSigLink1, signatorySigLink1] = documentsignatorylinks doc1
  let [authorSigLink2, signatorySigLink2] = documentsignatorylinks doc2

  commit

  let createTemplateData = CreateTemplate "flow1" flow1
  GetCreateTemplate templateId <- assertRight "create template" . request $ createTemplate
    authorClient
    createTemplateData

  void . assertRight "commit template response" . request $ commitTemplate authorClient
                                                                           templateId

  let mapping = InstanceKeyValues
        { documents = Map.fromList [("doc1", documentid doc1), ("doc2", documentid doc2)]
        , users     = Map.fromList
                        [ ("author"    , Email authorEmail)
                        , ("signatory1", Email $ getEmail signatorySigLink1)
                        , ("signatory2", Email $ getEmail signatorySigLink2)
                        ]
        , messages  = Map.fromList []
        }

  flowInstance <-
    (assertRight "start template response" . request)
    . startTemplate authorClient templateId
    $ CreateInstance Nothing (toTemplateParameters mapping) Nothing

  authorFlowLink <- assertJust' "author's access link should be present"
    $ Map.lookup "author" (flowInstance ^. #accessLinks)

  signatoryFlowLink1 <- assertJust' "author's access link should be present"
    $ Map.lookup "signatory1" (flowInstance ^. #accessLinks)

  signatoryFlowLink2 <- assertJust' "author's access link should be present"
    $ Map.lookup "signatory2" (flowInstance ^. #accessLinks)

  authorEnv     <- mkEnvForUser
  signatoryEnv1 <- mkEnvForUser
  signatoryEnv2 <- mkEnvForUser

  void
    . assertRight "authenticate with author flow link"
    . callFlowMagicHashLink authorEnv
    $ fromUrl authorFlowLink

  void
    . assertRight "authenticate with signatory flow link"
    . callFlowMagicHashLink signatoryEnv1
    $ fromUrl signatoryFlowLink1

  void
    . assertRight "authenticate with signatory flow link"
    . callFlowMagicHashLink signatoryEnv2
    $ fromUrl signatoryFlowLink2

  authorSignContext     <- mkContext defaultLang >>= authenticateContext authorEnv
  signatorySignContext1 <- mkContext defaultLang >>= authenticateContext signatoryEnv1
  signatorySignContext2 <- mkContext defaultLang >>= authenticateContext signatoryEnv2

  do
    request1 <- mkRequest
      POST
      [ ("fields"           , inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
    (response, _) <- runTestKontra request1 signatorySignContext1
      $ docApiV2SigSign (documentid doc1) (signatorylinkid signatorySigLink1)
    let code = rsCode response
    assertEqual "Signatory cannot sign before author" 409 code

  void $ mockDocTestRequestHelper
    authorSignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign (documentid doc1) $ signatorylinkid authorSigLink1)
    200

  void $ mockDocTestRequestHelper
    signatorySignContext1
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign (documentid doc1) $ signatorylinkid signatorySigLink1)
    200

  void $ mockDocTestRequestHelper
    signatorySignContext2
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign (documentid doc2) $ signatorylinkid signatorySigLink2)
    200

  void $ mockDocTestRequestHelper
    authorSignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign (documentid doc2) $ signatorylinkid authorSigLink2)
    200

  doc1a <- dbQuery $ GetDocumentByDocumentID (documentid doc1)
  doc1b <- dbQuery $ GetDocumentByDocumentID (documentid doc2)

  assertEqual "doc1 should be in closed state" Closed (documentstatus doc1a)

  assertEqual "doc2 should be in closed state" Closed (documentstatus doc1b)

flow2 :: Process
flow2 = Process [r|
dsl-version: "0.2.0"
stages:
  - author-sign-doc1:
      actions: []
      expect:
        signed-by:
          users: [author]
          documents: [doc1]
  - signatory-sign-doc1:
      actions: []
      expect:
        signed-by:
          users: [signatory1]
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

testScenario2 :: TestEnv ()
testScenario2 = do
  author <- instantiateRandomUser
  let authorEmail = getEmail author

  authorToken <- getToken (author ^. #id)

  let authorClient = mkApiClient (Left authorToken)

  doc1 <- addRandomDocument (rdaDefault author)
    { rdaTypes       = OneOf [Signable]
    , rdaStatuses    = OneOf [Preparation]
    , rdaTimeoutTime = False
    , rdaDaysToSign  = Just 90
    , rdaSignatories = let signatory = OneOf
                             [ [ RSC_IsSignatoryThatHasntSigned
                               , RSC_DeliveryMethodIs EmailDelivery
                               , RSC_AuthToViewIs StandardAuthenticationToView
                               , RSC_AuthToSignIs StandardAuthenticationToSign
                               , RSC_HasConsentModule False
                               ]
                             ]
                       in  OneOf [[signatory, signatory]]
    }

  doc2 <- addRandomDocument (rdaDefault author)
    { rdaTypes       = OneOf [Signable]
    , rdaStatuses    = OneOf [Preparation]
    , rdaTimeoutTime = False
    , rdaDaysToSign  = Just 90
    , rdaSignatories = let signatory = OneOf
                             [ [ RSC_IsSignatoryThatHasntSigned
                               , RSC_DeliveryMethodIs EmailDelivery
                               , RSC_AuthToViewIs StandardAuthenticationToView
                               , RSC_AuthToSignIs StandardAuthenticationToSign
                               , RSC_HasConsentModule False
                               ]
                             ]
                       in  OneOf [[signatory, signatory]]
    }

  let [authorSigLink1, signatorySigLink1] = documentsignatorylinks doc1
  let [_, signatorySigLink2]              = documentsignatorylinks doc2

  commit

  let createTemplateData = CreateTemplate "flow2" flow2
  GetCreateTemplate templateId <- assertRight "create template" . request $ createTemplate
    authorClient
    createTemplateData

  void . assertRight "commit template response" . request $ commitTemplate authorClient
                                                                           templateId

  let mapping = InstanceKeyValues
        { documents = Map.fromList [("doc1", documentid doc1), ("doc2", documentid doc2)]
        , users     = Map.fromList
                        [ ("author"    , Email authorEmail)
                        , ("signatory1", Email $ getEmail signatorySigLink1)
                        , ("signatory2", Email $ getEmail signatorySigLink2)
                        ]
        , messages  = Map.fromList []
        }

  flowInstance <-
    (assertRight "start template response" . request)
    . startTemplate authorClient templateId
    $ CreateInstance Nothing (toTemplateParameters mapping) Nothing

  authorFlowLink <- assertJust' "author's access link should be present"
    $ Map.lookup "author" (flowInstance ^. #accessLinks)

  signatoryFlowLink1 <- assertJust' "author's access link should be present"
    $ Map.lookup "signatory1" (flowInstance ^. #accessLinks)

  signatoryFlowLink2 <- assertJust' "author's access link should be present"
    $ Map.lookup "signatory2" (flowInstance ^. #accessLinks)

  authorEnv     <- mkEnvForUser
  signatoryEnv1 <- mkEnvForUser
  signatoryEnv2 <- mkEnvForUser

  void
    . assertRight "authenticate with author flow link"
    . callFlowMagicHashLink authorEnv
    $ fromUrl authorFlowLink

  void
    . assertRight "authenticate with signatory flow link"
    . callFlowMagicHashLink signatoryEnv1
    $ fromUrl signatoryFlowLink1

  void
    . assertRight "authenticate with signatory flow link"
    . callFlowMagicHashLink signatoryEnv2
    $ fromUrl signatoryFlowLink2

  authorSignContext     <- mkContext defaultLang >>= authenticateContext authorEnv
  signatorySignContext1 <- mkContext defaultLang >>= authenticateContext signatoryEnv1

  void $ mockDocTestRequestHelper
    authorSignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigReject (documentid doc1) $ signatorylinkid authorSigLink1)
    200

  do
    request1 <- mkRequest
      POST
      [ ("fields"           , inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
    (response, _) <- runTestKontra request1 signatorySignContext1
      $ docApiV2SigSign (documentid doc1) (signatorylinkid signatorySigLink1)
    let code = rsCode response
    assertEqual "Signatory 1 signing should fail with error 409" 409 code

  doc1a <- dbQuery $ GetDocumentByDocumentID (documentid doc1)
  doc1b <- dbQuery $ GetDocumentByDocumentID (documentid doc2)

  assertEqual "doc1 should be in rejected state" DocStatus.Rejected (documentstatus doc1a)

  assertEqual "doc2 should be in pending state"  DocStatus.Canceled (documentstatus doc1b)
