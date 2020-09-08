{-# LANGUAGE QuasiQuotes #-}
module Flow.IntegrationTest.SimpleCompleteProcess where

import Control.Monad.Reader.Class
import Happstack.Server hiding (Cookie(..), Request(..), resp)
import Optics hiding (mapping)
import Text.RawString.QQ
import qualified Data.Map as Map

import Auth.Session
import DB hiding (JSON)
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
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
  TestEnvSt {..} <- ask
  -- Prepare flow process author.
  user           <- instantiateRandomUser
  let authorEmail = getEmail user

  -- Prepare flow authentication sessions.
  oauth            <- getToken (user ^. #id)
  AuthCookies {..} <- AuthModel.insertNewSession "localhost" Nothing

  let ApiClient {..}            = mkApiClient (Left oauth)
  let ParticipantApiClient {..} = mkParticipantApiClient (Nothing, Nothing)
  let PageClient {..}           = mkPageClient (Nothing, Nothing)

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

  template <- assertRight "create template" . request $ createTemplate createTemplateData
  let tid = id (template :: GetCreateTemplate)
  void . assertRight "commit template response" . request $ commitTemplate tid
  startedInstance <-
    assertRight "start template response" . request . startTemplate tid $ CreateInstance
      Nothing
      mapping
      Nothing

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
    $ getInstanceView (startedInstance ^. #id) Nothing
  signatoryViewBeforeSign <-
    assertRight "get signatory instance view data before signing"
    . requestWithEnv signatoryEnv
    $ getInstanceView (startedInstance ^. #id) Nothing

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
    assertRight "get author instance view data before signing"
    . requestWithEnv authorEnv
    $ getInstanceView (startedInstance ^. #id) Nothing

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
    $ getInstanceView (startedInstance ^. #id) Nothing

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
