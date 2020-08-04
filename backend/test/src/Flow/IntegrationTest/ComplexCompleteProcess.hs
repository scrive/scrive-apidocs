{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedLabels #-}

module Flow.IntegrationTest.ComplexCompleteProcess where

import Control.Monad.Reader.Class
import Happstack.Server hiding (Cookie(..), Request(..), resp)
import Optics hiding (mapping)
import Servant.Client
import Text.RawString.QQ
import qualified Data.Map as Map

import Auth.Session
import Context
import DB hiding (JSON)
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
import Doc.DocumentID
import Doc.Model.Query
import Doc.Model.Update
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.Client
import Flow.Id
import Flow.IntegrationTest.Common
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Model.Types.Internal
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.Routes.Api
import Flow.Routes.Types
import Flow.TestUtil
import MinutesTime
import TestEnvSt.Internal (flowPort)
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import User.Lang
import Util.Actor
import Util.HasSomeUserInfo
import qualified Auth.Model as AuthModel

processCompleteFlow :: Process
processCompleteFlow = Process [r|
dsl-version: "0.1.0"
stages:
  - signatory:
      actions: []
      expect:
        signed-by:
          users: [signatory]
          documents: [doc1, doc2]
  - author:
      actions: []
      expect:
        signed-by:
          users: [author]
          documents: [doc1, doc2]
|]

testComplexFlowProcess :: TestEnv ()
testComplexFlowProcess = do
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
  -- Consent module is disabled so we don't need to care about it when signing.
  doc1 <- addRandomDocument (rdaDefault user)
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

  now         <- currentTime
  Just doc2id <- dbUpdate
    $ CloneDocumentWithUpdatedAuthor Nothing doc1 (systemActor now) identity
  doc2 <- dbQuery $ GetDocumentByDocumentID doc2id

  -- Retrieve flow participant emails and signatory IDs.
  let [authorSigLink, signatorySigLink] = documentsignatorylinks doc1
  let authorSigLinkId    = signatorylinkid authorSigLink
  let signatorySigLinkId = signatorylinkid signatorySigLink
  let signatoryEmail     = getEmail signatorySigLink

  let createTemplateData = CreateTemplate "name" processCompleteFlow
  let doc1id             = documentid doc1
  let mapping = InstanceKeyValues
        { documents = Map.fromList [("doc1", doc1id), ("doc2", doc2id)]
        , users     = Map.fromList
                        [("author", Email authorEmail), ("signatory", Email signatoryEmail)]
        , messages  = mempty
        }

  -- Flow stuff runs in another DB session so we need to commit before calling
  -- any flow related functions.
  commit

  template <- assertRight "create template" . request $ createTemplate createTemplateData
  let tid = id (template :: GetCreateTemplate)
  void . assertRight "commit template response" . request $ commitTemplate tid
  GetInstance {..} <- assertRight "start template response" . request $ startTemplate
    tid
    mapping
  authorFlowLink <-
    assertJust' "author's flow sign link is missing" $ accessLinks Map.!? "author"
  signatoryFlowLink <-
    assertJust' "signatory's flow sign link is missing" $ accessLinks Map.!? "signatory"

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

  signatoryViewBeforeSign <-
    assertRight "get signatory instance view data before signing"
    . requestWithEnv signatoryEnv
    $ getInstanceView id Nothing

  assertEqual "signatory's state should be empty at the beginning" []
    $ view (#state % #documents) signatoryViewBeforeSign

  assertEqual
      "check signatory's action before signing"
      (  Just
      .  InstanceUserAction Sign doc1id signatorySigLinkId
      .  Url
      $  "http://localhost:"
      <> showt flowPort
      <> "/s/"
      <> showt doc1id
      <> "/"
      <> showt signatorySigLinkId
      )
    $ preview (#actions % ix 0) signatoryViewBeforeSign

  -- Create kontrakcja authenticated context from authenticated servant client
  -- environment.
  authorSignContext    <- mkContext defaultLang >>= authenticateContext authorEnv
  signatorySignContext <- mkContext defaultLang >>= authenticateContext signatoryEnv

  let authorSigning    = Signing authorSignContext authorSigLink authorEnv
  let signatorySigning = Signing signatorySignContext signatorySigLink signatoryEnv
  let [authorSigLink2, signatorySigLink2] = documentsignatorylinks doc2

  signDocument id doc1id signatorySigning 1
  signDocument id doc2id signatorySigning { signatory = signatorySigLink2 } 2

  authorViewBeforeSign <-
    assertRight "get author instance view data before signing"
    . requestWithEnv authorEnv
    $ getInstanceView id Nothing
  assertEqual "author's state should be empty at the beginning" []
    $ view (#state % #documents) authorViewBeforeSign
  assertEqual
      "check author's action before signing"
      (  Just
      .  InstanceUserAction Sign doc1id authorSigLinkId
      .  Url
      $  "http://localhost:"
      <> showt flowPort
      <> "/s/"
      <> showt doc1id
      <> "/"
      <> showt authorSigLinkId
      )
    $ preview (#actions % ix 0) authorViewBeforeSign

  signDocument id doc1id authorSigning 1
  signDocument id doc2id authorSigning { signatory = authorSigLink2 } 2

  toBeSealed <- getDocumentsToBeSealed [doc1id, doc2id]
  assertEqual "Documents are scheduled for sealing" [doc1id, doc2id] toBeSealed

data Signing = Signing
  { context :: Context
  , signatory :: SignatoryLink
  , env :: ClientEnv
  }

signDocument :: InstanceId -> DocumentID -> Signing -> Int -> TestEnv ()
signDocument id docId Signing {..} step = do
  let ParticipantApiClient {..} = mkParticipantApiClient (Nothing, Nothing)
  let linkId                    = signatorylinkid signatory

  void $ mockDocTestRequestHelper
    context
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign docId linkId)
    200

  authorViewAfterSign <-
    assertRight "get author instance view data before signing"
    . requestWithEnv env
    $ getInstanceView id Nothing

  let (signedDocuments, remainingActions) = case step of
        1 -> (step, 2 - step)
        2 -> (step, 2 - step)
        _ -> unexpectedError $ "Unexpected step " <> showt step

  assertEqual "user state should contain some documents" signedDocuments
    $ length (view (#state % #documents) authorViewAfterSign)

  assertEqual "there should be correct number of actions" remainingActions
    $ length (view #actions authorViewAfterSign)
