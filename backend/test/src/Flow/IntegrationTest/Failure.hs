{-# LANGUAGE QuasiQuotes #-}
module Flow.IntegrationTest.Failure where

import Control.Monad.Reader.Class
import Happstack.Server hiding (Cookie(..), Request(..), resp)
import Network.HTTP.Types (statusCode)
import Servant.Client (responseStatusCode)
import Text.RawString.QQ
import qualified Data.Map as Map

import DB hiding (JSON)
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.Client
import Flow.IntegrationTest.Common
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Model.Types.Internal
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.Routes.Api
import Flow.Routes.Types
import Flow.TestUtil
import TestEnvSt.Internal (flowPort)
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import User.Lang
import Util.HasSomeUserInfo

rejectedDocumentProcess :: Process
rejectedDocumentProcess = Process [r|
dsl-version: "0.1.0"
stages:
  - author-stage:
      actions: []
      expect:
        signed-by:
          users: [author]
          documents: [doc]
  - signatory-stage:
      actions: []
      expect:
        signed-by:
          users: [signatory]
          documents: [doc]
|]

testRejectedDocumentCausesProcessFailure :: TestEnv ()
testRejectedDocumentCausesProcessFailure = do
  TestEnvSt {..} <- ask
  user           <- instantiateRandomUser
  oauth          <- getToken (user ^. #id)
  let ac = mkApiClient (Left oauth)

  -- Prepare a document with two signatories (one of them is the author).
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

  let [authorSigLink, signatorySigLink] = documentsignatorylinks doc
  let did = documentid doc
  let mapping = InstanceKeyValues
        { documents = Map.fromList [("doc", did)]
        , users     = Map.fromList
                        [ ("author"   , Email $ getEmail user)
                        , ("signatory", Email $ getEmail signatorySigLink)
                        ]
        , messages  = mempty
        }

  -- Flow stuff runs in another DB session so we need to commit before calling
  -- any flow related functions.
  commit

  GetInstance {..} <- assertRight "start template"
    $ createInstance ac "dummy" rejectedDocumentProcess mapping

  -- Author rejects the document
  authorContext <- mkContextWithUser defaultLang user
  void $ mockDocTestRequestHelper
    authorContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigReject did $ signatorylinkid authorSigLink)
    200

  -- Signatory tries to access the overview page.
  -- They should not be able to since the process has failed due to the author's rejection.
  let signatoryFlowLink = fromJust $ accessLinks Map.!? "signatory"
  signatoryEnv <- mkEnvForUser
  mResponse    <-
    fmap errorResponse
    . assertLeft "authenticate with signatory access link"
    . callFlowMagicHashLink signatoryEnv
    $ fromUrl signatoryFlowLink
  assertEqual "authenticating with signatory access link should fail with error 403"
              (Just 403)
              (statusCode . responseStatusCode <$> mResponse)

reusedDocumentProcess :: Process
reusedDocumentProcess = Process [r|
dsl-version: "0.1.0"
stages:
  - initial:
      actions: []
      expect:
        signed-by:
          users: [signatory]
          documents: [doc1]
|]

testInstanceFailureReusedDocument :: TestEnv ()
testInstanceFailureReusedDocument = do
  user  <- instantiateRandomUser
  oauth <- getToken (user ^. #id)
  let ac@ApiClient {..} = mkApiClient (Left oauth)

  doc1 <- addRandomDocument (rdaDefault user)
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
  commit

  let mapping = InstanceKeyValues documents users messages
        where
          documents = Map.fromList [("doc1", documentid doc1)]
          users     = Map.fromList [("signatory", UserId $ user ^. #id)]
          messages  = Map.empty

  void $ createInstance ac "name" reusedDocumentProcess mapping
  clientError <-
    assertLeft "creating a second instance with the same document"
      $ createInstance ac "name" reusedDocumentProcess mapping
  assertIsJsonError clientError
