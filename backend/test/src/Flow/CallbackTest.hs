{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Flow.CallbackTest
  ( tests
  , withTestCallbackProcessor
  )
where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Reader.Class
import Data.Aeson hiding (Success)
import Database.PostgreSQL.Consumers
import Happstack.Server hiding (Cookie(..), Request(..), resp)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types hiding (POST)
import Network.Wai
import Network.Wai.Handler.Warp
import Optics.View
import Test.Framework
import Text.RawString.QQ
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map

import Callback.Consumer
import DB
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.CallbackPayload
import Flow.Client
import Flow.Core.Type.Callback
import Flow.Core.Type.Url
import Flow.IntegrationTest.Common
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Model.Types.Internal
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.Routes.Api hiding (Completed)
import Flow.TestUtil
import TestEnvSt.Internal (flowPort)
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import User.Lang
import Util.HasSomeUserInfo

tests :: TestEnvSt -> Test
tests env = testGroup "Flow callbacks"
                      [testThat "Receive final flow callback" env testSingleCallback]

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
|]

testSingleCallback :: TestEnv ()
testSingleCallback = do
  -- TODO Flow: Multiple parts of this function are really similar to
  -- SimpleCompleteProcess test and ComplexCompleteProcess test.
  -- Try to think of creating some functions which would reduced the
  -- boilerplate.
  (startedInstance, receivedPayloads) <- withTestCallbackProcessor $ do
    TestEnvSt {..} <- ask
    -- Prepare flow process author.
    user           <- instantiateRandomUser
    let authorEmail = getEmail user

    -- Prepare flow authentication sessions.
    oauth <- getToken (user ^. #id)

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
    let
      mapping = InstanceKeyValues
        { documents = Map.fromList [("doc", did)]
        , users     = Map.fromList
                        [("author", Email authorEmail), ("signatory", Email signatoryEmail)]
        , messages  = mempty
        }

    -- Flow stuff runs in another DB session so we need to commit before calling
    -- any flow related functions.
    commit

    template <- assertRight "create template" . request $ createTemplate
      createTemplateData
    let tid = id (template :: GetCreateTemplate)
    void . assertRight "commit template response" . request $ commitTemplate tid
    startedInstance <-
      assertRight "start template response"
      . request
      . startTemplate tid
      . CreateInstance Nothing (toTemplateParameters mapping)
      . Just
      $ Callback receiverAddress V1

    authorFlowLink <-
      assertJust "author's access link should be present"
      $      (startedInstance ^. #accessLinks)
      Map.!? "author"
    signatoryFlowLink <-
      assertJust "signatory's access link should be present"
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

    authorSignContext    <- mkContext defaultLang >>= authenticateContext authorEnv
    signatorySignContext <- mkContext defaultLang >>= authenticateContext signatoryEnv

    void $ mockDocTestRequestHelper
      authorSignContext
      POST
      [ ("fields"           , inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
      (docApiV2SigSign did authorSigLinkId)
      200

    void $ mockDocTestRequestHelper
      signatorySignContext
      POST
      [ ("fields"           , inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
      (docApiV2SigSign did signatorySigLinkId)
      200
    pure startedInstance

  assertEqual "Only one callback should be received" 1 $ length receivedPayloads
  assertEqual "Instance Id should match" (startedInstance ^. #id) . flowInstanceId $ head
    receivedPayloads
  assertEqual "Result should be successful" Completed . event $ head receivedPayloads


receiverPort :: Int
receiverPort = 31846

receiverAddress :: Url
receiverAddress = Url $ "http://localhost:" <> showt receiverPort

-- | Start the warp server that receives callbacks, then the callback processor
-- so that it can send callbacks to the server, which collects their payloads.
withTestCallbackProcessor :: TestEnv r -> TestEnv (r, [FlowCallbackEventV1Envelope])
withTestCallbackProcessor action = do
  mv                    <- newMVar []
  reqManager            <- liftBase newTlsManager
  ConnectionSource pool <- gview #connSource
  withAsync (liftBase $ runReceiver mv) $ \_ -> do
    -- Process multiple callbacks in parallel to make sure we're not messing up
    -- processing of dependent callbacks, which should be processed one by one.
    let callbacks = callbackConsumer runDB reqManager 10
    res <- finalize (runConsumer callbacks pool) $ do
      res <- action
      -- Gotta commit so that dispatcher sees db updates
      commit
      -- Idle signal doesn't work here, because we want to potentially process
      -- multiple callbacks here, so just pause for a second.
      threadDelay 1000000
      pure res
    payload <- reverse <$> readMVar mv
    pure (res, payload)
  where
    runDB m = do
      res <- m
      -- Gotta commit so that dispatcher sees db updates
      commit
      pure res

    runReceiver mv = run receiverPort $ \req respondWith -> do
      payload <-
        fromMaybe (unexpectedError "invalid payload")
        .   decode @FlowCallbackEventV1Envelope
        .   BSL.fromStrict
        <$> getRequestBodyChunk req
      modifyMVar_ mv $ \payloads -> return $! payload : payloads
      respondWith $ responseLBS status200 [] BSL.empty
