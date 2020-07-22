{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedLabels #-}

module Flow.IntegrationTest (tests) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson
import Data.Binary.Builder
import Data.Proxy
import GHC.Conc
import Happstack.Server hiding (Cookie(..), Request(..), resp)
import Network.HTTP.Client hiding (Proxy, Request, responseBody)
import Network.HTTP.Types (http11, methodGet)
import Optics hiding (mapping)
import Servant.API.ContentTypes
import Servant.Client
import Servant.Client.Core.Request
import Servant.Client.Internal.HttpClient
import Test.Framework
import Text.RawString.QQ
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Servant.Client as Servant
import qualified Test.HUnit as T

import Auth.Session
import Context.Internal
import DB hiding (JSON)
import Doc.API.V2.Calls.DocumentGetCalls
import Doc.API.V2.Calls.SignatoryCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Model.Update
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.Client
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Model.Types.Internal
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.Routes.Api
import Flow.Routes.Types
import Flow.Server.Cookies
import Flow.TestUtil
import MinutesTime
import Session.Model
import Session.Types
import TestCron
import TestEnvSt.Internal (flowPort)
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import User.Lang
import User.Types.User
import Util.Actor
import Util.HasSomeUserInfo
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
  , testThat "Verify endpoint"          env testVerifyEndpoint
  , testThat "Complete flow process"    env testCompleteFlowProcess
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
  AuthCookies {..} <- AuthModel.insertNewSession flowTestCookieDomain Nothing

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
              (Map.keys $ accessLinks (instance2 :: GetInstance))

  -- View instance as "signatory"
  let ParticipantApiClient {..} =
        mkParticipantApiClient (Just authCookieSession, Just authCookieXToken)
  Model.upsertInstanceSession (cookieSessionID authCookieSession) iid "signatory"
  commit
  instanceView <- assertRight "view instance response" . request $ getInstanceView
    iid
    Nothing
  assertEqual "view instance: id in response" iid $ id (instanceView :: GetInstanceView)
  assertEqual "view instance: there are 2 document actions" 2
    $ length (actions instanceView)

  void
    . assertLeft "view instance with incorrect Host should fail"
    . request
    $ getInstanceView iid (Just "wrongdomain.com")

  -- Action links
  baseUrl <- getBaseUrl . actionLink . head $ actions instanceView
  assertEqual "action link has correct base url when Host is Nothing"
              ("http://" <> flowTestCookieDomain)
              baseUrl

  instanceView2 <- assertRight "view instance response" . request $ getInstanceView
    iid
    (Just flowTestCookieDomain)
  baseUrl2 <- getBaseUrl . actionLink . head $ actions instanceView2
  assertEqual "action link has correctbase url when Host is Just"
              ("http://" <> flowTestCookieDomain)
              baseUrl2

getBaseUrl :: MonadThrow m => Url -> m Text
getBaseUrl (Url u) = do
  url <- parseBaseUrl $ Text.unpack u
  pure . Text.pack . showBaseUrl $ url { baseUrlPath = "" }

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
  assertIsJsonError clientError


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

processInvalid :: Process
processInvalid = Process [r|
dsl-version: "0.1.0"
stages:
  - initial:
      actions: []
      expect:
        signed-by:
sdfgsdfg          users: [signatory]
          documents: [doc1 doc2]
  - notification:
      actions:dfgdfsg
        - notify:
            users: [watcher
            message: was-signed
      expect: {}
|]

testVerifyEndpoint :: TestEnv ()
testVerifyEndpoint = do
  -- Verify does not need Auth to use
  let ApiClient {..} = mkApiClient $ Right (Nothing, Nothing)
      blankReq       = request $ validateTemplate ""
      failureReq     = request $ validateTemplate processInvalid
      successReq     = request $ validateTemplate processZero
  assertIsJsonError =<< assertLeft "validating blank DSL" blankReq
  assertIsJsonError =<< assertLeft "validating invalid DSL" failureReq
  void $ assertRight "validating valid DSL" successReq

assertIsJsonError :: ClientError -> TestEnv ()
assertIsJsonError = assert . hasJsonBody
  where
    isJustObject :: Maybe Value -> Bool
    isJustObject = \case
      Just (Object _) -> True
      _               -> False
    hasJsonBody :: ClientError -> Bool
    hasJsonBody = \case
      FailureResponse _ resp -> isJustObject . decode $ responseBody resp
      _ -> False

processCompleteFlow :: Process
processCompleteFlow = Process [r|
dsl-version: "0.1.0"
stages:
  - initial:
      actions: []
      expect:
        signed-by:
          users: [author, signatory]
          documents: [doc]
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
  let authSigLinkId      = signatorylinkid authorSigLink
  let signatorySigLinkId = signatorylinkid signatorySigLink
  let signatoryEmail     = getEmail signatorySigLink

  let createTemplateData = CreateTemplate "name" processCompleteFlow
  let did                = documentid doc
  let mapping = InstanceKeyValues
        { documents = Map.fromList [("doc", did)]
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
    . assertRight "can't authenticate with author flow link"
    . callFlowMagicHashLink authorEnv
    $ unUrl authorFlowLink
  void
    . assertRight "can't authenticate with signatory flow link"
    . callFlowMagicHashLink signatoryEnv
    $ unUrl signatoryFlowLink

  authorViewBeforeSign <-
    assertRight "get author instance view data before signing"
    . requestWithEnv authorEnv
    $ getInstanceView id Nothing
  signatoryViewBeforeSign <-
    assertRight "get signatory instance view data before signing"
    . requestWithEnv signatoryEnv
    $ getInstanceView id Nothing

  assertEqual "author's state should be empty at the beginning" []
    $ view (#state % #documents) authorViewBeforeSign
  assertEqual "signatory's state should be empty at the beginning" []
    $ view (#state % #documents) signatoryViewBeforeSign

  assertEqual
      "unexpected author's action"
      (  Just
      .  InstanceUserAction Sign did authSigLinkId
      .  Url
      $  "http://localhost:"
      <> showt flowPort
      <> "/s/"
      <> showt did
      <> "/"
      <> showt authSigLinkId
      )
    $ preview (#actions % ix 0) authorViewBeforeSign
  assertEqual
      "unexpected signatory's action"
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
    (docApiV2SigSign did authSigLinkId)
    200

  authorViewAfterSign <-
    assertRight "get author instance view data before signing"
    . requestWithEnv authorEnv
    $ getInstanceView id Nothing

  assertEqual "author's state should contain one document"
              (Just $ InstanceUserDocument did Signed authSigLinkId)
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
    (docApiV2SigSign did authSigLinkId)
    409

  _ <- mockDocTestRequestHelper
    signatorySignContext
    POST
    [ ("fields"           , inText "[]")
    , ("accepted_author_attachments", inText "[]")
    , ("consent_responses", inText "[]")
    ]
    (docApiV2SigSign did signatorySigLinkId)
    200

  signatoryViewAfterSign <-
    assertRight "get signatory instance view data after signing"
    . requestWithEnv signatoryEnv
    $ getInstanceView id Nothing

  assertEqual "signatory's state should contain one document"
              (Just $ InstanceUserDocument did Signed signatorySigLinkId)
    $ preview (#state % #documents % ix 0) signatoryViewAfterSign

  assertEqual "there should be no left action for signatory" []
    $ view #actions signatoryViewAfterSign

  cronContext <- mkContextWithUser defaultLang user
  runTestCronUntilIdle cronContext
  mockDocSigned <- mockDocTestRequestHelper cronContext GET [] (docApiV2Get did) 200

  assertEqual "document status after signing should match"
              Closed
              (getMockDocStatus mockDocSigned)


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
  where
    mkEnvForUser :: TestEnv ClientEnv
    mkEnvForUser = do
      TestEnvSt {..} <- ask
      mgr            <- liftIO $ newManager defaultManagerSettings
      url            <- parseBaseUrl "localhost"
      cookieJar      <- liftIO . newTVarIO $ createCookieJar []
      pure . setCookieJar cookieJar . mkClientEnv mgr $ url { baseUrlPort = flowPort }

    setCookieJar :: TVar CookieJar -> ClientEnv -> ClientEnv
    setCookieJar jar env = env { cookieJar = Just jar }

    authenticateContext :: ClientEnv -> Context -> TestEnv Context
    authenticateContext ClientEnv {..} context = do
      TestEnvSt {..} <- ask
      cookies        <-
        assertJust' "cookie jar not present" cookieJar
        >>= fmap (fmap toTransformCookie . destroyCookieJar)
        .   liftIO
        .   readTVarIO
      AuthCookies {..} <- assertJust' "authentication cookies are not set"
        $ readAuthCookies cookies
      session <-
        getSession (cookieSessionID authCookieSession)
                   (cookieSessionToken authCookieSession)
                   ("localhost:" <> showt flowPort)
          >>= assertJust' "can't create kontrakcja session"
      muser    <- getUserFromSession session
      mpaduser <- getPadUserFromSession session

      pure $ context { sessionID    = sesID session
                     , xToken       = sesCSRFToken session
                     , maybeUser    = muser
                     , maybePadUser = mpaduser
                     }

    assertJust' :: MonadIO m => String -> Maybe a -> m a
    assertJust' _   (Just v) = pure v
    assertJust' msg Nothing  = liftIO $ T.assertFailure msg

    toTransformCookie :: Cookie -> (BSC.ByteString, BSC.ByteString)
    toTransformCookie Cookie {..} = (cookie_name, cookie_value)

    callFlowMagicHashLink
      :: MonadIO m => ClientEnv -> Text -> m (Either ClientError Servant.Response)
    callFlowMagicHashLink env link =
      requestWithEnv env (mkMagicRequest link >>= performRequest)

    mkMagicRequest :: MonadThrow m => Text -> m Request
    mkMagicRequest link = do
      BaseUrl {..} <- parseBaseUrl $ Text.unpack link
      pure $ Request
        { requestPath        = fromByteString $ BSC.pack baseUrlPath
        , requestQueryString = mempty
        , requestBody        = Nothing
        , requestAccept      = Seq.fromList
                                 [contentType $ Proxy @JSON, contentType $ Proxy @HTML]
        , requestHeaders     = mempty
        , requestHttpVersion = http11
        , requestMethod      = methodGet
        }

    unUrl :: Url -> Text
    unUrl (Url link) = link
