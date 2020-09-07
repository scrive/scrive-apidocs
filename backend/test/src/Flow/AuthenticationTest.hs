{-# LANGUAGE QuasiQuotes #-}
module Flow.AuthenticationTest (tests) where

import Network.HTTP.Types.Status
import Servant.Client
import Test.Framework
import Text.RawString.QQ
import qualified Data.Map as Map

import Auth.MagicHash
import Auth.Session
import DB
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import Flow.Client
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.OrphanTestInstances ()
import Flow.Process.Internal
import Flow.Routes.Api
import Flow.Server.Cookies
import Flow.TestUtil
import TestEnvSt.Internal
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra
import User.Types.User
import User.UserID
import qualified Auth.Model as AuthModel
import qualified Flow.Model.InstanceSession as Model

tests :: TestEnvSt -> Test
tests env = testGroup
  "Authentication"
  [ testThat "Account authentication with cookies" env testCookieAuthAccount
  , testThat "Invitation link login"               env testInvitationLinkLogin
  ]

-- TODO: Add tests for OAuth

testCookieAuthAccount :: TestEnv ()
testCookieAuthAccount = do
  user             <- instantiateRandomUser
  validAuthCookies <- AuthModel.insertNewSession flowTestCookieDomain
                                                 (Just . unUserID $ user ^. #id)
  commit

  let validSesID    = cookieSessionID . authCookieSession $ validAuthCookies
      validSesToken = cookieSessionToken . authCookieSession $ validAuthCookies
      validXToken   = authCookieXToken validAuthCookies

      validCreateTemplate =
        createTemplate
          . mkApiClient
          . Right
          $ (Just (SessionCookieInfo validSesID validSesToken), Just validXToken)

      invalidCreateTemplate1 =
        createTemplate
          . mkApiClient
          . Right
          $ (Just (SessionCookieInfo validSesID (unsafeMagicHash 666)), Just validXToken)

      invalidCreateTemplate2 =
        createTemplate
          . mkApiClient
          . Right
          $ ( Just (SessionCookieInfo validSesID validSesToken)
            , Just (unsafeMagicHash 666)
            )

      invalidCreateTemplate3 =
        createTemplate
          . mkApiClient
          . Right
          $ (Just (SessionCookieInfo validSesID validSesToken), Nothing)

      invalidCreateTemplate4 = createTemplate . mkApiClient $ Right (Nothing, Nothing)

      createTemplateData     = CreateTemplate "dummyname" "dummyprocess"

  void
    . assertRight "Request with valid cookies should succeed"
    . request
    $ validCreateTemplate createTemplateData

  void
    . assertLeft "Request with an invalid session token should fail"
    . request
    $ invalidCreateTemplate1 createTemplateData

  void
    . assertLeft "Request with an invalid XToken should fail"
    . request
    $ invalidCreateTemplate2 createTemplateData

  void
    . assertLeft "Request with missing XToken cookie should fail"
    . request
    $ invalidCreateTemplate3 createTemplateData

  void
    . assertLeft "Request with both cookies missing should fail"
    . request
    $ invalidCreateTemplate4 createTemplateData

process1 :: Process
process1 = Process [r|
dsl-version: "0.2.0"
stages:
  - initial:
      actions: []
      expect:
        signed-by:
          users: [signatory]
          documents: [doc1]
|]

-- TODO: This function is currently identical to addDocument in IntegrationTest.
-- However, we might want to produce a slightly different document here at some point.
-- Consider putting this function in TestUtil if that turns out not to be necessary.
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

testInvitationLinkLogin :: TestEnv ()
testInvitationLinkLogin = do
  user  <- instantiateRandomUser
  oauth <- getToken (user ^. #id)
  doc1  <- addDocument user
  commit

  let apiClient = mkApiClient (Left oauth)

  -- Create and start an instance
  instanceId <- getInstanceId
    $ createInstance apiClient "dummy" process1 (mapping (user ^. #id) doc1)
  instanceToken <- getInstanceAccessToken instanceId "signatory"

  -- Log in with an invitation link containing a magic hash
  -- We expect a 302 redirect. For Servant it's an error.
  let instanceOverviewMagicHash' =
        instanceOverviewMagicHash $ mkPageClient (Nothing, Nothing)
  env       <- getEnv managerSettingsNoRedirects
  mResponse <-
    fmap errorResponse
    . assertLeft "instanceOverviewMagicHash response"
    . requestWithEnv env
    $ instanceOverviewMagicHash' instanceId
                                 "signatory"
                                 (instanceToken ^. #hash)
                                 Nothing
                                 Nothing
  response <- assertJustAndExtract mResponse
  assertEqual "instanceOverviewMagicHash should respond with a redirect"
              302
              (statusCode . responseStatusCode $ response)

  -- If the link login is successful we should get auth cookies
  let mAuthCookies = readAuthCookies . toCookies $ responseSetCookieHeaders response
  AuthCookies {..} <- assertJustAndExtract mAuthCookies

  -- Use these cookies to access the instance overview page
  let instanceOverviewAuth =
        instanceOverview $ mkPageClient (Just authCookieSession, Just authCookieXToken)
  void $ assertRight
    "instanceOverview with auth cookies should succeed"
    (request . instanceOverviewAuth instanceId "signatory" $ Just flowTestCookieDomain)

  -- Without the auth cookies the overview page should be inaccessible
  let instanceOverviewNoAuth = instanceOverview $ mkPageClient (Nothing, Nothing)
  void $ assertLeft
    "instanceOverview without auth cookies should fail"
    (request . instanceOverviewNoAuth instanceId "signatory" $ Just flowTestCookieDomain)

  where
    mapping uid doc = InstanceKeyValues documents users messages
      where
        documents = Map.fromList [("doc1", documentid doc)]
        users     = Map.fromList [("signatory", UserId uid)]
        messages  = Map.empty

    getInstanceId createInstanceAction = do
      flowInstance <- assertRight "create instance" createInstanceAction
      pure $ id (flowInstance :: GetInstance)

    getInstanceAccessToken instanceId userName = do
      allTokens <- Model.selectInstanceAccessTokens instanceId
      let tokens = filter (\at -> at ^. #userName == userName) allTokens
      case tokens of
        (t : _) -> pure t
        []      -> fail "No instance access token found"

