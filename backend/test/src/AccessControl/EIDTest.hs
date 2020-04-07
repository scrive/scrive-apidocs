{-# LANGUAGE OverloadedStrings #-}
module AccessControl.EIDTest (accessControlEIDTests) where

import Control.Monad.Catch (try)
import Happstack.Server
import Test.Framework

import AccessControl.Model
import AccessControl.Types
import DB
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Tokens.Model
import EID.CGI.GRP.Config
import EID.CGI.GRP.Control
import KontraError
import Session.Model
import TestingUtil
import TestKontra
import User.Model

accessControlEIDTests :: TestEnvSt -> Test
accessControlEIDTests env = testGroup
  "AccessControlEID"
  [ testThat
      "Authenticating/Signing with Swedish BankID fails if impersonating another user group without the necessary permissions"
      env
      testImpersonateAuthAndSignFailWithoutPermissions
  ]

testImpersonateAuthAndSignFailWithoutPermissions :: TestEnv ()
testImpersonateAuthAndSignFailWithoutPermissions = do
  user <- instantiateRandomUser

  ctx  <- do
    defctx <- mkContext defaultLang
    let cgiGrpConfig = CgiGrpConfig "" "" "" ""
    return . set #maybeUser (Just user) . set #cgiGrpConfig (Just cgiGrpConfig) $ defctx

  (did, slid) <- do
    mdoc <- testDocApiV2New' ctx
    return (getMockDocId mdoc, getMockDocSigLinkId 1 mdoc)

  do  -- set `user_group_to_impersonate_for_eid` field
    ug <- instantiateRandomUserGroup
    let ugid = ug ^. #id  -- user group to be impersonated (not the author's user group!)
    let params =
          [ ( "document"
            , inText
              $  "{\"experimental_features\": {\"user_group_to_impersonate_for_eid\":\""
              <> showt ugid
              <> "\"}}"
            )
          ]
    -- assign EidImpersonator role for the duration of the update call
    Just (AccessRoleUser roleid _ _) <-
      dbUpdate . AccessControlCreateForUser (user ^. #id) $ EidImpersonatorAR ugid
    void $ mockDocTestRequestHelper ctx POST params (docApiV2Update did) 200
    void $ testDocApiV2Start' ctx did  -- start signing process
    void . dbUpdate $ AccessControlRemoveRole roleid


  do  -- set up document for signing with Swedish BankID
    let params =
          [ ("authentication_type", inText "se_bankid")
          , ("personal_number"    , inText "0000000000")
          ]
    void $ testRequestHelper ctx
                             POST
                             params
                             (docApiV2SigSetAuthenticationToSign did slid)
                             200

  -- start session
  req       <- mkRequest GET []
  (_, ctx') <- runTestKontra req ctx $ do
    sid <- getNonTempSessionID
    randomUpdate $ AddDocumentSession sid slid

  emptyPOSTRequest <- mkRequestWithHeaders POST [] []

  resSign <- try . void . runTestKontra emptyPOSTRequest ctx' $ handleSignRequest did slid
  assertEqual
    "Swedish BankID Signing should fail when impersonating another\
              \ user group without sufficient privileges"
    (Left Respond404)
    resSign

  resAuth <- try . void . runTestKontra emptyPOSTRequest ctx' $ handleAuthRequest did slid
  assertEqual
    "Swedish BankID Authenticating should fail when impersonating another\
              \ user group without sufficient privileges"
    (Left Respond404)
    resAuth
