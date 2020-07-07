{-# LANGUAGE DuplicateRecordFields #-}

module Flow.AuthenticationTest (tests) where

import Test.Framework

import Auth.MagicHash
import Auth.Session
import Flow.Api
import Flow.Client
import Flow.OrphanTestInstances ()
import Flow.TestUtil
import Session.Types (Session(..))
import SessionsTest (insertNewSession)
import TestEnvSt.Internal
import TestingUtil hiding (assertLeft, assertRight)
import TestKontra

tests :: TestEnvSt -> Test
tests env = testGroup "Authentication"
                      [testThat "Cookie auth/sessions are working" env testCookieAuth]

-- TODO: Add tests for OAuth

testCookieAuth :: TestEnv ()
testCookieAuth = do
  user         <- instantiateRandomUser
  (session, _) <- insertNewSession (user ^. #id)

  let validSesID    = sesID session
      validSesToken = sesToken session
      validXToken   = sesCSRFToken session

      validCreateTemplate =
        createTemplate
          . mkClient
          . Right
          $ (Just (SessionCookieInfo validSesID validSesToken), Just validXToken)

      invalidCreateTemplate1 =
        createTemplate
          . mkClient
          . Right
          $ (Just (SessionCookieInfo validSesID (unsafeMagicHash 666)), Just validXToken)

      invalidCreateTemplate2 =
        createTemplate
          . mkClient
          . Right
          $ ( Just (SessionCookieInfo validSesID validSesToken)
            , Just (unsafeMagicHash 666)
            )

      invalidCreateTemplate3 =
        createTemplate
          . mkClient
          . Right
          $ (Just (SessionCookieInfo validSesID validSesToken), Nothing)

      invalidCreateTemplate4 = createTemplate . mkClient $ Right (Nothing, Nothing)

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
  where mkClient authDataAccount = mkApiClient authDataAccount (Nothing, Nothing)
