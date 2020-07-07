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

  let
    validSesID    = sesID session
    validSesToken = sesToken session
    validXToken   = sesCSRFToken session

    TemplateClient { createTemplate = validCreateTemplate } =
      mkTemplateClient
        . Right
        $ (Just (SessionCookieInfo validSesID validSesToken), Just validXToken)

    TemplateClient { createTemplate = invalidCreateTemplate1 } =
      mkTemplateClient
        . Right
        $ (Just (SessionCookieInfo validSesID (unsafeMagicHash 666)), Just validXToken)

    TemplateClient { createTemplate = invalidCreateTemplate2 } =
      mkTemplateClient
        . Right
        $ (Just (SessionCookieInfo validSesID validSesToken), Just (unsafeMagicHash 666))

    TemplateClient { createTemplate = invalidCreateTemplate3 } =
      mkTemplateClient
        . Right
        $ (Just (SessionCookieInfo validSesID validSesToken), Nothing)

    TemplateClient { createTemplate = invalidCreateTemplate4 } =
      mkTemplateClient $ Right (Nothing, Nothing)

    createTemplateData = CreateTemplate "dummyname" "dummyprocess"

  env <- getEnv

  void
    . assertRight "Request with valid cookies should succeed"
    . request env
    $ validCreateTemplate createTemplateData

  void
    . assertLeft "Request with an invalid session token should fail"
    . request env
    $ invalidCreateTemplate1 createTemplateData

  void
    . assertLeft "Request with an invalid XToken should fail"
    . request env
    $ invalidCreateTemplate2 createTemplateData

  void
    . assertLeft "Request with missing XToken cookie should fail"
    . request env
    $ invalidCreateTemplate3 createTemplateData

  void
    . assertLeft "Request with both cookies missing should fail"
    . request env
    $ invalidCreateTemplate4 createTemplateData

