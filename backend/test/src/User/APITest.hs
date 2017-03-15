{-# LANGUAGE OverloadedStrings #-}
module User.APITest (userAPITests) where

import Data.Aeson
import Happstack.Server
import Test.Framework
import qualified Data.HashMap.Strict as H

import DB
import Doc.SignatoryLinkID ()
import KontraPrelude
import TestingUtil
import TestKontra as T
import User.API
import User.Email
import User.Model

userAPITests :: TestEnvSt -> Test
userAPITests env = testGroup "UserAPI"
  [ testThat "Test User API Create Login Link" env testUserLoginAndGetSession
  ]

testUserLoginAndGetSession :: TestEnv ()
testUserLoginAndGetSession = do
  -- create a user
  let password = "Secret Password!"
  randomUser <- addNewRandomUserWithPassword password
  ctx <- mkContext def
  req1 <- mkRequest GET
    [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
    , ("password", inText password)
    ]
  -- get access tokens using an email and password
  (res1, _) <- runTestKontra req1 ctx $ apiCallGetUserPersonalToken
  let Just (Object respObject1) = decode (rsBody res1) :: Maybe Value
      Just (String apitoken) = H.lookup "apitoken" respObject1
      Just (String apisecret) = H.lookup "apisecret" respObject1
      Just (String accesstoken) = H.lookup "accesstoken" respObject1
      Just (String accesssecret) = H.lookup "accesssecret" respObject1

  -- use access tokens to log in and get cookie-like session id
  req2 <- mkRequest GET [ ("personal_token", inTextBS $ rsBody res1) ]
  (res2, _) <- runTestKontra req2 ctx $ apiCallLoginUserAndGetSession
  let Just (Object respObject2) = decode (rsBody res2) :: Maybe Value
      Just (String session_id) = H.lookup "session_id" respObject2
  assertBool ("We should get an ok status in JSON") (session_id /= "")

  -- switch API and Access secrets to get bad input in correct format
  let badtokens = encode . Object . H.fromList $
        [ ("apitoken"    , String apitoken    )
        , ("apisecret"   , String accesssecret)
        , ("accesstoken" , String accesstoken )
        , ("accesssecret", String apisecret   )
        ]
  req3 <- mkRequest GET [ ("personal_token", inTextBS badtokens) ]
  (res3, _) <- runTestKontra req3 ctx $ apiCallLoginUserAndGetSession
  let Just (Object respObject3) = decode (rsBody res3) :: Maybe Value
      Just (String errorType) = H.lookup "error_type" respObject3
  assertEqual ("We should get an error status in JSON") "invalid_authorisation" errorType
