module OAuth (oauthTest) where

--import Test.HUnit (Assertion)
import Test.Framework
--import Test.Framework.Providers.HUnit
import TestKontra
import TestingUtil
import Test.QuickCheck
import Network.URI
import Data.Maybe

import MagicHash
import OAuth.Model
import User.Model
import DB

--import qualified Log

oauthTest :: TestEnvSt -> Test
oauthTest env = testGroup "OAuth" [
  testThat "CreateAPIToken makes readable token." env testCreateAPIToken,
  testThat "DeleteAPIToken does delete it." env testDeleteAPIToken,
  testThat "RequestTempCredentials security." env testRTCSecurity,
  testThat "VerifyCredentials security." env testVerifyCredentials
  ]

-- test model

testCreateAPIToken :: TestEnv ()
testCreateAPIToken = do
  singleuser <- addNewRandomUser
  r <- dbUpdate $ CreateAPIToken (userid singleuser)
  assertBool "CreateAPIToken did not return True." r
  ls <- dbQuery $ GetAPITokensForUser (userid singleuser)
  assertBool "GetAPITokensForUser did not return the token just created (first user)." $ length ls == 1
  seconduser <- addNewRandomUser
  r2 <- dbUpdate $ CreateAPIToken (userid seconduser)
  assertBool "CreateAPIToken did not return True." r2
  ls2 <- dbQuery $ GetAPITokensForUser (userid seconduser)
  assertBool "GetAPITokensForUser did not return the token just created (second user)." $ length ls2 == 1
  ls' <- dbQuery $ GetAPITokensForUser (userid singleuser)
  assertBool "GetAPITokensForUser should return 1 token." $ length ls' == 1
  _ <- dbUpdate $ CreateAPIToken (userid singleuser)
  ls'' <- dbQuery $ GetAPITokensForUser (userid singleuser)
  assertBool "GetAPITokensForUser should return 2 tokens." $ length ls'' == 2
  
  let loop = do
        uid <- rand 10 arbitrary
        if uid /= (userid singleuser) && uid /= (userid seconduser)
          then return uid
          else loop
  nonuid <- loop
  r3 <- dbUpdate $ CreateAPIToken nonuid
  assertBool "CreateAPIToken for non-existing user did not return False." $ not r3
  
testDeleteAPIToken :: TestEnv ()
testDeleteAPIToken = do
  singleuser <- addNewRandomUser
  _ <- dbUpdate $ CreateAPIToken (userid singleuser)
  ls <- dbQuery $ GetAPITokensForUser (userid singleuser)
  assertBool "GetAPITokensForUser did not return the token just created." $ length ls == 1
  let (apitoken,_):_ = ls
  _ <- dbUpdate $ DeleteAPIToken (userid singleuser) apitoken
  ls' <- dbQuery $ GetAPITokensForUser (userid singleuser)
  assertBool "GetAPITokensForUser did not delete the token." $ length ls' == 0
  
testRTCSecurity :: TestEnv ()
testRTCSecurity = do
  -- api token must exist
  let noapitoken = APIToken 0 $ unsafeMagicHash 0
      noapisecret = unsafeMagicHash 0
  time <- rand 10 arbitrary
  mcr <- dbUpdate $ RequestTempCredentials (OAuthTempCredRequest { tcCallback = fromJust $ parseURI "http://www.google.com/"
                                                                 , tcAPIToken = noapitoken
                                                                 , tcAPISecret = noapisecret
                                                                 , tcPrivileges = [APIDocCreate]
                                                                 }) time
  assertBool "RequestTempCredentials: API Token that does not exist should return Nothing." $ isNothing mcr
  
  -- api secret must match api token
  user <- addNewRandomUser
  _ <- dbUpdate $ CreateAPIToken (userid user)
  ls <- dbQuery $ GetAPITokensForUser (userid user)
  assertBool "GetAPITokensForUser did not return the token just created." $ length ls == 1
  let (apitoken, apisecret):_ = ls
  mcr' <- dbUpdate $ RequestTempCredentials (OAuthTempCredRequest { tcCallback = fromJust $ parseURI "http://www.google.com/"
                                                                  , tcAPIToken = apitoken
                                                                  , tcAPISecret = noapisecret
                                                                  , tcPrivileges = [APIDocCreate]
                                                                  }) time
  assertBool "RequestTempCredentials: API Secret does not match API Token should return Nothing." $ isNothing mcr'
  
  -- privileges must not be empty
  mcr'' <- dbUpdate $ RequestTempCredentials (OAuthTempCredRequest { tcCallback = fromJust $ parseURI "http://www.google.com/"
                                                                   , tcAPIToken = apitoken
                                                                   , tcAPISecret = apisecret
                                                                   , tcPrivileges = []
                                                                   }) time
  assertBool "RequestTempCredentials: empty privileges should return nothing." $ isNothing mcr''
  
  -- privileges must not contain APIPersonal
  mcr''' <- dbUpdate $ RequestTempCredentials (OAuthTempCredRequest { tcCallback = fromJust $ parseURI "http://www.google.com/"
                                                                    , tcAPIToken = apitoken
                                                                    , tcAPISecret = apisecret
                                                                    , tcPrivileges = [APIDocCreate, APIPersonal]
                                                                    }) time
  assertBool "RequestTempCredentials: privileges containing APIPersonal should return nothing." $ isNothing mcr'''

testVerifyCredentials :: TestEnv ()
testVerifyCredentials = do
  -- temp token must exist
  user <- addNewRandomUser
  apiuser <- addNewRandomUser  
  let loop = do
        uid <- rand 10 arbitrary
        if uid /= (userid user) && uid /= (userid apiuser)
          then return uid
          else loop
  nonuid <- loop
 
  time <- rand 10 arbitrary
  mvc <- dbUpdate $ VerifyCredentials (APIToken 0 $ unsafeMagicHash 0) (userid user) time
  assertBool "VerifyCredentials: temp token that does not exist should return Nothing." $ isNothing mvc
  
  -- userid must exist
  _ <- dbUpdate $ CreateAPIToken (userid apiuser)
  (apitoken,apisecret):_ <- dbQuery $ GetAPITokensForUser (userid apiuser)
  mcr <- dbUpdate $ RequestTempCredentials (OAuthTempCredRequest { tcCallback = fromJust $ parseURI "http://www.google.com/"
                                                                 , tcAPIToken = apitoken
                                                                 , tcAPISecret = apisecret
                                                                 , tcPrivileges = [APIDocCreate]
                                                                 }) time
  case mcr of
    Nothing -> assertFailure "RequestTempCredentials should work!"
    Just (tok, _sec) -> do
      mvc' <- dbUpdate $ VerifyCredentials tok nonuid time
      assertBool "VerifyCredentials: user does not exist should return Nothing." $ isNothing mvc'