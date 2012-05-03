module OAuth (oauthTest) where

--import Test.HUnit (Assertion)
import Test.Framework
--import Test.Framework.Providers.HUnit
import TestKontra
import TestingUtil
import Test.QuickCheck

import OAuth.Model
import User.Model
import DB

--import qualified Log

oauthTest :: TestEnvSt -> Test
oauthTest env = testGroup "OAuth" [
  testThat "CreateAPIToken makes readable token." env testCreateAPIToken,
  testThat "DeleteAPIToken does delete it." env testDeleteAPIToken
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
  