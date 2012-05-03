module OAuth (oauthTest) where

--import Test.HUnit (Assertion)
import Test.Framework
--import Test.Framework.Providers.HUnit
import TestKontra
import TestingUtil

import OAuth.Model
import User.Model
import DB

oauthTest :: TestEnvSt -> Test
oauthTest env = testGroup "OAuth" [
  testThat "CreateAPIToken makes readable token." env testCreateAPIToken
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
  
