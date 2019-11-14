module OAuth (oauthTest) where

import Network.URI
import Test.Framework
import Test.QuickCheck

import DB
import MagicHash
import MinutesTime
import OAuth.Model
import TestingUtil
import TestKontra

oauthTest :: TestEnvSt -> Test
oauthTest env = testGroup
  "OAuth"
  [ testThat "CreateAPIToken makes readable token." env testCreateAPIToken
  , testThat "DeleteAPIToken does delete it." env testDeleteAPIToken
  , testThat "RequestTempCredentials security." env testRTCSecurity
  , testThat "VerifyCredentials security."    env testVerifyCredentials
  , testThat "RequestAccessToken security."   env testRequestAccessToken
  , testThat "OAuthFlow."                     env testOAuthFlow
  , testThat "OAuthFlow with deny"            env testOAuthFlowWithDeny
  , testThat "GetGrantedPrivileges"           env testGetGrantedPrivileges
  , testThat "DeletePrivileges"               env testDeletePrivileges
  , testThat "DeletePrivilege"                env testDeletePrivilege
  , testThat "PersonalToken"                  env testPersonalToken
  , testThat "RecentPersonalToken"            env testRecentPersonalToken
  ]

-- test model

testCreateAPIToken :: TestEnv ()
testCreateAPIToken = do
  singleuser <- addNewRandomUser
  r          <- dbUpdate $ CreateAPIToken (singleuser ^. #id)
  assertBool "CreateAPIToken did not return True." r
  ls <- dbQuery $ GetAPITokensForUser (singleuser ^. #id)
  assertBool "GetAPITokensForUser did not return the token just created (first user)."
    $  length ls
    == 1
  seconduser <- addNewRandomUser
  r2         <- dbUpdate $ CreateAPIToken (seconduser ^. #id)
  assertBool "CreateAPIToken did not return True." r2
  ls2 <- dbQuery $ GetAPITokensForUser (seconduser ^. #id)
  assertBool "GetAPITokensForUser did not return the token just created (second user)."
    $  length ls2
    == 1
  ls' <- dbQuery $ GetAPITokensForUser (singleuser ^. #id)
  assertBool "GetAPITokensForUser should return 1 token." $ length ls' == 1
  void $ dbUpdate $ CreateAPIToken (singleuser ^. #id)
  ls'' <- dbQuery $ GetAPITokensForUser (singleuser ^. #id)
  assertBool "GetAPITokensForUser should return 2 tokens." $ length ls'' == 2

  let loop = do
        uid <- rand 10 arbitrary
        if uid /= (singleuser ^. #id) && uid /= (seconduser ^. #id)
          then return uid
          else loop
  nonuid <- loop
  r3     <- dbUpdate $ CreateAPIToken nonuid
  assertBool "CreateAPIToken for non-existing user did not return False." $ not r3

testDeleteAPIToken :: TestEnv ()
testDeleteAPIToken = do
  singleuser <- addNewRandomUser
  void $ dbUpdate $ CreateAPIToken (singleuser ^. #id)
  ls <- dbQuery $ GetAPITokensForUser (singleuser ^. #id)
  assertBool "GetAPITokensForUser did not return the token just created." $ length ls == 1
  let (apitoken, _) : _ = ls
  void $ dbUpdate $ DeleteAPIToken (singleuser ^. #id) apitoken
  ls' <- dbQuery $ GetAPITokensForUser (singleuser ^. #id)
  assertBool "GetAPITokensForUser did not delete the token." $ length ls' == 0

testRTCSecurity :: TestEnv ()
testRTCSecurity = do
  -- api token must exist
  let noapitoken  = APIToken 0 $ unsafeMagicHash 0
      noapisecret = unsafeMagicHash 0
  time <- rand 10 arbitrary
  mcr  <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = noapitoken
                          , tcAPISecret  = noapisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time
  assertBool
      "RequestTempCredentials: API Token that does not exist should return Nothing."
    $ isNothing mcr

  -- api secret must match api token
  user <- addNewRandomUser
  void $ dbUpdate $ CreateAPIToken (user ^. #id)
  ls <- dbQuery $ GetAPITokensForUser (user ^. #id)
  assertBool "GetAPITokensForUser did not return the token just created." $ length ls == 1
  let (apitoken, apisecret) : _ = ls
  mcr' <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = noapisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time
  assertBool
      "RequestTempCredentials: API Secret does not match API Token should return Nothing."
    $ isNothing mcr'

  -- privileges must not be empty
  mcr'' <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = []
                          }
    )
    time
  assertBool "RequestTempCredentials: empty privileges should return nothing."
    $ isNothing mcr''

  -- privileges must not contain APIPersonal
  mcr''' <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCreate, APIPersonal]
                          }
    )
    time
  assertBool
      "RequestTempCredentials: privileges containing APIPersonal should return nothing."
    $ isNothing mcr'''

testVerifyCredentials :: TestEnv ()
testVerifyCredentials = do
  -- temp token must exist
  user    <- addNewRandomUser
  apiuser <- addNewRandomUser
  let loop = do
        uid <- rand 10 arbitrary
        if uid /= (user ^. #id) && uid /= (apiuser ^. #id) then return uid else loop
  nonuid <- loop

  time <- rand 10 arbitrary
  mvc <- dbUpdate $ VerifyCredentials (APIToken 0 $ unsafeMagicHash 0) (user ^. #id) time
  assertBool "VerifyCredentials: temp token that does not exist should return Nothing."
    $ isNothing mvc

  -- userid must exist
  void $ dbUpdate $ CreateAPIToken (apiuser ^. #id)
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser (apiuser ^. #id)
  mcr <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time
  case mcr of
    Nothing          -> assertFailure "RequestTempCredentials should work!"
    Just (tok, _sec) -> do
      mvc' <- dbUpdate $ VerifyCredentials tok nonuid time
      assertBool "VerifyCredentials: user does not exist should return Nothing."
        $ isNothing mvc'

  -- cannot verify twice on same token
  (apitoken', apisecret') : _ <- dbQuery $ GetAPITokensForUser (apiuser ^. #id)
  mcr' <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken'
                          , tcAPISecret  = apisecret'
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time
  case mcr' of
    Nothing        -> assertFailure "RequestTempCredentials should work!"
    Just (tok', _) -> do
      user1 <- addNewRandomUser
      void $ dbUpdate $ VerifyCredentials tok' (user1 ^. #id) time
      user2 <- addNewRandomUser
      mvc'' <- dbUpdate $ VerifyCredentials tok' (user2 ^. #id) time
      assertBool "VerifyCredentials: second user verifying should not work."
        $ isNothing mvc''

testRequestAccessToken :: TestEnv ()
testRequestAccessToken = do
  -- setup
  apiclient <- addNewRandomUser
  user      <- addNewRandomUser
  time      <- rand 10 arbitrary
  void $ dbUpdate $ CreateAPIToken (apiclient ^. #id)
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser (apiclient ^. #id)
  Just (tok, sec)           <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time

  Just (_, ver) <- dbUpdate $ VerifyCredentials tok (user ^. #id) time

  -- api token must exist
  mrat1         <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = (APIToken 0 $ unsafeMagicHash 0)
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = ver
                       }
    )
    time

  assertBool "RequestAccessToken: when APIToken does not exist should return Nothing."
    $ isNothing mrat1

  -- api secret must match
  mrat2 <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = unsafeMagicHash 0
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = ver
                       }
    )
    time

  assertBool "RequestAccessToken: when APISecret does not exist should return Nothing."
    $ isNothing mrat2

  -- temp token must exist
  mrat3 <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = (APIToken 0 $ unsafeMagicHash 0)
                       , trTempSecret = sec
                       , trVerifier   = ver
                       }
    )
    time

  assertBool "RequestAccessToken: when Temp Token does not match should return Nothing."
    $ isNothing mrat3

  -- temp secret must match
  mrat4 <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = unsafeMagicHash 0
                       , trVerifier   = ver
                       }
    )
    time

  assertBool "RequestAccessToken: when Temp Secret does not match should return Nothing."
    $ isNothing mrat4

  -- verifier must match
  mrat5 <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = unsafeMagicHash 0
                       }
    )
    time

  assertBool "RequestAccessToken: when verifier does not match should return Nothing."
    $ isNothing mrat5

testOAuthFlow :: TestEnv ()
testOAuthFlow = do
  apiclient <- addNewRandomUser
  user      <- addNewRandomUser
  time      <- rand 10 arbitrary
  void $ dbUpdate $ CreateAPIToken (apiclient ^. #id)
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser (apiclient ^. #id)
  Just (tok, sec)           <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time

  Just (_, ver) <- dbUpdate $ VerifyCredentials tok (user ^. #id) time
  Just (t, s  ) <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = ver
                       }
    )
    time
  mup <- dbQuery $ GetUserIDForAPIWithPrivilege apitoken apisecret t s [APIDocCreate]
  assertBool "GetUserIDForAPIWithPrivilege: returned Nothing but should have worked"
    $ isJust mup

testOAuthFlowWithDeny :: TestEnv ()
testOAuthFlowWithDeny = do
  apiclient <- addNewRandomUser
  time      <- rand 10 arbitrary
  void $ dbUpdate $ CreateAPIToken (apiclient ^. #id)
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser (apiclient ^. #id)
  Just (tok, sec)           <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time

  Just _ <- dbUpdate $ DenyCredentials tok time
  mup    <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = unsafeMagicHash 0
                       }
    )
    time
  assertBool "RequestAccessToken should return Nothing" $ isNothing mup

testGetGrantedPrivileges :: TestEnv ()
testGetGrantedPrivileges = do
  apiclient <- addNewRandomUser
  user      <- addNewRandomUser
  time      <- rand 10 arbitrary
  void $ dbUpdate $ CreateAPIToken (apiclient ^. #id)
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser (apiclient ^. #id)
  Just (tok, sec)           <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time

  Just (_, ver) <- dbUpdate $ VerifyCredentials tok (user ^. #id) time
  Just (t, _  ) <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = ver
                       }
    )
    time
  [(tid, _, [APIDocCreate])] <- dbQuery $ GetGrantedPrivileges (user ^. #id)

  assertBool "GetGrantedPrivileges: id should be the same" $ tid == atID t

testDeletePrivileges :: TestEnv ()
testDeletePrivileges = do
  apiclient <- addNewRandomUser
  user      <- addNewRandomUser
  time      <- rand 10 arbitrary
  void $ dbUpdate $ CreateAPIToken (apiclient ^. #id)
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser (apiclient ^. #id)
  Just (tok, sec)           <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time

  Just (_, ver) <- dbUpdate $ VerifyCredentials tok (user ^. #id) time
  Just (t, _  ) <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = ver
                       }
    )
    time

  void $ dbUpdate $ DeletePrivileges (user ^. #id) (atID t)
  ps <- dbQuery $ GetGrantedPrivileges (user ^. #id)
  assertBool "DeletePrivileges: should have 0 privileges granted." $ ps == []

testDeletePrivilege :: TestEnv ()
testDeletePrivilege = do
  apiclient <- addNewRandomUser
  user      <- addNewRandomUser
  time      <- rand 10 arbitrary
  void $ dbUpdate $ CreateAPIToken (apiclient ^. #id)
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser (apiclient ^. #id)
  Just (tok, sec)           <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCreate]
                          }
    )
    time

  Just (_, ver) <- dbUpdate $ VerifyCredentials tok (user ^. #id) time
  Just (t, _  ) <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = ver
                       }
    )
    time

  void $ dbUpdate $ DeletePrivilege (user ^. #id) (atID t) APIDocCreate
  ps <- dbQuery $ GetGrantedPrivileges (user ^. #id)
  assertBool "DeletePrivileges: should have 0 privileges granted." $ ps == []

testPersonalToken :: TestEnv ()
testPersonalToken = do
  user <- addNewRandomUser
  mt   <- dbQuery $ GetPersonalToken (user ^. #id)
  assertBool "GetPersonalToken: should return Nothing with new User." $ mt == Nothing

  r <- dbUpdate $ CreatePersonalToken (user ^. #id)
  assertBool "Should have worked!" $ r

  mt' <- dbQuery $ GetPersonalToken (user ^. #id)
  assertBool "CreatePersonalToken: should return Just!" $ isJust mt'

  r' <- dbUpdate $ CreatePersonalToken (user ^. #id)
  assertBool "Should have failed!" $ not r'

  void $ dbUpdate $ DeletePersonalToken (user ^. #id)
  mt'' <- dbQuery $ GetPersonalToken (user ^. #id)
  assertBool "GetPersonalToken: should return Nothing with User who just deleted."
    $  mt''
    == Nothing

testRecentPersonalToken :: TestEnv ()
testRecentPersonalToken = do
  user <- addNewRandomUser
  mt   <- dbQuery $ GetRecentPersonalToken (user ^. #id) 5
  assertBool "GetPersonalToken: should return Nothing with new User." $ mt == Nothing

  r <- dbUpdate $ CreatePersonalToken (user ^. #id)
  assertBool "Should have worked!" $ r

  recent5min <- dbQuery $ GetRecentPersonalToken (user ^. #id) 5
  assertBool "GetRecentPersonalToken: should return Just! for 5-min recent token"
    $ isJust recent5min

  now <- currentTime
  setTestTime $ 6 `minutesAfter` now

  recent5min' <- dbQuery $ GetRecentPersonalToken (user ^. #id) 5
  assertBool "GetRecentPersonalToken: should return Nothing for 5-min recent token"
    $  recent5min'
    == Nothing

  void $ dbUpdate $ DeletePersonalToken (user ^. #id)
  mt'' <- dbQuery $ GetRecentPersonalToken (user ^. #id) 5
  assertBool "GetPersonalToken: should return Nothing with User who just deleted."
    $  mt''
    == Nothing
