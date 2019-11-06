module AccountInfoTest (accountInfoTests) where

import Happstack.Server
import Network.URI
import Test.Framework
import Test.QuickCheck
import qualified Data.Text as T

import DB hiding (query, update)
import InternalResponse
import KontraLink
import MagicHash (unsafeMagicHash)
import Mails.Model
import MinutesTime
import OAuth.Model
import TestingUtil
import TestKontra as T
import User.API
import User.Email
import User.EmailChangeRequest
import User.Lang ()
import User.Model
import User.UserControl
import Util.HasSomeUserInfo

accountInfoTests :: TestEnvSt -> Test
accountInfoTests env = testGroup
  "AccountInfo"
  [ testThat "lets users change their email addresses" env testChangeEmailAddress
  , testThat "need unique email to request email change"
             env
             testNeedEmailToBeUniqueToRequestChange
  , testThat "need the correct action id to complete email change"
             env
             testEmailChangeFailsIfActionIDIsWrong
  , testThat "need the correct hash to complete email change"
             env
             testEmailChangeFailsIfMagicHashIsWrong
  , testThat "need the correct user to complete email change"
             env
             testEmailChangeIfForAnotherUser
  , testThat "need the email to still be unique to complete email change"
             env
             testEmailChangeFailsIfEmailInUse
  , testThat "need the password to the correct to complete the email change"
             env
             testEmailChangeFailsIfPasswordWrong
  , testThat "need the password to be entered to complete the email change"
             env
             testEmailChangeFailsIfNoPassword
  , testThat "getprofile can be called with tokens aquired through OAuth"
             env
             testGetUserInfoWithOAuthTokens
  , testThat "the login api call redirects and sets cookie" env testLoginUsingAPI
  ]

testChangeEmailAddress :: TestEnv ()
testChangeEmailAddress = do
  Just user'   <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  void $ dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  ctx <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req1 <- mkRequest POST
                    [("changeemail", inText "true"), ("newemail", inText "jim@bob.com")]
  (res1, ctx1) <- runTestKontra req1 ctx $ apiCallChangeEmail
  assertEqual "Response code is 200" 200 (rsCode res1)
  Just uuser <- dbQuery $ GetUserByID (userid user)
  assertEqual "Email hasn't changed yet" "bob@blue.com" (getEmail uuser)
  -- move test time, so that email change requests expire
  modifyTestTime (30 `daysAfter`)
  actions <- dbQuery GetExpiredEmailChangeRequestsForTesting

  assertEqual "A request change email action was made" 1 (length $ actions)
  let EmailChangeRequest {..} = head actions
  assertEqual "Inviter id is correct"   (userid user)         ecrUserID
  assertEqual "Action email is correct" (Email "jim@bob.com") ecrNewEmail

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

  req2          <- mkRequest POST [("password", inText "abc123")]
  (res2, _ctx2) <- runTestKontra req2 ctx1 $ handlePostChangeEmail ecrUserID ecrToken
  assertBool "Response is redirect to account page" (isRedirect LinkAccount res2)
  assertBool "Response contains a flash message"    (hasFlashMessage res2)
  Just uuuser <- dbQuery $ GetUserByID (userid user)
  assertEqual "Email has changed" "jim@bob.com" (getEmail uuuser)


testNeedEmailToBeUniqueToRequestChange :: TestEnv ()
testNeedEmailToBeUniqueToRequestChange = do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  void $ addNewUser "Jim" "Bob" "jim@bob.com"
  ctx  <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req1 <- mkRequest
    POST
    [ ("changeemail"  , inText "true")
    , ("newemail"     , inText "jim@bob.com")
    , ("newemailagain", inText "jim@bob.com")
    ]
  (res1, _) <- runTestKontra req1 ctx $ apiCallChangeEmail
  assertEqual "Response code is 200" 200 (rsCode res1)
  -- move test time, so that email change requests expire
  modifyTestTime (30 `daysAfter`)
  actions <- dbQuery GetExpiredEmailChangeRequestsForTesting
  assertEqual "No request email action was made" 0 (length $ actions)

testEmailChangeFailsIfActionIDIsWrong :: TestEnv ()
testEmailChangeFailsIfActionIDIsWrong = do
  Just user'   <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  void $ dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user               <- dbQuery $ GetUserByID (userid user')
  ctx                     <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req                     <- mkRequest POST [("password", inText "abc123")]
  EmailChangeRequest {..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")
  (res, _ctx') <- runTestKontra req ctx $ handlePostChangeEmail (unsafeUserID 0) ecrToken
  assertBool "Response code is a redirect"  (isRedirect LinkAccount res)
  assertBool "Response has a flash message" (hasFlashMessage res)


testEmailChangeFailsIfMagicHashIsWrong :: TestEnv ()
testEmailChangeFailsIfMagicHashIsWrong = do
  Just user'   <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  void $ dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user               <- dbQuery $ GetUserByID (userid user')
  ctx                     <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req                     <- mkRequest POST [("password", inText "abc123")]
  EmailChangeRequest {..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")
  let wrongtoken = if ecrToken == unsafeMagicHash 123
        then unsafeMagicHash 12345
        else unsafeMagicHash 123
  (res, _ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID wrongtoken
  assertBool "Response code is a redirect"  (isRedirect LinkAccount res)
  assertBool "Response has a flash message" (hasFlashMessage res)

testEmailChangeIfForAnotherUser :: TestEnv ()
testEmailChangeIfForAnotherUser = do
  Just user'   <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  void $ dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user               <- dbQuery $ GetUserByID (userid user')
  Just anotheruser        <- addNewUser "Fred" "Frog" "fred@frog.com"
  ctx                     <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req                     <- mkRequest POST [("password", inText "abc123")]
  EmailChangeRequest {..} <- newEmailChangeRequest (userid anotheruser)
                                                   (Email "jim@bob.com")
  (res, _ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID ecrToken
  assertBool "Response code is a redirect"  (isRedirect LinkAccount res)
  assertBool "Response has a flash message" (hasFlashMessage res)

testEmailChangeFailsIfEmailInUse :: TestEnv ()
testEmailChangeFailsIfEmailInUse = do
  Just user'   <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  void $ dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user               <- dbQuery $ GetUserByID (userid user')
  ctx                     <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req                     <- mkRequest POST [("password", inText "abc123")]
  EmailChangeRequest {..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")

  Just _                  <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, _ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID ecrToken
  assertBool "Response code is a redirect"  (isRedirect LinkAccount res)
  assertBool "Response has a flash message" (hasFlashMessage res)

testEmailChangeFailsIfPasswordWrong :: TestEnv ()
testEmailChangeFailsIfPasswordWrong = do
  Just user'   <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  void $ dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user               <- dbQuery $ GetUserByID (userid user')
  ctx                     <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req                     <- mkRequest POST [("password", inText "wrongpassword")]
  EmailChangeRequest {..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")

  Just _                  <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, _ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID ecrToken
  assertBool "Response code is a redirect"  (isRedirect LinkAccount res)
  assertBool "Response has a flash message" (hasFlashMessage res)

testEmailChangeFailsIfNoPassword :: TestEnv ()
testEmailChangeFailsIfNoPassword = do
  Just user'   <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  void $ dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user               <- dbQuery $ GetUserByID (userid user')
  ctx                     <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req                     <- mkRequest POST [("password", inText "")]
  EmailChangeRequest {..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")

  Just _                  <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, _ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID ecrToken
  assertBool "Response code is a redirect" (isRedirect LinkAccount res)

testGetUserInfoWithOAuthTokens :: TestEnv ()
testGetUserInfoWithOAuthTokens = do
  user <- addNewRandomUser
  ctx  <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  -- Create OAuth API tokens
  let uid = userid user
  void $ dbUpdate $ CreateAPIToken uid
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser uid
  time            <- rand 10 arbitrary
  Just (tok, sec) <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest { tcCallback   = fromJust $ parseURI "http://www.google.com/"
                          , tcAPIToken   = apitoken
                          , tcAPISecret  = apisecret
                          , tcPrivileges = [APIDocCheck]
                          }
    )
    time
  Just (_, ver) <- dbUpdate $ VerifyCredentials tok uid time
  Just (t, s  ) <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest { trAPIToken   = apitoken
                       , trAPISecret  = apisecret
                       , trTempToken  = tok
                       , trTempSecret = sec
                       , trVerifier   = ver
                       }
    )
    time
  let authStr =
        "oauth_signature_method=\"PLAINTEXT\""
          ++ ",oauth_consumer_key=\""
          ++ show apitoken
          ++ "\""
          ++ ",oauth_token=\""
          ++ show t
          ++ "\""
          ++ ",oauth_signature=\""
          ++ show apisecret
          ++ "&"
          ++ show s
          ++ "\""

  reqUserInfo      <- mkRequestWithHeaders GET [] [("authorization", [T.pack authStr])]
  (resUserInfo, _) <- runTestKontra reqUserInfo ctx $ apiCallGetUserProfile
  assertEqual "We should get a 200 response" 200 (rsCode resUserInfo)

testLoginUsingAPI :: TestEnv ()
testLoginUsingAPI = do
  user <- addNewRandomUser
  ctx  <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  let uid = userid user
  void $ dbUpdate $ DeletePersonalToken uid
  void $ dbUpdate $ CreatePersonalToken uid
  Just (OAuthAuthorization {..}) <- dbQuery $ GetPersonalToken uid

  let authStr =
        "oauth_signature_method=\"PLAINTEXT\""
          ++ ",oauth_consumer_key=\""
          ++ show oaAPIToken
          ++ "\""
          ++ ",oauth_token=\""
          ++ show oaAccessToken
          ++ "\""
          ++ ",oauth_signature=\""
          ++ show oaAPISecret
          ++ "&"
          ++ show oaAccessSecret
          ++ "\""

  reqLogin <- mkRequestWithHeaders POST
                                   [("redirect", inText "/newdocument")]
                                   [("authorization", [T.pack authStr])]
  (resLogin, _) <- runTestKontra reqLogin ctx $ apiCallLoginUser
  assertEqual "We should get a 303 response" 303 (rsCode resLogin)
