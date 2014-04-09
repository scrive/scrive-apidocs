module AccountInfoTest (accountInfoTests) where

import Control.Applicative
import Happstack.Server
import Test.Framework

import ActionQueue.Core
import ActionQueue.EmailChangeRequest
import Context
import DB hiding (query, update)
import FlashMessage
import MagicHash (unsafeMagicHash)
import Mails.Model
import MinutesTime
import Utils.Default
import Redirect
import TestingUtil
import TestKontra as T
import User.Lang()
import User.Model
import User.Email
import User.UserControl
import Util.HasSomeUserInfo
import User.API

accountInfoTests :: TestEnvSt -> Test
accountInfoTests env = testGroup "AccountInfo" [
    testThat "lets users change their email addresses" env testChangeEmailAddress
  , testThat "need unique email to request email change" env testNeedEmailToBeUniqueToRequestChange
  , testThat "need the correct action id to complete email change" env testEmailChangeFailsIfActionIDIsWrong
  , testThat "need the correct hash to complete email change" env testEmailChangeFailsIfMagicHashIsWrong
  , testThat "need the correct user to complete email change" env testEmailChangeIfForAnotherUser
  , testThat "need the email to still be unique to complete email change" env testEmailChangeFailsIfEmailInUse
  , testThat "need the password to the correct to complete the email change" env testEmailChangeFailsIfPasswordWrong
  , testThat "need the password to be entered to complete the email change" env testEmailChangeFailsIfNoPassword
  ]

testChangeEmailAddress :: TestEnv ()
testChangeEmailAddress = do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req1 <- mkRequest POST [ ("changeemail", inText "true")
                        , ("newemail", inText "jim@bob.com")
                        ]
  (res1, ctx1) <- runTestKontra req1 ctx $ apiCallChangeEmail
  assertEqual "Response code is 200" 200 (rsCode res1)
  Just uuser <- dbQuery $ GetUserByID (userid user)
  assertEqual "Email hasn't changed yet" "bob@blue.com" (getEmail uuser)

  actions <- getRequestChangeEmailActions
  assertEqual "A request change email action was made" 1 (length $ actions)
  let EmailChangeRequest{..} = head actions
  assertEqual "Inviter id is correct" (userid user) ecrUserID
  assertEqual "Action email is correct" (Email "jim@bob.com") ecrNewEmail

  emails <- dbQuery GetEmails
  assertEqual "An email was sent" 1 (length emails)

  req2 <- mkRequest POST [("password", inText "abc123")]
  (res2, ctx2) <- runTestKontra req2 ctx1 $ handlePostChangeEmail ecrUserID ecrToken >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res2)
  assertEqual "Location is /account" (Just "/account") (T.getHeader "location" (rsHeaders res2))
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx2)
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx2) `isFlashOfType` OperationDone
  Just uuuser <- dbQuery $ GetUserByID (userid user)
  assertEqual "Email has changed" "jim@bob.com" (getEmail uuuser)


testNeedEmailToBeUniqueToRequestChange :: TestEnv ()
testNeedEmailToBeUniqueToRequestChange = do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req1 <- mkRequest POST [ ("changeemail", inText "true")
                        , ("newemail", inText "jim@bob.com")
                        , ("newemailagain", inText "jim@bob.com")
                        ]
  (res1, _) <- runTestKontra req1 ctx $ apiCallChangeEmail
  assertEqual "Response code is 200" 200 (rsCode res1)
  actions <- getRequestChangeEmailActions
  assertEqual "No request email action was made" 0 (length $ actions)

testEmailChangeFailsIfActionIDIsWrong :: TestEnv ()
testEmailChangeFailsIfActionIDIsWrong = do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [("password", inText "abc123")]
  EmailChangeRequest{..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (unsafeUserID 0) ecrToken >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfMagicHashIsWrong :: TestEnv ()
testEmailChangeFailsIfMagicHashIsWrong = do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [("password", inText "abc123")]
  EmailChangeRequest{..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")
  let wrongtoken = if ecrToken == unsafeMagicHash 123
                     then unsafeMagicHash 12345
                     else unsafeMagicHash 123
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID wrongtoken >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeIfForAnotherUser :: TestEnv ()
testEmailChangeIfForAnotherUser = do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  Just anotheruser <- addNewUser "Fred" "Frog" "fred@frog.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [("password", inText "abc123")]
  EmailChangeRequest{..} <- newEmailChangeRequest (userid anotheruser) (Email "jim@bob.com")
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID ecrToken >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfEmailInUse:: TestEnv ()
testEmailChangeFailsIfEmailInUse = do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [("password", inText "abc123")]
  EmailChangeRequest{..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")

  Just _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID ecrToken >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfPasswordWrong :: TestEnv ()
testEmailChangeFailsIfPasswordWrong = do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [("password", inText "wrongpassword")]
  EmailChangeRequest{..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")

  Just _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID ecrToken >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfNoPassword :: TestEnv ()
testEmailChangeFailsIfNoPassword = do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [("password", inText "")]
  EmailChangeRequest{..} <- newEmailChangeRequest (userid user) (Email "jim@bob.com")

  Just _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, _ctx') <- runTestKontra req ctx $ handlePostChangeEmail ecrUserID ecrToken >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)

getRequestChangeEmailActions :: TestEnv [EmailChangeRequest]
getRequestChangeEmailActions = do
  expirytime <- (30 `daysAfter`) <$> getMinutesTime
  dbQuery $ GetExpiredActions emailChangeRequest expirytime
