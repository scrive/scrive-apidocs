module UserHistoryTest (userHistoryTests) where

import Happstack.Server
import Test.Framework
import Text.JSON
import Text.JSON.Gen

import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Context
import DB
import IPAddress
import Login
import MinutesTime
import SignupTest (getAccountCreatedActions)
import TestingUtil
import TestKontra as T
import User.API
import User.Email
import User.History.Model
import User.Model
import User.UserAccountRequest
import User.UserControl
import UserGroup.Data
import UserGroup.Model

userHistoryTests :: TestEnvSt -> Test
userHistoryTests env = testGroup "User's history" [
      testThat "Test creating login attempt event"          env testLoginAttempt
    , testThat "Test creating login success event"          env testLoginSuccess
    , testThat "Test creating password setup event"         env testPasswordSetup
    , testThat "Test creating password setup request event" env testPasswordSetupReq
    , testThat "Test creating account created event"        env testAccountCreated
    , testThat "Test creating TOS accept event"             env testTOSAccept
    , testThat "Test creating details changed event"        env testDetailsChanged
    -- Handlers:
    , testThat "Test creating login attempt event by handler"
               env testHandlerForLoginAttempt
    , testThat "Test creating login success event by handler"
               env testHandlerForLoginSuccess
    , testThat "Test creating password setup event by handler"
               env testHandlerForPasswordSetup
    , testThat "Test creating password setup request event by handler"
               env testHandlerForPasswordSetupReq
    , testThat "Test creating account created event by handler"
               env testHandlerForAccountCreated
    , testThat "Test creating TOS Accept event by handler"
               env testHandlerForTOSAccept
    , testThat "Test creating details changed event by handler"
               env testHandlerForDetailsChanged
    ]

testLoginAttempt :: TestEnv ()
testLoginAttempt = do
    User{userid} <- createTestUser
    now <- currentTime
    success <- dbUpdate $ LogHistoryLoginFailure userid noIP now
    assertBool "LogHistoryLoginFailure inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testLoginSuccess :: TestEnv ()
testLoginSuccess = do
    User{userid} <- createTestUser
    now <- currentTime
    success <- dbUpdate $ LogHistoryLoginSuccess userid noIP now
    assertBool "LogHistoryLoginSuccess inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testPasswordSetup :: TestEnv ()
testPasswordSetup = do
    User{userid} <- createTestUser
    now <- currentTime
    success <- dbUpdate $ LogHistoryPasswordSetup userid noIP now Nothing
    assertBool "LogHistoryPasswordSetup inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testPasswordSetupReq :: TestEnv ()
testPasswordSetupReq = do
    User{userid} <- createTestUser
    now <- currentTime
    success <- dbUpdate $ LogHistoryPasswordSetupReq userid noIP now Nothing
    assertBool "LogHistoryPasswordSetupReq inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testAccountCreated :: TestEnv ()
testAccountCreated = do
    User{userid} <- createTestUser
    now <- currentTime
    success <- dbUpdate $ LogHistoryAccountCreated userid noIP now
      (Email "test@test.com") Nothing
    assertBool "LogHistoryAccountCreated inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testTOSAccept :: TestEnv ()
testTOSAccept = do
    User{userid} <- createTestUser
    now <- currentTime
    success <- dbUpdate $ LogHistoryTOSAccept userid noIP now Nothing
    assertBool "LogHistoryTOSAccept inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testDetailsChanged :: TestEnv ()
testDetailsChanged = do
    User{userid} <- createTestUser
    now <- currentTime
    success <- dbUpdate $ LogHistoryDetailsChanged userid noIP now
      [("email", "test@test.com", "test2@test.com")] Nothing
    assertBool "LogHistoryTOSAccept inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testHandlerForLoginAttempt :: TestEnv ()
testHandlerForLoginAttempt = do
    user <- createTestUser
    ctx <- mkContext def
    req <- mkRequest POST [ ("email", inText "karol@skrivapa.se")
                          , ("password", inText "test")
                          ]
    _ <- runTestKontra req ctx $ handleLoginPost
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains login attempt event"
                $ compareEventTypeFromList UserLoginFailure history

testHandlerForLoginSuccess :: TestEnv ()
testHandlerForLoginSuccess = do
    user <- createTestUser
    ctx <- mkContext def
    req <- mkRequest POST [ ("email", inText "karol@skrivapa.se")
                          , ("password", inText "test_password")
                          ]
    _ <- runTestKontra req ctx $ handleLoginPost
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains login success event"
                $ compareEventTypeFromList UserLoginSuccess history

testHandlerForPasswordSetup :: TestEnv ()
testHandlerForPasswordSetup = do
    user <- createTestUser
    ctx <- (set ctxmaybeuser (Just user))
      <$> mkContext def
    req <- mkRequest POST [ ("oldpassword", inText "test_password")
                          , ("password", inText "test1111test")
                          ]
    _ <- runTestKontra req ctx $ apiCallChangeUserPassword
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains password setup event"
                $ compareEventTypeFromList UserPasswordSetup $ history

testHandlerForPasswordSetupReq :: TestEnv ()
testHandlerForPasswordSetupReq = do
    user <- createTestUser
    ctx <- (set ctxmaybeuser (Just user))
      <$> mkContext def
    req <- mkRequest POST [ ("oldpassword", inText "test")
                          , ("password", inText "test1111test")
                          ]
    _ <- runTestKontra req ctx $ apiCallChangeUserPassword
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains password setup event"
                $ compareEventTypeFromList UserPasswordSetupReq $ history

testHandlerForAccountCreated :: TestEnv ()
testHandlerForAccountCreated = do
    ctx <- mkContext def
    req <- mkRequest POST [ ("email", inText "test@test.com")]
    _ <- runTestKontra req ctx $ apiCallSignup
    Just user <- dbQuery $ GetUserByEmail $ Email "test@test.com"
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains account created event"
               $ compareEventTypeFromList UserAccountCreated $ history
    assertBool "History log contains user's email"
               $ compareEventDataFromList [("email", "", "test@test.com")]
               $ history

testHandlerForTOSAccept :: TestEnv ()
testHandlerForTOSAccept = do
    ctx <- mkContext def
    req1 <- mkRequest POST [("email", inText "karol@skrivapa.se")]
    (_, ctx1) <- runTestKontra req1 ctx $ apiCallSignup
    UserAccountRequest{..} <- head <$> getAccountCreatedActions
    req2 <- mkRequest POST [ ("tos", inText "on")
                           , ("fstname", inText "Karol")
                           , ("sndname", inText "Samborski")
                           , ("password", inText "test1111test")
                           , ("password2", inText "test1111test")
                           ]
    _ <- runTestKontra req2 ctx1 $ handleAccountSetupPost uarUserID uarToken AccountRequest
    history <- dbQuery $ GetUserHistoryByUserID uarUserID
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains TOS accept event"
               $ compareEventTypeFromList UserTOSAccept $ history

testHandlerForDetailsChanged :: TestEnv ()
testHandlerForDetailsChanged = do
    user <- createTestUser
    ctx <- (set ctxmaybeuser (Just user))
      <$> mkContext def
    req <- mkRequest POST [ ("fstname", inText "Karol")
                          , ("sndname", inText "Samborski")
                          , ("personalnumber", inText "1234567890")
                          , ("phone", inText "+222112233")
                          , ("companyposition", inText "Engineer")
                          ]
    _ <- runTestKontra req ctx $ apiCallUpdateUserProfile
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains details changed event"
                $ compareEventTypeFromList UserDetailsChange $ history
    assertBool "History log contains valid data"
               $ compareEventDataFromList [
                      ("first_name", "", "Karol")
                    , ("last_name", "", "Samborski")
                    , ("personal_number", "", "1234567890")
                    , ("company_position", "", "Engineer")
                    , ("phone", "", "+222112233")
                    ]
               $ history

compareEventTypeFromList :: UserHistoryEventType -> [UserHistory] -> Bool
compareEventTypeFromList t l = not . null . filter (\h -> (uheventtype . uhevent $ h) == t) $ l

compareEventDataFromList :: [(String, String, String)] -> [UserHistory] -> Bool
compareEventDataFromList d l = (uheventdata . uhevent . head $ l) == (Just $ JSArray $
       for d $ \(field, oldv, newv) -> runJSONGen $ do
          value "field" field
          value "oldval" oldv
          value "newval" newv
    )

createTestUser :: TestEnv User
createTestUser = do
    bd <- dbQuery $ GetMainBrandedDomain
    pwd <- createPassword "test_password"
    ug <- dbUpdate $ UserGroupCreate def
    muser <- dbUpdate $ AddUser ("", "")
                                "karol@skrivapa.se"
                                (Just pwd)
                                (get ugID ug, True)
                                def
                                (get bdid bd)
                                AccountRequest
    case muser of
        Nothing     -> unexpectedError "can't create user"
        (Just user) -> return user
