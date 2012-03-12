module UserHistoryTest (userHistoryTests) where

import Test.HUnit (Assertion)
import Test.Framework
import Test.Framework.Providers.HUnit
import StateHelper
import TestingUtil
import IPAddress
import Misc
import DB.Classes
import MinutesTime
import User.Model
import User.UserControl
import User.History.Model
import Context
import TestKontra as T
import Control.Applicative
import Happstack.Server
import Templates.TemplatesLoader
import Text.JSON
import ActionSchedulerState
import Login
import SignupTest (getAccountCreatedActions)

userHistoryTests :: DBEnv -> Test
userHistoryTests env = testGroup "User's history" [
      testCase "Test creating login attempt event"          $ testLoginAttempt env
    , testCase "Test creating login success event"          $ testLoginSuccess env
    , testCase "Test creating password setup event"         $ testPasswordSetup env
    , testCase "Test creating password setup request event" $ testPasswordSetupReq env
    , testCase "Test creating account created event"        $ testAccountCreated env
    , testCase "Test creating TOS accept event"             $ testTOSAccept env
    , testCase "Test creating details changed event"        $ testDetailsChanged env
    -- Handlers:
    , testCase "Test creating login attempt event by handler" 
               $ testHandlerForLoginAttempt env
    , testCase "Test creating login success event by handler" 
               $ testHandlerForLoginSuccess env
    , testCase "Test creating password setup event by handler" 
               $ testHandlerForPasswordSetup env
    , testCase "Test creating password setup request event by handler" 
               $ testHandlerForPasswordSetupReq env
    , testCase "Test creating account created event by handler" 
               $ testHandlerForAccountCreated env
    , testCase "Test creating TOS Accept event by handler" 
               $ testHandlerForTOSAccept env
    , testCase "Test creating details changed event by handler" 
               $ testHandlerForDetailsChanged env
    ]

testLoginAttempt :: DBEnv -> Assertion
testLoginAttempt env = withTestEnvironment env $ do
    User{userid} <- createTestUser
    now <- getMinutesTime
    success <- dbUpdate $ LogHistoryLoginAttempt userid unknownIPAddress now
    assertBool "LogHistoryLoginAttempt inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testLoginSuccess :: DBEnv -> Assertion
testLoginSuccess env = withTestEnvironment env $ do
    User{userid} <- createTestUser
    now <- getMinutesTime
    success <- dbUpdate $ LogHistoryLoginSuccess userid unknownIPAddress now
    assertBool "LogHistoryLoginSuccess inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testPasswordSetup :: DBEnv -> Assertion
testPasswordSetup env = withTestEnvironment env $ do
    User{userid} <- createTestUser
    now <- getMinutesTime
    success <- dbUpdate $ LogHistoryPasswordSetup userid unknownIPAddress now Nothing
    assertBool "LogHistoryPasswordSetup inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testPasswordSetupReq :: DBEnv -> Assertion
testPasswordSetupReq env = withTestEnvironment env $ do
    User{userid} <- createTestUser
    now <- getMinutesTime
    success <- dbUpdate $ LogHistoryPasswordSetupReq userid unknownIPAddress now Nothing
    assertBool "LogHistoryPasswordSetupReq inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testAccountCreated :: DBEnv -> Assertion
testAccountCreated env = withTestEnvironment env $ do
    User{userid} <- createTestUser
    now <- getMinutesTime
    success <- dbUpdate $ LogHistoryAccountCreated userid unknownIPAddress now
      (Email "test@test.com") Nothing
    assertBool "LogHistoryAccountCreated inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testTOSAccept :: DBEnv -> Assertion
testTOSAccept env = withTestEnvironment env $ do
    User{userid} <- createTestUser
    now <- getMinutesTime
    success <- dbUpdate $ LogHistoryTOSAccept userid unknownIPAddress now Nothing
    assertBool "LogHistoryTOSAccept inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testDetailsChanged :: DBEnv -> Assertion
testDetailsChanged env = withTestEnvironment env $ do
    User{userid} <- createTestUser
    now <- getMinutesTime
    success <- dbUpdate $ LogHistoryDetailsChanged userid unknownIPAddress now
      [("email", "test@test.com", "test2@test.com")] Nothing
    assertBool "LogHistoryTOSAccept inserted correctly" success
    history <- dbQuery $ GetUserHistoryByUserID userid
    assertBool "User's history is not empty" (not $ null history)

testHandlerForLoginAttempt :: DBEnv -> Assertion
testHandlerForLoginAttempt env = withTestEnvironment env $ do
    user <- createTestUser
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [ ("email", inText "karol@skrivapa.se")
                          , ("password", inText "test")
                          ]
    _ <- runTestKontra req ctx $ handleLoginPost
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains login attempt event" 
                $ compareEventTypeFromList UserLoginAttempt history

testHandlerForLoginSuccess :: DBEnv -> Assertion
testHandlerForLoginSuccess env = withTestEnvironment env $ do
    user <- createTestUser
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [ ("email", inText "karol@skrivapa.se")
                          , ("password", inText "test_password")
                          ]
    _ <- runTestKontra req ctx $ handleLoginPost
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains login success event" 
                $ compareEventTypeFromList UserLoginSuccess history

testHandlerForPasswordSetup :: DBEnv -> Assertion
testHandlerForPasswordSetup env = withTestEnvironment env $ do
    user <- createTestUser
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user})
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [ ("oldpassword", inText "test_password")
                          , ("password", inText "test1111test")
                          , ("password2", inText "test1111test")
                          ]
    _ <- runTestKontra req ctx $ handlePostUserSecurity
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains password setup event" 
                $ compareEventTypeFromList UserPasswordSetup $ history

testHandlerForPasswordSetupReq :: DBEnv -> Assertion
testHandlerForPasswordSetupReq env = withTestEnvironment env $ do
    user <- createTestUser
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user})
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [ ("oldpassword", inText "test")
                          , ("password", inText "test1111test")
                          , ("password2", inText "test1111test")
                          ]
    _ <- runTestKontra req ctx $ handlePostUserSecurity
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains password setup event" 
                $ compareEventTypeFromList UserPasswordSetupReq $ history

testHandlerForAccountCreated :: DBEnv -> Assertion
testHandlerForAccountCreated env = withTestEnvironment env $ do
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env})
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [ ("email", inText "test@test.com")]
    _ <- runTestKontra req ctx $ signupPagePost
    Just user <- dbQuery $ GetUserByEmail Nothing $ Email "test@test.com"
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains account created event" 
               $ compareEventTypeFromList UserAccountCreated $ history
    assertBool "History log contains user's email"
               $ compareEventDataFromList [("email", "", "test@test.com")] 
               $ history

testHandlerForTOSAccept :: DBEnv -> Assertion
testHandlerForTOSAccept env = withTestEnvironment env $ do
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env})
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req1 <- mkRequest POST [("email", inText "karol@skrivapa.se")]
    (_, ctx1) <- runTestKontra req1 ctx $ signupPagePost
    actions <- getAccountCreatedActions
    let aid = actionID (head actions)
        (AccountCreated uid token) = actionType (head actions)
    req2 <- mkRequest POST [ ("tos", inText "on")
                           , ("fstname", inText "Karol")
                           , ("sndname", inText "Samborski")
                           , ("password", inText "test1111test")
                           , ("password2", inText "test1111test")
                           ]
    _ <- runTestKontra req2 ctx1 $ handleAccountSetupPost aid token 
    history <- dbQuery $ GetUserHistoryByUserID uid
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains TOS accept event"
               $ compareEventTypeFromList UserTOSAccept $ history

testHandlerForDetailsChanged :: DBEnv -> Assertion
testHandlerForDetailsChanged env = withTestEnvironment env $ do
    user <- createTestUser
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user})
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [ ("fstname", inText "Karol")
                          , ("sndname", inText "Samborski")
                          , ("personalnumber", inText "123")
                          , ("phone", inText "2221122")
                          , ("companyposition", inText "Engineer")
                          ]
    _ <- runTestKontra req ctx $ handleUserPost
    history <- dbQuery $ GetUserHistoryByUserID $ userid user
    assertBool "History log exists" (not . null $ history)
    assertBool "History log contains details changed event" 
                $ compareEventTypeFromList UserDetailsChange $ history
    assertBool "History log contains valid data"
               $ compareEventDataFromList [
                      ("first_name", "", "Karol")
                    , ("last_name", "", "Samborski")
                    , ("personal_number", "", "123")
                    , ("company_position", "", "Engineer")
                    , ("phone", "", "2221122")
                    ] 
               $ history

compareEventTypeFromList :: UserHistoryEventType -> [UserHistory] -> Bool
compareEventTypeFromList t l = not . null . filter (\h -> (uheventtype . uhevent $ h) == t) $ l

compareEventDataFromList :: [(String, String, String)] -> [UserHistory] -> Bool
compareEventDataFromList d l = (uheventdata . uhevent . head $ l) == (Just $ JSArray $ 
    map (\(field, oldv, newv) -> JSObject . toJSObject $ [
          ("field", JSString $ toJSString field)
        , ("oldval", JSString $ toJSString oldv)
        , ("newval", JSString $ toJSString newv)
        ]) d)

createTestUser :: DB User
createTestUser = do
    pwd <- createPassword "test_password"
    muser <- dbUpdate $ AddUser ("", "")
                                "karol@skrivapa.se"
                                (Just pwd) 
                                False 
                                Nothing 
                                Nothing 
                                (mkLocaleFromRegion defaultValue)
    case muser of
        Nothing     -> error "Can't create user"
        (Just user) -> return user

