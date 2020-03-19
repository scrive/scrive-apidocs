module UserHistoryTest (userHistoryTests) where

import Happstack.Server
import Test.Framework
import Text.JSON
import Text.JSON.Gen

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

userHistoryTests :: TestEnvSt -> Test
userHistoryTests env = testGroup
  "User's history"
  [ testThat "Test creating login attempt event"            env testLoginAttempt
  , testThat "Test creating login success event"            env testLoginSuccess
  , testThat "Test creating password setup event"           env testPasswordSetup
  , testThat "Test creating password setup request event"   env testPasswordSetupReq
  , testThat "Test creating account created event"          env testAccountCreated
  , testThat "Test creating TOS accept event"               env testTOSAccept
  , testThat "Test creating details changed event"          env testDetailsChanged
    -- Handlers:
  , testThat "Test creating login attempt event by handler" env testHandlerForLoginAttempt
  , testThat "Test creating login success event by handler" env testHandlerForLoginSuccess
  , testThat "Test creating password setup event by handler"
             env
             testHandlerForPasswordSetup
  , testThat "Test creating password setup request event by handler"
             env
             testHandlerForPasswordSetupReq
  , testThat "Test creating account created event by handler"
             env
             testHandlerForAccountCreated
  , testThat "Test creating TOS Accept event by handler" env testHandlerForTOSAccept
  , testThat "Test creating details changed event by handler"
             env
             testHandlerForDetailsChanged
  ]

testLoginAttempt :: TestEnv ()
testLoginAttempt = do
  uid     <- view #id <$> createTestUser
  now     <- currentTime
  success <- dbUpdate $ LogHistoryLoginFailure uid noIP now
  assertBool "LogHistoryLoginFailure inserted correctly" success
  history <- dbQuery $ GetUserHistoryByUserID uid
  assertBool "User's history is not empty" (not $ null history)

testLoginSuccess :: TestEnv ()
testLoginSuccess = do
  uid     <- view #id <$> createTestUser
  now     <- currentTime
  success <- dbUpdate $ LogHistoryLoginSuccess uid noIP now
  assertBool "LogHistoryLoginSuccess inserted correctly" success
  history <- dbQuery $ GetUserHistoryByUserID uid
  assertBool "User's history is not empty" (not $ null history)

testPasswordSetup :: TestEnv ()
testPasswordSetup = do
  uid     <- view #id <$> createTestUser
  now     <- currentTime
  success <- dbUpdate $ LogHistoryPasswordSetup uid noIP now Nothing
  assertBool "LogHistoryPasswordSetup inserted correctly" success
  history <- dbQuery $ GetUserHistoryByUserID uid
  assertBool "User's history is not empty" (not $ null history)

testPasswordSetupReq :: TestEnv ()
testPasswordSetupReq = do
  uid     <- view #id <$> createTestUser
  now     <- currentTime
  success <- dbUpdate $ LogHistoryPasswordSetupReq uid noIP now Nothing
  assertBool "LogHistoryPasswordSetupReq inserted correctly" success
  history <- dbQuery $ GetUserHistoryByUserID uid
  assertBool "User's history is not empty" (not $ null history)

testAccountCreated :: TestEnv ()
testAccountCreated = do
  uid     <- view #id <$> createTestUser
  now     <- currentTime
  success <- dbUpdate
    $ LogHistoryAccountCreated uid noIP now (Email "test@test.com") Nothing
  assertBool "LogHistoryAccountCreated inserted correctly" success
  history <- dbQuery $ GetUserHistoryByUserID uid
  assertBool "User's history is not empty" (not $ null history)

testTOSAccept :: TestEnv ()
testTOSAccept = do
  uid     <- view #id <$> createTestUser
  now     <- currentTime
  success <- dbUpdate $ LogHistoryTOSAccept uid noIP now Nothing
  assertBool "LogHistoryTOSAccept inserted correctly" success
  history <- dbQuery $ GetUserHistoryByUserID uid
  assertBool "User's history is not empty" (not $ null history)

testDetailsChanged :: TestEnv ()
testDetailsChanged = do
  uid     <- view #id <$> createTestUser
  now     <- currentTime
  success <- dbUpdate $ LogHistoryDetailsChanged
    uid
    noIP
    now
    [("email", "test@test.com", "test2@test.com")]
    Nothing
  assertBool "LogHistoryTOSAccept inserted correctly" success
  history <- dbQuery $ GetUserHistoryByUserID uid
  assertBool "User's history is not empty" (not $ null history)

testHandlerForLoginAttempt :: TestEnv ()
testHandlerForLoginAttempt = do
  user <- createTestUser
  ctx  <- mkContext defaultLang
  req  <- mkRequest POST
                    [("email", inText "karol@skrivapa.se"), ("password", inText "test")]
  void $ runTestKontra req ctx handleLoginPost
  history <- dbQuery . GetUserHistoryByUserID $ user ^. #id
  assertBool "History log exists" (not . null $ history)
  assertBool "History log contains login attempt event"
    $ compareEventTypeFromList UserLoginFailure history

testHandlerForLoginSuccess :: TestEnv ()
testHandlerForLoginSuccess = do
  user <- createTestUser
  ctx  <- mkContext defaultLang
  req  <- mkRequest
    POST
    [("email", inText "karol@skrivapa.se"), ("password", inText "test_password")]
  void $ runTestKontra req ctx handleLoginPost
  history <- dbQuery . GetUserHistoryByUserID $ user ^. #id
  assertBool "History log exists" (not . null $ history)
  assertBool "History log contains login success event"
    $ compareEventTypeFromList UserLoginSuccess history

testHandlerForPasswordSetup :: TestEnv ()
testHandlerForPasswordSetup = do
  user <- createTestUser
  ctx  <- set #maybeUser (Just user) <$> mkContext defaultLang
  req  <- mkRequest
    POST
    [("oldpassword", inText "test_password"), ("password", inText "test1111test")]
  void $ runTestKontra req ctx apiCallChangeUserPassword
  history <- dbQuery . GetUserHistoryByUserID $ user ^. #id
  assertBool "History log exists" (not . null $ history)
  assertBool "History log contains password setup event"
    $ compareEventTypeFromList UserPasswordSetup history

testHandlerForPasswordSetupReq :: TestEnv ()
testHandlerForPasswordSetupReq = do
  user <- createTestUser
  ctx  <- set #maybeUser (Just user) <$> mkContext defaultLang
  req  <- mkRequest POST
                    [("oldpassword", inText "test"), ("password", inText "test1111test")]
  void $ runTestKontra req ctx apiCallChangeUserPassword
  history <- dbQuery . GetUserHistoryByUserID $ user ^. #id
  assertBool "History log exists" (not . null $ history)
  assertBool "History log contains password setup event"
    $ compareEventTypeFromList UserPasswordSetupReq history

testHandlerForAccountCreated :: TestEnv ()
testHandlerForAccountCreated = do
  ctx <- mkContext defaultLang
  req <- mkRequest POST [("email", inText "test@test.com")]
  void $ runTestKontra req ctx apiCallSignup
  Just user <- dbQuery . GetUserByEmail $ Email "test@test.com"
  history   <- dbQuery . GetUserHistoryByUserID $ user ^. #id
  assertBool "History log exists" (not . null $ history)
  assertBool "History log contains account created event"
    $ compareEventTypeFromList UserAccountCreated history
  assertBool "History log contains user's email"
    $ compareEventDataFromList [("email", "", "test@test.com")] history

testHandlerForTOSAccept :: TestEnv ()
testHandlerForTOSAccept = do
  ctx                     <- mkContext defaultLang
  req1                    <- mkRequest POST [("email", inText "karol@skrivapa.se")]
  (_, ctx1)               <- runTestKontra req1 ctx apiCallSignup
  UserAccountRequest {..} <- head <$> getAccountCreatedActions
  req2                    <- mkRequest
    POST
    [ ("tos"      , inText "on")
    , ("fstname"  , inText "Karol")
    , ("sndname"  , inText "Samborski")
    , ("password" , inText "test1111test")
    , ("password2", inText "test1111test")
    ]
  void . runTestKontra req2 ctx1 $ handleAccountSetupPost uarUserID
                                                          uarToken
                                                          AccountRequest
  history <- dbQuery $ GetUserHistoryByUserID uarUserID
  assertBool "History log exists" (not . null $ history)
  assertBool "History log contains TOS accept event"
    $ compareEventTypeFromList UserTOSAccept history

testHandlerForDetailsChanged :: TestEnv ()
testHandlerForDetailsChanged = do
  user <- createTestUser
  ctx  <- set #maybeUser (Just user) <$> mkContext defaultLang
  req  <- mkRequest
    POST
    [ ("fstname"        , inText "Karol")
    , ("sndname"        , inText "Samborski")
    , ("personalnumber" , inText "1234567890")
    , ("phone"          , inText "+222112233")
    , ("companyposition", inText "Engineer")
    ]
  void $ runTestKontra req ctx apiCallUpdateUserProfile
  history <- dbQuery . GetUserHistoryByUserID $ user ^. #id
  assertBool "History log exists" (not . null $ history)
  assertBool "History log contains details changed event"
    $ compareEventTypeFromList UserDetailsChange history
  assertBool "History log contains valid data" $ compareEventDataFromList
    [ ("first_name"      , "", "Karol")
    , ("last_name"       , "", "Samborski")
    , ("personal_number" , "", "1234567890")
    , ("company_position", "", "Engineer")
    , ("phone"           , "", "+222112233")
    ]
    history

compareEventTypeFromList :: UserHistoryEventType -> [UserHistory] -> Bool
compareEventTypeFromList t = any (\h -> (uheventtype . uhevent $ h) == t)

compareEventDataFromList :: [(String, String, String)] -> [UserHistory] -> Bool
compareEventDataFromList d l = (uheventdata . uhevent . head $ l) == Just
  (JSArray . for d $ \(field, oldv, newv) -> runJSONGen $ do
    value "field"  field
    value "oldval" oldv
    value "newval" newv
  )

createTestUser :: TestEnv User
createTestUser = do
  instantiateUser $ randomUserTemplate { email     = return "karol@skrivapa.se"
                                       , firstName = return ""
                                       , lastName  = return ""
                                       , password  = Just "test_password"
                                       }
