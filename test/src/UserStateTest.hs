module UserStateTest (userStateTests) where

import Data.Maybe
import Happstack.State
import Test.HUnit hiding (Test)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import qualified Data.ByteString.UTF8 as BS

import StateHelper
import User.Password
import User.UserState

userStateTests :: Test
userStateTests = testGroup "UserState" [
      testGroup "getUserByEmail" [
          testCase "returns nothing when there isn't a user with a matching email" test_getUserByEmail_returnsNothing
        , testCase "returns a user with a matching email" test_getUserByEmail_returnsTheRightUser
        ]
    , testGroup "getUserByUserID" [
          testCase "returns nothing when there isn't a user with a matching id" test_getUserByUserID_returnsNothing
        , testCase "returns a user with a matching id" test_getUserByUserID_returnsTheRightUser
        ]
    , testGroup "getUserStats" [
          testCase "returns the number of users" test_getUserStats_returnsTheUserCount
        ]
    , testGroup "getAllUsers" [
          testCase "returns all the users" test_getAllUsers_returnsAllUsers
        ]
    , testGroup "setUserPassword" [
          testCase "password is successfully set" test_setUserPassword_changesPassword
        ]
    , testGroup "addUser" [
          testCase "adding a repeated email returns nothing" test_addUser_repeatedEmailReturnsNothing
        ]
    ]

test_getUserByEmail_returnsNothing :: Assertion
test_getUserByEmail_returnsNothing = withTestState $ do
    queriedUser <- query $ GetUserByEmail Nothing (Email (BS.fromString "emily@green.com"))
    assert (isNothing queriedUser)

test_getUserByEmail_returnsTheRightUser :: Assertion
test_getUserByEmail_returnsTheRightUser = withTestState $ do
    Just user <- addNewUser "Emily" "Green" "emily@green.com"
    queriedUser <- query $ GetUserByEmail Nothing (Email (BS.fromString "emily@green.com"))
    assert (isJust queriedUser) 
    assertEqual "For GetUserByEmail result" user (fromJust queriedUser)

test_getUserByUserID_returnsNothing :: Assertion
test_getUserByUserID_returnsNothing = withTestState $ do
    queriedUser <- query $ GetUserByUserID (UserID 100)
    assert (isNothing queriedUser)

test_getUserByUserID_returnsTheRightUser :: Assertion
test_getUserByUserID_returnsTheRightUser = withTestState $ do
    Just user <- addNewUser "Emily" "Green" "emiy@green.com"
    queriedUser <- query $ GetUserByUserID (userid user)
    assert (isJust queriedUser)
    assertEqual "For GetUserByUserID result" user (fromJust queriedUser)

test_getUserStats_returnsTheUserCount :: Assertion
test_getUserStats_returnsTheUserCount = withTestState $ do
    Just _ <- addNewUser "Emily" "Green" "emily@green.com"
    Just _ <- addNewUser "Bob" "Blue" "bob@blue.com"
    queriedStats <- query $ GetUserStats
    assertEqual "For GetUserStats" 2 (usercount queriedStats)

test_getAllUsers_returnsAllUsers :: Assertion
test_getAllUsers_returnsAllUsers = withTestState $ do
    Just user0 <- addNewUser "Emily" "Green" "emily@green.com"
    Just user1 <- addNewUser "Bob" "Blue" "bob@blue.com"
    queriedUsers <- query $ GetAllUsers
    assertEqual "For GetAllUsers result" 2 (length queriedUsers)
    assert $ user0 `elem` queriedUsers
    assert $ user1 `elem` queriedUsers

test_setUserPassword_changesPassword :: Assertion
test_setUserPassword_changesPassword = withTestState $ do
    Just user <- addNewUser "Emily" "Green" "emily@green.com"
    passwordhash <- (createPassword (BS.fromString "Secret Password!"))
    _ <- update $ SetUserPassword (userid user) passwordhash
    queriedUser <- query $ GetUserByEmail Nothing (Email (BS.fromString "emily@green.com"))
    assert $ verifyPassword (userpassword (fromJust queriedUser)) (BS.fromString "Secret Password!")

test_addUser_repeatedEmailReturnsNothing :: Assertion
test_addUser_repeatedEmailReturnsNothing = withTestState $ do
    Just _ <- addNewUser "Emily" "Green" "emily@green.com"
    result <- addNewUser "Emily" "Green Again" "emily@green.com"
    assert (isNothing result)

addNewUser :: String -> String -> String -> IO (Maybe User)
addNewUser firstname secondname email = do
    muser <- update $ AddUser (BS.fromString firstname, BS.fromString secondname)(BS.fromString email) NoPassword False Nothing Nothing
    return muser
