module UserStateTest (userStateTests) where

import Data.Maybe
import Database.HDBC.PostgreSQL
import Test.HUnit (Assertion)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import qualified Data.ByteString.UTF8 as BS
import DB.Classes
import StateHelper
import User.Model
import TestingUtil

userStateTests :: Connection -> Test
userStateTests conn = testGroup "UserState" [
      testGroup "getUserByEmail" [
          testCase "returns nothing when there isn't a user with a matching email" $ test_getUserByEmail_returnsNothing conn
        , testCase "returns a user with a matching email" $ test_getUserByEmail_returnsTheRightUser conn
        ]
    , testGroup "getUserByUserID" [
          testCase "returns nothing when there isn't a user with a matching id" $ test_getUserByUserID_returnsNothing conn
        , testCase "returns a user with a matching id" $ test_getUserByUserID_returnsTheRightUser conn
        ]
{-    , testGroup "getUserStats" [
          testCase "returns the number of users" $ test_getUserStats_returnsTheUserCount conn
        ]-}
    , testGroup "getAllUsers" [
          testCase "returns all the users" $ test_getAllUsers_returnsAllUsers conn
        ]
    , testGroup "setUserPassword" [
          testCase "password is successfully set" $ test_setUserPassword_changesPassword conn
        ]
    , testGroup "addUser" [
          testCase "adding a repeated email returns nothing" $ test_addUser_repeatedEmailReturnsNothing conn
        ]
    ]

test_getUserByEmail_returnsNothing :: Connection -> Assertion
test_getUserByEmail_returnsNothing conn = withTestEnvironment conn $ do
    queriedUser <- dbQuery $ GetUserByEmail Nothing (Email (BS.fromString "emily@green.com"))
    assert (isNothing queriedUser)

test_getUserByEmail_returnsTheRightUser :: Connection -> Assertion
test_getUserByEmail_returnsTheRightUser conn = withTestEnvironment conn $ do
    Just user <- addNewUser "Emily" "Green" "emily@green.com"
    queriedUser <- dbQuery $ GetUserByEmail Nothing (Email $ BS.fromString "emily@green.com")
    assert (isJust queriedUser) 
    assertEqual "For GetUserByEmail result" user (fromJust queriedUser)

test_getUserByUserID_returnsNothing :: Connection -> Assertion
test_getUserByUserID_returnsNothing conn = withTestEnvironment conn $ do
    queriedUser <- dbQuery $ GetUserByID (UserID 100)
    assert (isNothing queriedUser)

test_getUserByUserID_returnsTheRightUser :: Connection -> Assertion
test_getUserByUserID_returnsTheRightUser conn = withTestEnvironment conn $ do
    Just user <- addNewUser "Emily" "Green" "emiy@green.com"
    queriedUser <- dbQuery $ GetUserByID $ userid user
    assert (isJust queriedUser)
    assertEqual "For GetUserByUserID result" user (fromJust queriedUser)

{-test_getUserStats_returnsTheUserCount :: Connection -> Assertion
test_getUserStats_returnsTheUserCount conn = withTestState $ do
    Just _ <- addNewUser conn "Emily" "Green" "emily@green.com"
    Just _ <- addNewUser conn "Bob" "Blue" "bob@blue.com"
    queriedStats <- ioRunDB conn $ dbQuery GetUserStats
    assertEqual "For GetUserStats" 2 (usercount queriedStats)-}

test_getAllUsers_returnsAllUsers :: Connection -> Assertion
test_getAllUsers_returnsAllUsers conn = withTestEnvironment conn $ do
    Just user0 <- addNewUser "Emily" "Green" "emily@green.com"
    Just user1 <- addNewUser "Bob" "Blue" "bob@blue.com"
    queriedUsers <- dbQuery GetUsers
    assertEqual "For GetUsers result" 2 (length queriedUsers)
    assert $ user0 `elem` queriedUsers
    assert $ user1 `elem` queriedUsers

test_setUserPassword_changesPassword :: Connection -> Assertion
test_setUserPassword_changesPassword conn = withTestEnvironment conn $ do
    Just user <- addNewUser "Emily" "Green" "emily@green.com"
    passwordhash <- createPassword (BS.fromString "Secret Password!")
    _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
    queriedUser <- dbQuery $ GetUserByEmail Nothing (Email (BS.fromString "emily@green.com"))
    assert $ verifyPassword (userpassword (fromJust queriedUser)) (BS.fromString "Secret Password!")

test_addUser_repeatedEmailReturnsNothing :: Connection -> Assertion
test_addUser_repeatedEmailReturnsNothing conn = withTestEnvironment conn $ do
    Just _ <- addNewUser "Emily" "Green" "emily@green.com"
    result <- addNewUser "Emily" "Green Again" "emily@green.com"
    assert (isNothing result)
