module UserStateTest(
    userStateTests
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import StateHelper
import UserState

import Happstack.State (update, query)

import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Either
import System.IO.Error
import qualified Data.Set as Set

userStateTests :: [Test]
userStateTests = [testGroup "getUserByEmail" 
                     [testCase "returns nothing when there isn't a user with a matching email" test_getUserByEmail_returnsNothing
                     ,testCase "returns a user with a matching email" test_getUserByEmail_returnsTheRightUser
                     ]  
                 ,testGroup "getUserByUserID" 
                     [testCase "returns nothing when there isn't a user with a matching id" test_getUserByUserID_returnsNothing
                     ,testCase "returns a user with a matching id" test_getUserByUserID_returnsTheRightUser
                     ]
                  ,testGroup "getUserSubaccounts" 
                     [
                     testCase "returns a list of users with matching supervisor ids" test_getUserSubaccounts_returnsTheRightUsers
                     ]
                  ,testGroup "getUserStats" 
                     [
                     testCase "returns the number of users" test_getUserStats_returnsTheUserCount
                     ]
                  ,testGroup "getAllUsers" 
                     [
                     testCase "returns all the users" test_getAllUsers_returnsAllUsers
                     ]
                  ,testGroup "setUserPassword" 
                     [
                     testCase "password is successfully set" test_setUserPassword_changesPassword
                     ]  
                  ,testGroup "setUserDetails" 
                     [
                     testCase "details are successfully set" test_setUserDetails_changesDetails
                     ] 
--                  ,testGroup "getUserFlashMessages" 
--                     [
--                     testCase "returns all of a particular user's flash messages" test_getUserFlashMessages_returnsTheRightFlashMessages
--                     ] 
--                 ,testGroup "addUser" 
--                     [testCase "adding a repeated email causes error" test_addUser_repeatedEmailCausesError
--                     ]
                 ]

test_getUserByEmail_returnsNothing = withTestState $ do
    queriedUser <- query $ GetUserByEmail (Email (BS.fromString "emily@green.com"))
    assert (isNothing queriedUser)

test_getUserByEmail_returnsTheRightUser = withTestState $ do
    user <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") NoPassword Nothing
    queriedUser <- query $ GetUserByEmail (Email (BS.fromString "emily@green.com"))
    assert (isJust queriedUser) 
    assertEqual "For GetUserByEmail result" user (fromJust queriedUser)

test_getUserByUserID_returnsNothing = withTestState $ do
    queriedUser <- query $ GetUserByUserID (UserID 100)
    assert (isNothing queriedUser)

test_getUserByUserID_returnsTheRightUser = withTestState $ do
    user <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") NoPassword Nothing
    queriedUser <- query $ GetUserByUserID (userid user)
    assert (isJust queriedUser)
    assertEqual "For GetUserByUserID result" user (fromJust queriedUser)

test_getUserSubaccounts_returnsTheRightUsers = withTestState $ do
    user0 <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") NoPassword (Just (UserID 100))
    user1 <- update $ AddUser (BS.fromString "Bob Blue") (BS.fromString "bob@blue.com") NoPassword (Just (UserID 100))
    queriedSubAccounts <- query $ GetUserSubaccounts (UserID 100)
    assertEqual "For GetUserSubaccounts result" 2 (Set.size queriedSubAccounts)
    assert $ user0 `Set.member` queriedSubAccounts
    assert $ user1 `Set.member` queriedSubAccounts

test_getUserStats_returnsTheUserCount = withTestState $ do
    user0 <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") NoPassword Nothing
    user1 <- update $ AddUser (BS.fromString "Bob Blue") (BS.fromString "bob@blue.com") NoPassword Nothing
    queriedStats <- query $ GetUserStats
    assertEqual "For GetUserStats" 2 queriedStats

test_getAllUsers_returnsAllUsers = withTestState $ do
    user0 <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") NoPassword (Just (UserID 100))
    user1 <- update $ AddUser (BS.fromString "Bob Blue") (BS.fromString "bob@blue.com") NoPassword (Just (UserID 100))
    queriedUsers <- query $ GetAllUsers
    assertEqual "For GetAllUsers result" 2 (length queriedUsers)
    assert $ user0 `elem` queriedUsers
    assert $ user1 `elem` queriedUsers

test_setUserPassword_changesPassword = withTestState $ do
    user <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") NoPassword Nothing
    passwordhash <- (createPassword (BS.fromString "Secret Password!"))
    update $ SetUserPassword user passwordhash
    queriedUser <- query $ GetUserByEmail (Email (BS.fromString "emily@green.com"))
    assert $ verifyPassword (userpassword (fromJust queriedUser)) (BS.fromString "Secret Password!")

test_setUserDetails_changesDetails = withTestState $ do
    user <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") NoPassword Nothing
    newUser <- update $ SetUserDetails user (BS.fromString "Emily May Green") (BS.fromString "Some Corp") (BS.fromString "12345") (BS.fromString "15 High Street, Town")
    assertEqual "For SetUserDetails result" (BS.fromString "Emily May Green") (userfullname newUser)
    assertEqual "For SetUserDetails result" (BS.fromString "Some Corp") (usercompanyname newUser)
    assertEqual "For SetUserDetails result" (BS.fromString "12345") (usercompanynumber newUser)
    assertEqual "For SetUserDetails result" (BS.fromString "15 High Street, Town") (userinvoiceaddress newUser)

-- This one doesn't compile
--
--test_getUserFlashMessages_returnsTheRightFlashMessages = withTestState $ do
--    user <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") BS.empty Nothing
--    update $ AddUserFlashMessage (userid user) firstFlashMessage
--    update $ AddUserFlashMessage (userid user) secondFlashMessage
--    queriedFlashMessages <- query $ GetUserFlashMessages (userid user)
--    assertEqual "For GetUserFlashMessages result" 2 (length queriedFlashMessages)
--    assert $ firstFlashMessage `elem` queriedFlashMessages
--    assert $ secondFlashMessage `elem` queriedFlashMessages
--        where firstFlashMessage = FlashMessage (BS.fromString "flash message 0")
--              secondFlashMessage = FlashMessage (BS.fromString "flash message 1")

-- This one errors
--
-- test_addUser_repeatedEmailCausesError = do
--     user <- update $ AddUser (BS.fromString "Emily Green") (BS.fromString "emily@green.com") BS.empty Nothing
--     result <- try $ update $ AddUser (BS.fromString "Emily Green Again") (BS.fromString "emily@green.com") BS.empty Nothing
--     assert (not (isRight result))

-- isRight (Right _) = True
-- isRight _ = False
