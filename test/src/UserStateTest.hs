module UserStateTest (userStateTests) where

import Control.Monad
import Data.Maybe
import Database.HDBC.PostgreSQL
import Test.Framework
import qualified Data.ByteString.UTF8 as BS

import Company.Model
import DB.Classes
import DB.Types
import MinutesTime
import User.Model
import TestingUtil

userStateTests :: Connection -> Test
userStateTests conn = testGroup "UserState" [
    testGroup "getUserByEmail" [
      testThat "returns nothing when there isn't a user with a matching email" conn test_getUserByEmail_returnsNothing
    , testThat "returns a user with a matching email" conn test_getUserByEmail_returnsTheRightUser
    ]
  , testGroup "getUserByID" [
      testThat "returns nothing when there isn't a user with a matching id" conn test_getUserByID_returnsNothing
    , testThat "returns a user with a matching id" conn test_getUserByID_returnsTheRightUser
    ]
  , testGroup "getAllUsers" [
      testThat "returns all the users" conn test_getAllUsers_returnsAllUsers
    ]
  , testGroup "setUserPassword" [
      testThat "password is successfully set" conn test_setUserPassword_changesPassword
    ]
  , testGroup "addUser" [
      testThat "adding a repeated email returns nothing" conn test_addUser_repeatedEmailReturnsNothing
    ]
  , testThat "AddViewerByEmail/GetUserFriends works" conn test_addViewerByEmail
  , testThat "GetUsersByFriendUserID works" conn test_getUsersByFriendUserID
  , testThat "MakeUserCompanyAdmin/GetCompanyAccounts works" conn test_getCompanyAccounts
  , testThat "GetInviteInfo/SetInviteInfo works" conn test_getInviteInfo
  , testThat "GetUserMailAPI/SetUserMailAPI works" conn test_getUserMailAPI
  , testThat "ExportUsersDetailsToCSV works" conn test_exportUsersDetailsToCSV
  , testThat "SetUserCompany works" conn test_setUserCompany
  , testThat "DeleteUser works" conn test_deleteUser
  , testThat "SetUserInfo works" conn test_setUserInfo
  , testThat "SetUserSettings works" conn test_setUserSettings
  , testThat "SetPreferredDesignMode works" conn test_setPreferredDesignMode
  , testThat "AcceptTermsOfService works" conn test_acceptTermsOfService
  , testThat "SetSignupMethod works" conn test_setSignupMethod
  ]

test_getUserByEmail_returnsNothing :: DB ()
test_getUserByEmail_returnsNothing = do
  queriedUser <- dbQuery $ GetUserByEmail Nothing (Email (BS.fromString "emily@green.com"))
  assert (isNothing queriedUser)

test_getUserByEmail_returnsTheRightUser :: DB ()
test_getUserByEmail_returnsTheRightUser = do
  Just user <- addNewUser "Emily" "Green" "emily@green.com"
  queriedUser <- dbQuery $ GetUserByEmail Nothing (Email $ BS.fromString "emily@green.com")
  assert (isJust queriedUser)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser)

test_getUserByID_returnsNothing :: DB ()
test_getUserByID_returnsNothing = do
  queriedUser <- dbQuery $ GetUserByID (UserID 100)
  assert (isNothing queriedUser)

test_getUserByID_returnsTheRightUser :: DB ()
test_getUserByID_returnsTheRightUser = do
  Just user <- addNewUser "Emily" "Green" "emiy@green.com"
  queriedUser <- dbQuery $ GetUserByID $ userid user
  assert (isJust queriedUser)
  assertEqual "For GetUserByUserID result" user (fromJust queriedUser)

test_getAllUsers_returnsAllUsers :: DB ()
test_getAllUsers_returnsAllUsers = do
  Just user0 <- addNewUser "Emily" "Green" "emily@green.com"
  Just user1 <- addNewUser "Bob" "Blue" "bob@blue.com"
  queriedUsers <- dbQuery GetUsers
  assertEqual "For GetUsers result" 2 (length queriedUsers)
  assert $ user0 `elem` queriedUsers
  assert $ user1 `elem` queriedUsers

test_setUserPassword_changesPassword :: DB ()
test_setUserPassword_changesPassword = do
  Just user <- addNewUser "Emily" "Green" "emily@green.com"
  passwordhash <- createPassword (BS.fromString "Secret Password!")
  _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
  queriedUser <- dbQuery $ GetUserByEmail Nothing (Email (BS.fromString "emily@green.com"))
  assert $ verifyPassword (userpassword (fromJust queriedUser)) (BS.fromString "Secret Password!")

test_addUser_repeatedEmailReturnsNothing :: DB ()
test_addUser_repeatedEmailReturnsNothing = do
  Just _ <- addNewUser "Emily" "Green" "emily@green.com"
  result <- addNewUser "Emily" "Green Again" "emily@green.com"
  assert $ isNothing result

test_addViewerByEmail :: DB ()
test_addViewerByEmail = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let emails = ["emily1@green.com", "emily2@green.com"]
  friends <- forM emails $ \email -> do
    Just user <- addNewUser "Emily" "Green" email
    res <- dbUpdate $ AddViewerByEmail userid (Email $ BS.fromString email)
    assertBool "User was successfully added as a friend" res
    return user
  users <- dbQuery $ GetUserFriends userid
  assertBool "User's friends list contains correct users" $ users == friends

test_getUsersByFriendUserID :: DB ()
test_getUsersByFriendUserID = do
  let friend_email = "andrzej@skrivapa.se"
  Just User{userid = fid} <- addNewUser "Andrzej" "Rybczak" friend_email
  users <- forM ["emily1@green.com", "emily2@green.com"] $ \email -> do
    Just user@User{userid} <- addNewUser "Emily" "Green" email
    res <- dbUpdate $ AddViewerByEmail userid (Email $ BS.fromString friend_email)
    assertBool "User was successfully added as a friend" res
    return user
  have_as_friend <- dbQuery $ GetUsersByFriendUserID fid
  assertBool "User's 'have as friends' list contains correct users" $ users == have_as_friend
  return ()

test_getCompanyAccounts :: DB ()
test_getCompanyAccounts = do
  Company{companyid = cid} <- dbUpdate $ CreateCompany Nothing Nothing
  Just User{userid} <- addNewCompanyUser "Andrzej" "Rybczak" "andrzej@skrivapa.se" cid
  users <- forM ["emily1@green.com", "emily2@green.com"] $ \email -> do
    Just user <- addNewCompanyUser "Emily" "Green" email cid
    return user
  res <- dbUpdate $ MakeUserCompanyAdmin userid
  assertBool "User was made company admin" res
  company_accounts <- dbQuery $ GetCompanyAccounts userid
  assertBool "Company accounts returned correctly" $ users == company_accounts

test_getInviteInfo :: DB ()
test_getInviteInfo = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  now <- getMinutesTime
  let ii = InviteInfo {
      userinviter = userid
    , invitetime = now
    , invitetype = Viral
  }
  res1 <- dbUpdate $ SetInviteInfo (Just userid) now Viral userid
  assertBool "InviteInfo created correctly" res1
  Just ii2 <- dbQuery $ GetInviteInfo userid
  assertBool "Correct InviteInfo returned" $ ii == ii2
  res2 <- dbUpdate $ SetInviteInfo (Just userid) now Admin userid
  assertBool "InviteInfo updated correctly" res2
  Just ii3 <- dbQuery $ GetInviteInfo userid
  assertBool "Correct updated InviteInfo returned" $ ii { invitetype = Admin } == ii3
  res3 <- dbUpdate $ SetInviteInfo Nothing undefined undefined userid
  assertBool "InviteInfo erased correctly" res3
  noii <- dbQuery $ GetInviteInfo userid
  assertBool "No InviteInfo returned" $ isNothing noii

test_getUserMailAPI :: DB ()
test_getUserMailAPI = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let mapi = UserMailAPI {
      umapiKey = MagicHash 0
    , umapiDailyLimit = 1
    , umapiSentToday = 0
  }
  res <- dbUpdate $ SetUserMailAPI userid $ Just mapi
  assertBool "UserMailAPI created correctly" res
  Just mapi2 <- dbQuery $ GetUserMailAPI userid
  assertBool "Correct UserMailAPI returned" $ mapi == mapi2
  res2 <- dbUpdate $ SetUserMailAPI userid $ Just mapi { umapiSentToday = 1 }
  assertBool "UserMailAPI updated correctly" res2
  Just mapi3 <- dbQuery $ GetUserMailAPI userid
  assertBool "Correct updated UserMailAPI returned" $ mapi { umapiSentToday = 1 } == mapi3
  res3 <- dbUpdate $ SetUserMailAPI userid Nothing
  assertBool "UserMailAPI erased correctly" res3
  nomapi <- dbQuery $ GetUserMailAPI userid
  assertBool "No UserMailAPI returned" $ isNothing nomapi

test_exportUsersDetailsToCSV :: DB ()
test_exportUsersDetailsToCSV = do
  Just _ <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just _ <- addNewUser "Emily" "Green" "emily@green.com"
  csv <- dbQuery ExportUsersDetailsToCSV
  let users = BS.fromString "Andrzej Rybczak, andrzej@skrivapa.se\nEmily Green, emily@green.com\n"
  assertBool "ExportUsersDetailsToCSV returned correct result" $ users == csv

test_setUserCompany :: DB ()
test_setUserCompany = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Company{companyid} <- dbUpdate $ CreateCompany Nothing Nothing
  res <- dbUpdate $ SetUserCompany userid companyid
  assertBool "Company was correctly set" res
  Just user <- dbQuery $ GetUserByID userid
  assertBool "Returned user has proper companyid" $ usercompany user == Just companyid

test_deleteUser :: DB ()
test_deleteUser = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  res <- dbUpdate $ DeleteUser userid
  assertBool "User was correctly removed" res
  nouser <- dbQuery $ GetUserByID userid
  assertBool "No user returned after removal" $ isNothing nouser

test_setUserInfo :: DB ()
test_setUserInfo = do
  Just User{userid, userinfo} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let ui = userinfo {
      userpersonalnumber = BS.fromString "1234567"
    , usercompanyposition = BS.fromString "blabla"
    , userphone = BS.fromString "66346343"
    , usermobile = BS.fromString "989834343"
  }
  res <- dbUpdate $ SetUserInfo userid ui
  assertBool "UserInfo updated correctly" res
  Just User{userinfo = ui2} <- dbQuery $ GetUserByID userid
  assertBool "Updated UserInfo returned" $ ui == ui2

test_setUserSettings :: DB ()
test_setUserSettings = do
  Just User{userid, usersettings} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let us = usersettings { preferreddesignmode = Just AdvancedMode }
  res <- dbUpdate $ SetUserSettings userid us
  assertBool "UserSettings updated correctly" res
  Just User{usersettings = us2} <- dbQuery $ GetUserByID userid
  assertBool "Updated UserSettings returned" $ us == us2

test_setPreferredDesignMode :: DB ()
test_setPreferredDesignMode = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let mode = Just AdvancedMode
  res <- dbUpdate $ SetPreferredDesignMode userid mode
  assertBool "DesignMode updated correctly" res
  Just User{usersettings = UserSettings{preferreddesignmode}} <- dbQuery $ GetUserByID userid
  assertBool "Updated DesignMode returned" $ mode == preferreddesignmode

test_acceptTermsOfService :: DB ()
test_acceptTermsOfService = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  now <- getMinutesTime
  res <- dbUpdate $ AcceptTermsOfService userid now
  assertBool "User updated correctly" res
  Just User{userhasacceptedtermsofservice = accepted} <- dbQuery $ GetUserByID userid
  assertBool "Time of acceptance is correct" $ accepted == Just now

test_setSignupMethod :: DB ()
test_setSignupMethod = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let method = ViralInvitation
  res <- dbUpdate $ SetSignupMethod userid method
  assertBool "User updated correctly" res
  Just User{usersignupmethod = newmethod} <- dbQuery $ GetUserByID userid
  assertBool "User's updated signup method is correct" $ newmethod == method
