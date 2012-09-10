module UserStateTest (userStateTests) where

import Control.Monad
import Data.Maybe
import Test.Framework

import Company.Model
import DB
import MinutesTime
import Utils.Default
import User.Model
import TestingUtil
import TestKontra
import Data.List

sortByEmail :: [User] -> [User]
sortByEmail = sortBy (\a b -> compare (f a) (f b))
  where f = useremail . userinfo

userStateTests :: TestEnvSt -> Test
userStateTests env = testGroup "UserState" [
    testGroup "getUserByEmail" [
      testThat "returns nothing when there isn't a user with a matching email" env test_getUserByEmail_returnsNothing
    , testThat "returns a user with a matching email" env test_getUserByEmail_returnsTheRightUser
    ]
  , testGroup "getUserByID" [
      testThat "returns nothing when there isn't a user with a matching id" env test_getUserByID_returnsNothing
    , testThat "returns a user with a matching id" env test_getUserByID_returnsTheRightUser
    ]
  , testGroup "getAllUsers" [
      testThat "returns all the users" env test_getAllUsers_returnsAllUsers
    ]
  , testGroup "setUserEmail" [
      testThat "email casing works with set user email" env test_setUserEmail_GetByEmail
    , testThat "email gets set!" env test_setUserEmail_works
    ]
  , testGroup "setUserPassword" [
      testThat "password is successfully set" env test_setUserPassword_changesPassword
    ]
  , testGroup "addUser" [
      testThat "adding a repeated email returns nothing" env test_addUser_repeatedEmailReturnsNothing
    ]
  , testThat "SetUserCompanyAdmin/GetCompanyAccounts works" env test_getCompanyAccounts
  , testThat "GetInviteInfo/SetInviteInfo works" env test_getInviteInfo
  , testThat "SetUserCompany works" env test_setUserCompany
  , testThat "DeleteUser works" env test_deleteUser
  , testGroup "SetUserInfo" [
      testThat "SetUserInfo works" env test_setUserInfo
    , testThat "SetUserInfo handles email correctly" env test_setUserInfoCapEmail
    ]
  , testThat "SetUserSettings works" env test_setUserSettings
  , testThat "AcceptTermsOfService works" env test_acceptTermsOfService
  , testThat "SetSignupMethod works" env test_setSignupMethod
  ]

test_getUserByEmail_returnsNothing :: TestEnv ()
test_getUserByEmail_returnsNothing = do
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isNothing queriedUser)

test_getUserByEmail_returnsTheRightUser :: TestEnv ()
test_getUserByEmail_returnsTheRightUser = do
  Just user <- addNewUser "Emily" "Green" "emily@green.com"
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isJust queriedUser)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "EMILY@green.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)
  Just user3 <- addNewUser "Eric" "Normand" "ERIc@Normand.Com"
  queriedUser3 <- dbQuery $ GetUserByEmail (Email "eric@normand.com")
  assert (isJust queriedUser3)
  assertEqual "For GetUserByEmail result" user3 (fromJust queriedUser3)
  queriedUser4 <- dbQuery $ GetUserByEmail (Email "erIc@normand.com")
  assert (isJust queriedUser4)
  assertEqual "For GetUserByEmail result" user3 (fromJust queriedUser4)

test_getUserByID_returnsNothing :: TestEnv ()
test_getUserByID_returnsNothing = do
  queriedUser <- dbQuery $ GetUserByID (unsafeUserID 100)
  assert (isNothing queriedUser)

test_getUserByID_returnsTheRightUser :: TestEnv ()
test_getUserByID_returnsTheRightUser = do
  Just user <- addNewUser "Emily" "Green" "emiy@green.com"
  queriedUser <- dbQuery $ GetUserByID $ userid user
  assert (isJust queriedUser)
  assertEqual "For GetUserByUserID result" user (fromJust queriedUser)

test_getAllUsers_returnsAllUsers :: TestEnv ()
test_getAllUsers_returnsAllUsers = do
  Just user0 <- addNewUser "Emily" "Green" "emily@green.com"
  Just user1 <- addNewUser "Bob" "Blue" "bob@blue.com"
  queriedUsers <- dbQuery GetUsers
  assertEqual "For GetUsers result" 2 (length queriedUsers)
  assert $ user0 `elem` queriedUsers
  assert $ user1 `elem` queriedUsers

test_setUserEmail_GetByEmail :: TestEnv ()
test_setUserEmail_GetByEmail = do
  Just user' <- addNewUser "Emily" "Green" "emily@green.com"
  _ <- dbUpdate $ SetUserEmail (userid user') $ Email "Emily@green.coM"
  Just user <- dbQuery $ GetUserByID $ userid user'
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isJust queriedUser)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "EMILY@green.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)

test_setUserEmail_works :: TestEnv ()
test_setUserEmail_works = do
  Just user' <- addNewUser "Emily" "Green" "emily@green.com"
  _ <- dbUpdate $ SetUserEmail (userid user') $ Email "other@email.com"
  Just user <- dbQuery $ GetUserByID $ userid user'
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isNothing queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "Other@EmAil.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)

test_setUserPassword_changesPassword :: TestEnv ()
test_setUserPassword_changesPassword = do
  Just user <- addNewUser "Emily" "Green" "emily@green.com"
  passwordhash <- createPassword "Secret Password!"
  _ <- dbUpdate $ SetUserPassword (userid user) passwordhash
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert $ verifyPassword (userpassword (fromJust queriedUser)) "Secret Password!"

test_addUser_repeatedEmailReturnsNothing :: TestEnv ()
test_addUser_repeatedEmailReturnsNothing = do
  Just _ <- addNewUser "Emily" "Green" "emily@green.com"
  result <- addNewUser "Emily" "Green Again" "emily@green.com"
  assert $ isNothing result

test_getCompanyAccounts :: TestEnv ()
test_getCompanyAccounts = do
  Company{companyid = cid} <- dbUpdate $ CreateCompany Nothing
  let emails = ["emily@green.com", "emily2@green.com", "andrzej@skrivapa.se"]
  users <- forM emails $ \email -> do
    Just user <- addNewCompanyUser "Emily" "Green" email cid
    return user
  company_accounts <- dbQuery $ GetCompanyAccounts cid
  assertBool "Company accounts returned in proper order (sorted by email)" $ sortByEmail users == company_accounts

test_getInviteInfo :: TestEnv ()
test_getInviteInfo = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  now <- getMinutesTime
  let ii = InviteInfo {
      userinviter = userid
    , invitetime = Just now
    , invitetype = Just Viral
  }
  res1 <- dbUpdate $ SetInviteInfo (Just userid) now Viral userid
  assertBool "InviteInfo created correctly" res1
  Just ii2 <- dbQuery $ GetInviteInfo userid
  assertBool "Correct InviteInfo returned" $ ii == ii2
  res2 <- dbUpdate $ SetInviteInfo (Just userid) now Admin userid
  assertBool "InviteInfo updated correctly" res2
  Just ii3 <- dbQuery $ GetInviteInfo userid
  assertBool "Correct updated InviteInfo returned" $ ii { invitetype = Just Admin } == ii3
  res3 <- dbUpdate $ SetInviteInfo Nothing undefined undefined userid
  assertBool "InviteInfo erased correctly" res3
  noii <- dbQuery $ GetInviteInfo userid
  assertBool "No InviteInfo returned" $ isNothing noii

test_setUserCompany :: TestEnv ()
test_setUserCompany = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Company{companyid} <- dbUpdate $ CreateCompany Nothing
  res <- dbUpdate $ SetUserCompany userid (Just $ companyid)
  assertBool "Company was correctly set" res
  Just user <- dbQuery $ GetUserByID userid
  assertBool "Returned user has proper companyid" $ usercompany user == Just companyid

test_deleteUser :: TestEnv ()
test_deleteUser = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  res <- dbUpdate $ DeleteUser userid
  assertBool "User was correctly removed" res
  nouser <- dbQuery $ GetUserByID userid
  assertBool "No user returned after removal" $ isNothing nouser

test_setUserInfo :: TestEnv ()
test_setUserInfo = do
  Just User{userid, userinfo} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let ui = userinfo {
      userpersonalnumber = "1234567"
    , usercompanyposition = "blabla"
    , userphone = "66346343"
    , usermobile = "989834343"
  }
  res <- dbUpdate $ SetUserInfo userid ui
  assertBool "UserInfo updated correctly" res
  Just User{userinfo = ui2} <- dbQuery $ GetUserByID userid
  assertBool "Updated UserInfo returned" $ ui == ui2

test_setUserInfoCapEmail :: TestEnv ()
test_setUserInfoCapEmail = do
  Just User{userid, userinfo} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let ui = userinfo {
      userpersonalnumber = "1234567"
    , usercompanyposition = "blabla"
    , userphone = "66346343"
    , usermobile = "989834343"
    , useremail = Email "DFSFS@fsdfs.com"
  }
  res <- dbUpdate $ SetUserInfo userid ui
  assertBool "UserInfo updated correctly" res
  Just User{userinfo = ui2} <- dbQuery $ GetUserByID userid
  assertBool "Updated UserInfo returned" $ ui {useremail = Email "dfsfs@fsdfs.com"} == ui2

test_setUserSettings :: TestEnv ()
test_setUserSettings = do
  Just User{userid, usersettings} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let us = usersettings { locale = mkLocaleFromRegion defaultValue }
  res <- dbUpdate $ SetUserSettings userid us
  assertBool "UserSettings updated correctly" res
  Just User{usersettings = us2} <- dbQuery $ GetUserByID userid
  assertBool "Updated UserSettings returned" $ us == us2

test_acceptTermsOfService :: TestEnv ()
test_acceptTermsOfService = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  now <- getMinutesTime
  res <- dbUpdate $ AcceptTermsOfService userid now
  assertBool "User updated correctly" res
  Just User{userhasacceptedtermsofservice = accepted} <- dbQuery $ GetUserByID userid
  assertBool "Time of acceptance is correct" $ accepted == Just now

test_setSignupMethod :: TestEnv ()
test_setSignupMethod = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let method = ViralInvitation
  res <- dbUpdate $ SetSignupMethod userid method
  assertBool "User updated correctly" res
  Just User{usersignupmethod = newmethod} <- dbQuery $ GetUserByID userid
  assertBool "User's updated signup method is correct" $ newmethod == method
