module UserStateTest (userStateTests) where

import Test.Framework

import Company.Model
import DB
import Doc.DocInfo
import MinutesTime
import TestingUtil
import TestKontra
import User.Email
import User.Model

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
  , testGroup "user usage statistics" [
      testThat "can fetch statistics by user id" env test_userUsageStatisticsByUser,
      testThat "can fetch statistics by company id" env test_userUsageStatisticsByCompany
    ]
  , testThat "SetUserCompanyAdmin/GetCompanyAccounts works" env test_getCompanyAccounts
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
  assert $ maybeVerifyPassword (userpassword (fromJust queriedUser)) "Secret Password!"

test_addUser_repeatedEmailReturnsNothing :: TestEnv ()
test_addUser_repeatedEmailReturnsNothing = do
  Just _ <- addNewUser "Emily" "Green" "emily@green.com"
  result <- addNewUser "Emily" "Green Again" "emily@green.com"
  assert $ isNothing result

test_getCompanyAccounts :: TestEnv ()
test_getCompanyAccounts = do
  Company{companyid = cid} <- dbUpdate $ CreateCompany
  let emails = ["emily@green.com", "emily2@green.com", "andrzej@skrivapa.se"]
  users <- forM emails $ \email -> do
    Just user <- addNewCompanyUser "Emily" "Green" email cid
    return user
  company_accounts <- dbQuery $ GetCompanyAccounts cid
  assertBool "Company accounts returned in proper order (sorted by email)" $
    sortByEmail users == company_accounts
  res <- dbUpdate . DeleteUser . userid . head . sortByEmail $ users
  assertBool "User was deleted" res
  do
    company_accounts1 <- dbQuery $ GetCompanyAccounts cid
    assertBool "Company accounts returned in proper order (sorted by email)" $
      tail (sortByEmail users) == company_accounts1
  do
    company_accounts_all <- dbQuery $ GetCompanyAccountsIncludeDeleted cid
    assertBool "Company accounts returned in proper order (sorted by email)" $
      sortByEmail users == company_accounts_all

test_userUsageStatisticsByUser :: TestEnv ()
test_userUsageStatisticsByUser = do
  let email = "emily@green.com"
  Just user <- addNewUser "Emily" "Green" email
  _ <- addRandomDocumentWithAuthorAndCondition user isClosed
  res <- dbQuery $ GetUsageStatsOld (Left $ userid user) PartitionByMonth $ iyears 2000
  assertEqual "Document present in stats" 1 (length res)
  let [UserUsageStats{..}] = res
  assertEqual "Email in statistics is correct" email uusUserEmail
  assertEqual "Name in statistics is correct" "Emily Green" uusUserName
  assertEqual "Statistics are correct" 1 $ dsDocumentsClosed uusDocumentStats

test_userUsageStatisticsByCompany :: TestEnv ()
test_userUsageStatisticsByCompany = do
  let email1 = "emily@green.com"
      email2 = "bob@gblue.com"
  Company{companyid = cid} <- dbUpdate $ CreateCompany
  Just user1 <- addNewCompanyUser "Emily" "Green" email1 cid
  Just user2 <- addNewCompanyUser "Bob" "Blue" email2 cid
  _ <- addRandomDocumentWithAuthorAndCondition user1 isClosed
  _ <- addRandomDocumentWithAuthorAndCondition user2 isClosed
  res <- dbQuery $ GetUsageStatsOld (Right cid) PartitionByDay $ iyears 2000
  assertEqual "Documents present in stats" 2 $ length res
  let Just uus1 = find ((email1 ==) . uusUserEmail) res
      Just uus2 = find ((email2 ==) . uusUserEmail) res
  assertEqual "Statistics for Emily are correct" 1 $ dsDocumentsClosed $ uusDocumentStats uus1
  assertEqual "Statistics for Bob are correct" 1 $ dsDocumentsClosed $ uusDocumentStats uus2

test_setUserCompany :: TestEnv ()
test_setUserCompany = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Company{companyid} <- dbUpdate $ CreateCompany
  res <- dbUpdate $ SetUserCompany userid companyid
  assertBool "Company was correctly set" res
  Just user <- dbQuery $ GetUserByID userid
  assertBool "Returned user has proper companyid" $ usercompany user == companyid

test_deleteUser :: TestEnv ()
test_deleteUser = do
  Just User{userid,usercompany} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  res <- dbUpdate $ DeleteUser userid
  assertBool "User was correctly removed" res
  nouser <- dbQuery $ GetUserByID userid
  assertBool "No user returned after removal" $ isNothing nouser
  company_accounts <- dbQuery $ GetCompanyAccounts usercompany
  assertBool "No users in company after removal" $ null company_accounts

test_setUserInfo :: TestEnv ()
test_setUserInfo = do
  Just User{userid, userinfo} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let ui = userinfo {
      userpersonalnumber = "1234567"
    , usercompanyposition = "blabla"
    , userphone = "66346343"
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
    , useremail = Email "DFSFS@fsdfs.com"
  }
  res <- dbUpdate $ SetUserInfo userid ui
  assertBool "UserInfo updated correctly" res
  Just User{userinfo = ui2} <- dbQuery $ GetUserByID userid
  assertBool "Updated UserInfo returned" $ ui {useremail = Email "dfsfs@fsdfs.com"} == ui2

test_setUserSettings :: TestEnv ()
test_setUserSettings = do
  Just User{userid, usersettings} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let us = usersettings { lang = def }
  res <- dbUpdate $ SetUserSettings userid us
  assertBool "UserSettings updated correctly" res
  Just User{usersettings = us2} <- dbQuery $ GetUserByID userid
  assertBool "Updated UserSettings returned" $ us == us2

test_acceptTermsOfService :: TestEnv ()
test_acceptTermsOfService = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  now <- currentTime
  res <- dbUpdate $ AcceptTermsOfService userid now
  assertBool "User updated correctly" res
  Just User{userhasacceptedtermsofservice = Just accepted} <- dbQuery $ GetUserByID userid
  assertBool "Time of acceptance is correct" $ compareTime accepted now

test_setSignupMethod :: TestEnv ()
test_setSignupMethod = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let method = ViralInvitation
  res <- dbUpdate $ SetSignupMethod userid method
  assertBool "User updated correctly" res
  Just User{usersignupmethod = newmethod} <- dbQuery $ GetUserByID userid
  assertBool "User's updated signup method is correct" $ newmethod == method
