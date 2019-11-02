module UserStateTest (userStateTests) where

import Test.Framework
import qualified Data.Text as T

import Chargeable.Model
import DB
import Doc.DocumentID
import Doc.Types.Document
import Doc.Types.DocumentStatus
import MinutesTime
import TestingUtil
import TestKontra
import User.Email
import User.Model
import UserGroup.Model
import UserGroup.Types

sortByEmail :: [User] -> [User]
sortByEmail = sortBy (\a b -> compare (f a) (f b)) where f = useremail . userinfo

userStateTests :: TestEnvSt -> Test
userStateTests env = testGroup
  "UserState"
  [ testGroup
    "getUserByEmail"
    [ testThat "returns nothing when there isn't a user with a matching email"
               env
               test_getUserByEmail_returnsNothing
    , testThat "returns a user with a matching email"
               env
               test_getUserByEmail_returnsTheRightUser
    ]
  , testGroup
    "getUserByID"
    [ testThat "returns nothing when there isn't a user with a matching id"
               env
               test_getUserByID_returnsNothing
    , testThat "returns a user with a matching id"
               env
               test_getUserByID_returnsTheRightUser
    ]
  , testGroup
    "setUserEmail"
    [ testThat "email casing works with set user email" env test_setUserEmail_GetByEmail
    , testThat "email gets set!" env test_setUserEmail_works
    ]
  , testGroup
    "setUserPassword"
    [testThat "password is successfully set" env test_setUserPassword_changesPassword]
  , testGroup
    "addUser"
    [ testThat "adding a repeated email returns nothing"
               env
               test_addUser_repeatedEmailReturnsNothing
    ]
  , testGroup
    "user usage statistics"
    [ testThat "can fetch statistics by user id"    env test_userUsageStatisticsByUser
    , testThat "can fetch statistics by company id" env test_userUsageStatisticsByCompany
    ]
  , testGroup
    "user shareable link statistics"
    [ testThat "can fetch shareable link statistics by user id"
               env
               test_userShareableLinkStatisticsByUser
    , testThat "can fetch shareable link statistics by user id for correct user only"
               env
               test_userShareableLinkStatisticsByUserOnlyCorrectUser
    , testThat "can fetch shareable link statistics by group id"
               env
               test_userShareableLinkStatisticsByGroup
    ]
  , testThat "UserGroupGetUsers/UserGroupGetUsersIncludeDeleted works"
             env
             test_userGroupGetUsers
  , testThat "SetUserUserGroup works" env test_setUserCompany
  , testThat "DeleteUser works"       env test_deleteUser
  , testGroup
    "SetUserInfo"
    [ testThat "SetUserInfo works"                   env test_setUserInfo
    , testThat "SetUserInfo handles email correctly" env test_setUserInfoCapEmail
    ]
  , testThat "SetUserSettings works"      env test_setUserSettings
  , testThat "AcceptTermsOfService works" env test_acceptTermsOfService
  , testThat "SetSignupMethod works"      env test_setSignupMethod
  ]

test_getUserByEmail_returnsNothing :: TestEnv ()
test_getUserByEmail_returnsNothing = do
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isNothing queriedUser)

test_getUserByEmail_returnsTheRightUser :: TestEnv ()
test_getUserByEmail_returnsTheRightUser = do
  Just user   <- addNewUser "Emily" "Green" "emily@green.com"
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isJust queriedUser)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "EMILY@green.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)
  Just user3   <- addNewUser "Eric" "Normand" "ERIc@Normand.Com"
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
  Just user   <- addNewUser "Emily" "Green" "emiy@green.com"
  queriedUser <- dbQuery $ GetUserByID $ userid user
  assert (isJust queriedUser)
  assertEqual "For GetUserByUserID result" user (fromJust queriedUser)

test_setUserEmail_GetByEmail :: TestEnv ()
test_setUserEmail_GetByEmail = do
  Just user' <- addNewUser "Emily" "Green" "emily@green.com"
  void $ dbUpdate $ SetUserEmail (userid user') $ Email "Emily@green.coM"
  Just user   <- dbQuery $ GetUserByID $ userid user'
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isJust queriedUser)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "EMILY@green.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)

test_setUserEmail_works :: TestEnv ()
test_setUserEmail_works = do
  Just user' <- addNewUser "Emily" "Green" "emily@green.com"
  void $ dbUpdate $ SetUserEmail (userid user') $ Email "other@email.com"
  Just user   <- dbQuery $ GetUserByID $ userid user'
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isNothing queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "Other@EmAil.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)

test_setUserPassword_changesPassword :: TestEnv ()
test_setUserPassword_changesPassword = do
  Just user    <- addNewUser "Emily" "Green" "emily@green.com"
  passwordhash <- createPassword "Secret Password!"
  void $ dbUpdate $ SetUserPassword (userid user) passwordhash
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert $ maybeVerifyPassword (userpassword (fromJust queriedUser)) "Secret Password!"

test_addUser_repeatedEmailReturnsNothing :: TestEnv ()
test_addUser_repeatedEmailReturnsNothing = do
  Just _ <- addNewUser "Emily" "Green" "emily@green.com"
  result <- addNewUser "Emily" "Green Again" "emily@green.com"
  assert $ isNothing result

test_userGroupGetUsers :: TestEnv ()
test_userGroupGetUsers = do
  ugid <- view #ugID <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  let emails = ["emily@green.com", "emily2@green.com", "andrzej@skrivapa.se"]
  users <- forM emails $ \email -> do
    Just user <- addNewUserToUserGroup "Emily" "Green" email ugid
    return user
  ugAccounts1 <- dbQuery $ UserGroupGetUsers ugid
  assertBool "Company accounts returned in proper order (sorted by email)"
    $  sortByEmail users
    == ugAccounts1
  res <- dbUpdate . DeleteUser . userid . head . sortByEmail $ users
  assertBool "User was deleted" res
  do
    ugAccounts2 <- dbQuery $ UserGroupGetUsers ugid
    assertBool "Company accounts returned in proper order (sorted by email)"
      $  tail (sortByEmail users)
      == ugAccounts2
  do
    ugAccountsAll <- dbQuery $ UserGroupGetUsersIncludeDeleted ugid
    assertBool "Company accounts returned in proper order (sorted by email)"
      $  map userid (sortByEmail users)
      == map userid ugAccountsAll

test_userUsageStatisticsByUser :: TestEnv ()
test_userUsageStatisticsByUser = do
  let email = "emily@green.com"
  Just user <- addNewUser "Emily" "Green" email
  doc       <- addRandomDocument (rdaDefault user) { rdaTypes    = OneOf [Signable]
                                                   , rdaStatuses = OneOf [Closed]
                                                   }
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc)
  res <- dbQuery
    $ GetUsageStats (UsageStatsForUser $ userid user) PartitionByMonth (iyears 2000)
  assertEqual "Document present in stats" 1 (length res)
  let [UserUsageStats {..}] = res
  assertEqual "Email in statistics is correct" email         (T.pack uusUserEmail)
  assertEqual "Name in statistics is correct"  "Emily Green" uusUserName
  assertEqual "Statistics are correct" 1 $ dsDocumentsClosed uusDocumentStats

test_userUsageStatisticsByCompany :: TestEnv ()
test_userUsageStatisticsByCompany = do
  let email1 = "emily@green.com"
      email2 = "bob@gblue.com"
  ugid       <- view #ugID <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  Just user1 <- addNewUserToUserGroup "Emily" "Green" email1 ugid
  Just user2 <- addNewUserToUserGroup "Bob" "Blue" email2 ugid
  doc0       <- addRandomDocument (rdaDefault user1) { rdaTypes    = OneOf [Signable]
                                                     , rdaStatuses = OneOf [Closed]
                                                     }
  doc1 <- addRandomDocument (rdaDefault user2) { rdaTypes    = OneOf [Signable]
                                               , rdaStatuses = OneOf [Closed]
                                               }
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc0)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc1)
  res <- dbQuery
    $ GetUsageStats (UsageStatsForUserGroup ugid) PartitionByDay (iyears 2000)
  assertEqual "Documents present in stats" 2 $ length res
  let Just uus1 = find ((email1 ==) . (T.pack . uusUserEmail)) res
      Just uus2 = find ((email2 ==) . (T.pack . uusUserEmail)) res
  assertEqual "Statistics for Emily are correct"
              1
              (dsDocumentsClosed . uusDocumentStats $ uus1)
  assertEqual "Statistics for Bob are correct"
              1
              (dsDocumentsClosed . uusDocumentStats $ uus2)

test_userShareableLinkStatisticsByUser :: TestEnv ()
test_userShareableLinkStatisticsByUser = do
  Just user <- addNewUser "Emily" "Green" "email"
  template  <- addRandomDocumentWithAuthor' user
  doc       <- addRandomDocumentFromShareableLinkWithTemplateId user (documentid template)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc)
  res <- dbQuery $ GetUsageStatsOnShareableLinks (UsageStatsForUser $ userid user)
                                                 PartitionByMonth
                                                 (iyears 2000)
  assertEqual "Document present in stats" 1 (length res)
  let [ShareableLinkUsageStats {..}] = res
  assertEqual "Template ID in statistics is correct"
              (fromDocumentID $ documentid template)
              slusTemplateId
  assertEqual "Template title in statistics is correct"
              (documenttitle template)
              (T.pack slusTemplateTitle)
  assertEqual "Statistics are correct" 1 $ dsDocumentsClosed slusDocumentStats

test_userShareableLinkStatisticsByUserOnlyCorrectUser :: TestEnv ()
test_userShareableLinkStatisticsByUserOnlyCorrectUser = do
  Just user1 <- addNewUser "Emily" "Green" "email"
  Just user2 <- addNewUser "Emily2" "Green" "email2"
  template <- addRandomDocumentWithAuthor' user1
  doc1 <- addRandomDocumentFromShareableLinkWithTemplateId user1 (documentid template)
  doc2 <- addRandomDocumentFromShareableLinkWithTemplateId user2 (documentid template)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc1)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc2)
  res <- dbQuery $ GetUsageStatsOnShareableLinks (UsageStatsForUser $ userid user2)
                                                 PartitionByMonth
                                                 (iyears 2000)
  assertEqual "Document present in stats" 1 (length res)
  let [ShareableLinkUsageStats {..}] = res
  assertEqual "Template ID in statistics is correct"
              (fromDocumentID $ documentid template)
              slusTemplateId
  assertEqual "Template title in statistics is correct"
              (documenttitle template)
              (T.pack slusTemplateTitle)
  assertEqual "Statistics are correct" 1 $ dsDocumentsClosed slusDocumentStats

test_userShareableLinkStatisticsByGroup :: TestEnv ()
test_userShareableLinkStatisticsByGroup = do
  ugid <- view #ugID <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  Just user1 <- addNewUserToUserGroup "Emily" "Green" "emily@green.com" ugid
  Just user2 <- addNewUserToUserGroup "Bob" "Blue" "bob@gblue.com" ugid
  template <- addRandomDocumentWithAuthor' user1
  doc1 <- addRandomDocumentFromShareableLinkWithTemplateId user1 (documentid template)
  doc2 <- addRandomDocumentFromShareableLinkWithTemplateId user2 (documentid template)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc1)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc2)
  res <- dbQuery $ GetUsageStatsOnShareableLinks (UsageStatsForUserGroup ugid)
                                                 PartitionByMonth
                                                 (iyears 2000)
  assertEqual "Document present in stats" 1 (length res)
  let [ShareableLinkUsageStats {..}] = res
  assertEqual "Template ID in statistics is correct"
              (fromDocumentID $ documentid template)
              slusTemplateId
  assertEqual "Template title in statistics is correct"
              (documenttitle template)
              (T.pack slusTemplateTitle)
  assertEqual "Statistics are correct" 2 $ dsDocumentsClosed slusDocumentStats

test_setUserCompany :: TestEnv ()
test_setUserCompany = do
  Just User { userid } <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  ugid                 <- view #ugID <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  res                  <- dbUpdate $ SetUserUserGroup userid ugid
  assertBool "Company was correctly set" res
  Just user <- dbQuery $ GetUserByID userid
  assertBool "Returned user has proper companyid" $ usergroupid user == ugid

test_deleteUser :: TestEnv ()
test_deleteUser = do
  Just User { userid, usergroupid } <- addNewUser "Andrzej"
                                                  "Rybczak"
                                                  "andrzej@skrivapa.se"
  res <- dbUpdate $ DeleteUser userid
  assertBool "User was correctly removed" res
  nouser <- dbQuery $ GetUserByID userid
  assertBool "No user returned after removal" $ isNothing nouser
  ugusers <- dbQuery $ UserGroupGetUsers usergroupid
  assertBool "No users in company after removal" $ null ugusers

test_setUserInfo :: TestEnv ()
test_setUserInfo = do
  Just User { userid, userinfo } <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let ui = userinfo { userpersonalnumber  = "1234567"
                    , usercompanyposition = "blabla"
                    , userphone           = "66346343"
                    }
  res <- dbUpdate $ SetUserInfo userid ui
  assertBool "UserInfo updated correctly" res
  Just User { userinfo = ui2 } <- dbQuery $ GetUserByID userid
  assertBool "Updated UserInfo returned" $ ui == ui2

test_setUserInfoCapEmail :: TestEnv ()
test_setUserInfoCapEmail = do
  Just User { userid, userinfo } <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let ui = userinfo { userpersonalnumber  = "1234567"
                    , usercompanyposition = "blabla"
                    , userphone           = "66346343"
                    , useremail           = Email "DFSFS@fsdfs.com"
                    }
  res <- dbUpdate $ SetUserInfo userid ui
  assertBool "UserInfo updated correctly" res
  Just User { userinfo = ui2 } <- dbQuery $ GetUserByID userid
  assertBool "Updated UserInfo returned"
    $  ui { useremail = Email "dfsfs@fsdfs.com" }
    == ui2

test_setUserSettings :: TestEnv ()
test_setUserSettings = do
  Just User { userid, usersettings } <- addNewUser "Andrzej"
                                                   "Rybczak"
                                                   "andrzej@skrivapa.se"
  let us = usersettings { lang = defaultLang }
  res <- dbUpdate $ SetUserSettings userid us
  assertBool "UserSettings updated correctly" res
  Just User { usersettings = us2 } <- dbQuery $ GetUserByID userid
  assertBool "Updated UserSettings returned" $ us == us2

test_acceptTermsOfService :: TestEnv ()
test_acceptTermsOfService = do
  Just User { userid } <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  now                  <- currentTime
  res                  <- dbUpdate $ AcceptTermsOfService userid now
  assertBool "User updated correctly" res
  Just User { userhasacceptedtermsofservice = Just accepted } <- dbQuery
    $ GetUserByID userid
  assertBool "Time of acceptance is correct" $ compareTime accepted now

test_setSignupMethod :: TestEnv ()
test_setSignupMethod = do
  Just User { userid } <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let method = ViralInvitation
  res <- dbUpdate $ SetSignupMethod userid method
  assertBool "User updated correctly" res
  Just User { usersignupmethod = newmethod } <- dbQuery $ GetUserByID userid
  assertBool "User's updated signup method is correct" $ newmethod == method
