module UserStateTest (userStateTests) where

import Test.Framework
import qualified Data.Set as S
import qualified Data.Text as T

import Chargeable.Model
import DB
import Doc.DocumentID
import Doc.Types.Document
import Doc.Types.DocumentStatus
import MinutesTime
import Tag
import TestingUtil
import TestKontra
import User.Email
import User.Model
import UserGroup.Model
import UserGroup.Types

sortByEmail :: [User] -> [User]
sortByEmail = sortBy (\a b -> compare (f a) (f b)) where f = (^. #info % #email)

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
  , testThat "SetUserTags works"          env test_setUserTags
  ]

test_getUserByEmail_returnsNothing :: TestEnv ()
test_getUserByEmail_returnsNothing = do
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isNothing queriedUser)

test_getUserByEmail_returnsTheRightUser :: TestEnv ()
test_getUserByEmail_returnsTheRightUser = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Emily"
                                               , lastName  = return "Green"
                                               , email     = return "emily@green.com"
                                               }
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isJust queriedUser)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "EMILY@green.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)
  user3 <- instantiateUser $ randomUserTemplate { firstName = return "Eric"
                                                , lastName  = return "Normand"
                                                , email     = return "ERIc@Normand.Com"
                                                }
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
  user <- instantiateUser $ randomUserTemplate { firstName = return "Emily"
                                               , lastName  = return "Green"
                                               , email     = return "emiy@green.com"
                                               }
  queriedUser <- dbQuery $ GetUserByID $ user ^. #id
  assert (isJust queriedUser)
  assertEqual "For GetUserByUserID result" user (fromJust queriedUser)

test_setUserEmail_GetByEmail :: TestEnv ()
test_setUserEmail_GetByEmail = do
  user' <- instantiateUser $ randomUserTemplate { firstName = return "Emily"
                                                , lastName  = return "Green"
                                                , email     = return "emily@green.com"
                                                }
  void $ dbUpdate $ SetUserEmail (user' ^. #id) $ Email "Emily@green.coM"
  Just user   <- dbQuery $ GetUserByID $ user' ^. #id
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isJust queriedUser)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "EMILY@green.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)

test_setUserEmail_works :: TestEnv ()
test_setUserEmail_works = do
  user' <- instantiateUser $ randomUserTemplate { firstName = return "Emily"
                                                , lastName  = return "Green"
                                                , email     = return "emily@green.com"
                                                }
  void $ dbUpdate $ SetUserEmail (user' ^. #id) $ Email "other@email.com"
  Just user   <- dbQuery $ GetUserByID $ user' ^. #id
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert (isNothing queriedUser)
  queriedUser2 <- dbQuery $ GetUserByEmail (Email "Other@EmAil.com")
  assert (isJust queriedUser2)
  assertEqual "For GetUserByEmail result" user (fromJust queriedUser2)

test_setUserPassword_changesPassword :: TestEnv ()
test_setUserPassword_changesPassword = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Emily"
                                               , lastName  = return "Green"
                                               , email     = return "emily@green.com"
                                               }
  passwordhash <- createPassword "Secret Password!"
  void $ dbUpdate $ SetUserPassword (user ^. #id) passwordhash
  queriedUser <- dbQuery $ GetUserByEmail (Email "emily@green.com")
  assert $ maybeVerifyPassword (fromJust queriedUser ^. #password) "Secret Password!"

test_addUser_repeatedEmailReturnsNothing :: TestEnv ()
test_addUser_repeatedEmailReturnsNothing = do
  Just _ <- tryInstantiateUser $ randomUserTemplate { firstName = return "Emily"
                                                    , lastName  = return "Green"
                                                    , email     = return "emily@green.com"
                                                    }
  result <- tryInstantiateUser $ randomUserTemplate { firstName = return "Emily"
                                                    , lastName  = return "Green Again"
                                                    , email     = return "emily@green.com"
                                                    }
  assert $ isNothing result

test_userGroupGetUsers :: TestEnv ()
test_userGroupGetUsers = do
  ugid <- view #id <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  let emails = ["emily@green.com", "emily2@green.com", "andrzej@skrivapa.se"]
  users <- forM emails $ \email ->
    instantiateUser $ randomUserTemplate { email = return email, groupID = return ugid }
  ugAccounts1 <- dbQuery $ UserGroupGetUsers ugid
  assertBool "Company accounts returned in proper order (sorted by email)"
    $  sortByEmail users
    == ugAccounts1
  res <- dbUpdate . DeleteUser . view #id . head . sortByEmail $ users
  assertBool "User was deleted" res
  do
    ugAccounts2 <- dbQuery $ UserGroupGetUsers ugid
    assertBool "Company accounts returned in proper order (sorted by email)"
      $  tail (sortByEmail users)
      == ugAccounts2
  do
    ugAccountsAll <- dbQuery $ UserGroupGetUsersIncludeDeleted ugid
    assertBool "Company accounts returned in proper order (sorted by email)"
      $  map (^. #id) (sortByEmail users)
      == map (^. #id) ugAccountsAll

test_userUsageStatisticsByUser :: TestEnv ()
test_userUsageStatisticsByUser = do
  let email = "emily@green.com"
  user <- instantiateUser $ randomUserTemplate { firstName = return "Emily"
                                               , lastName  = return "Green"
                                               , email     = return email
                                               }
  doc <- addRandomDocument (rdaDefault user) { rdaTypes    = OneOf [Signable]
                                             , rdaStatuses = OneOf [Closed]
                                             }
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc)
  res <- dbQuery
    $ GetUsageStats (UsageStatsForUser $ user ^. #id) PartitionByMonth (iyears 2000)
  assertEqual "Document present in stats" 1 (length res)
  let [UserUsageStats {..}] = res
  assertEqual "Email in statistics is correct" email         (T.pack uusUserEmail)
  assertEqual "Name in statistics is correct"  "Emily Green" uusUserName
  assertEqual "Statistics are correct" 1 $ dsDocumentsClosed uusDocumentStats

test_userUsageStatisticsByCompany :: TestEnv ()
test_userUsageStatisticsByCompany = do
  let email1 = "emily@green.com"
      email2 = "bob@gblue.com"
  ugid  <- view #id <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  user1 <- instantiateUser
    $ randomUserTemplate { email = return email1, groupID = return ugid }

  user2 <- instantiateUser
    $ randomUserTemplate { email = return email2, groupID = return ugid }
  doc0 <- addRandomDocument (rdaDefault user1) { rdaTypes    = OneOf [Signable]
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
  user <- instantiateUser $ randomUserTemplate { firstName = return "Emily"
                                               , lastName  = return "Green"
                                               , email     = return "email"
                                               }
  template <- addRandomDocumentWithAuthor' user
  doc      <- addRandomDocumentFromShareableLinkWithTemplateId user (documentid template)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc)
  res <- dbQuery $ GetUsageStatsOnShareableLinks (UsageStatsForUser $ user ^. #id)
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
  user1 <- instantiateUser $ randomUserTemplate { firstName = return "Emily"
                                                , lastName  = return "Green"
                                                , email     = return "email"
                                                }
  user2 <- instantiateUser $ randomUserTemplate { firstName = return "Emily2"
                                                , lastName  = return "Green"
                                                , email     = return "email2"
                                                }
  template <- addRandomDocumentWithAuthor' user1
  doc1     <- addRandomDocumentFromShareableLinkWithTemplateId user1 (documentid template)
  doc2     <- addRandomDocumentFromShareableLinkWithTemplateId user2 (documentid template)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc1)
  void $ dbUpdate (ChargeUserGroupForClosingDocument $ documentid doc2)
  res <- dbQuery $ GetUsageStatsOnShareableLinks (UsageStatsForUser $ user2 ^. #id)
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
  ugid     <- view #id <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  user1    <- instantiateUser $ randomUserTemplate { groupID = return ugid }
  user2    <- instantiateUser $ randomUserTemplate { groupID = return ugid }
  template <- addRandomDocumentWithAuthor' user1
  doc1     <- addRandomDocumentFromShareableLinkWithTemplateId user1 (documentid template)
  doc2     <- addRandomDocumentFromShareableLinkWithTemplateId user2 (documentid template)
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
  uid <- fmap (view #id) . instantiateUser $ randomUserTemplate
    { firstName = return "Andrzej"
    , lastName  = return "Rybczak"
    , email     = return "andrzej@skrivapa.se"
    }
  ugid <- view #id <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  res  <- dbUpdate $ SetUserUserGroup uid ugid
  assertBool "Company was correctly set" res
  Just user <- dbQuery $ GetUserByID uid
  assertBool "Returned user has proper companyid" $ user ^. #groupID == ugid

test_deleteUser :: TestEnv ()
test_deleteUser = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Andrzej"
                                               , lastName  = return "Rybczak"
                                               , email     = return "andrzej@skrivapa.se"
                                               }
  res <- dbUpdate $ DeleteUser $ user ^. #id
  assertBool "User was correctly removed" res
  nouser <- dbQuery $ GetUserByID $ user ^. #id
  assertBool "No user returned after removal" $ isNothing nouser
  ugusers <- dbQuery $ UserGroupGetUsers $ user ^. #groupID
  assertBool "No users in company after removal" $ null ugusers

test_setUserInfo :: TestEnv ()
test_setUserInfo = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Andrzej"
                                               , lastName  = return "Rybczak"
                                               , email     = return "andrzej@skrivapa.se"
                                               }
  let ui =
        (user ^. #info)
          & (#personalNumber .~ "1234567")
          & (#companyPosition .~ "blabla")
          & (#phone .~ "66346343")
  res <- dbUpdate $ SetUserInfo (user ^. #id) ui
  assertBool "UserInfo updated correctly" res
  Just user2 <- dbQuery $ GetUserByID (user ^. #id)
  assertBool "Updated UserInfo returned" $ ui == user2 ^. #info

test_setUserInfoCapEmail :: TestEnv ()
test_setUserInfoCapEmail = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Andrzej"
                                               , lastName  = return "Rybczak"
                                               , email     = return "andrzej@skrivapa.se"
                                               }
  let ui =
        (user ^. #info)
          & (#personalNumber .~ "1234567")
          & (#companyPosition .~ "blabla")
          & (#phone .~ "66346343")
          & (#email .~ Email "DFSFS@fsdfs.com")
  res <- dbUpdate $ SetUserInfo (user ^. #id) ui
  assertBool "UserInfo updated correctly" res
  Just user2 <- dbQuery $ GetUserByID (user ^. #id)
  assertBool "Updated UserInfo returned"
    $  (ui & #email .~ Email "dfsfs@fsdfs.com")
    == (user2 ^. #info)

test_setUserSettings :: TestEnv ()
test_setUserSettings = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Andrzej"
                                               , lastName  = return "Rybczak"
                                               , email     = return "andrzej@skrivapa.se"
                                               }
  let us = user ^. #settings & #lang .~ defaultLang
  res <- dbUpdate $ SetUserSettings (user ^. #id) us
  assertBool "UserSettings updated correctly" res
  Just user2 <- dbQuery $ GetUserByID (user ^. #id)
  assertBool "Updated UserSettings returned" $ us == user2 ^. #settings

test_acceptTermsOfService :: TestEnv ()
test_acceptTermsOfService = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Andrzej"
                                               , lastName  = return "Rybczak"
                                               , email     = return "andrzej@skrivapa.se"
                                               }
  now <- currentTime
  res <- dbUpdate $ AcceptTermsOfService (user ^. #id) now
  assertBool "User updated correctly" res
  Just accepted <- preview (_Just % #hasAcceptedTOS % _Just)
    <$> dbQuery (GetUserByID $ user ^. #id)
  assertBool "Time of acceptance is correct" $ compareTime accepted now

test_setSignupMethod :: TestEnv ()
test_setSignupMethod = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Andrzej"
                                               , lastName  = return "Rybczak"
                                               , email     = return "andrzej@skrivapa.se"
                                               }
  let method = ViralInvitation
  res <- dbUpdate $ SetSignupMethod (user ^. #id) method
  assertBool "User updated correctly" res
  Just newUser <- dbQuery $ GetUserByID (user ^. #id)
  assertBool "User's updated signup method is correct"
             (newUser ^. #signupMethod == method)

test_setUserTags :: TestEnv ()
test_setUserTags = do
  user <- instantiateRandomUser
  let internalTags = S.fromList [Tag "foo" "bar"]
  let externalTags = S.fromList [Tag "bar" "baz"]
  dbUpdate $ SetUserTags (user ^. #id) internalTags externalTags
  Just updatedUser <- dbQuery $ GetUserByID (user ^. #id)
  assertBool "User's internal tags are updated correctly"
             (updatedUser ^. #internalTags == internalTags)
  assertBool "User's external tags are updated correctly"
             (updatedUser ^. #externalTags == externalTags)
