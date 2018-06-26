module UserGroupAccountsTest ( companyAccountsTests
                           , addNewAdminUserAndUserGroup
                           , addNewUserToUserGroup
                           ) where

import Data.Ord
import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON.Gen
import qualified Control.Exception.Lifted as E

import Context
import DB hiding (query, update)
import Doc.DocStateData
import Doc.Model
import InternalResponse
import KontraError
import KontraLink
import Mails.Model
import MinutesTime
import TestingUtil
import TestKontra as T
import User.Email
import User.UserAccountRequest
import UserGroup.Data
import UserGroupAccounts.Model
import UserGroupAccounts.UserGroupAccountsControl
import Util.HasSomeUserInfo

companyAccountsTests :: TestEnvSt -> Test
companyAccountsTests env = testGroup "UserGroupAccounts" [
    testGroup "Model" [
        testThat "Adding an invite for a new email works" env test_addInviteForNewEmail
      , testThat "Removing a existing invite works" env test_removingExistingInvite
      , testThat "Removing a non-existant invite works" env test_removingNonExistantInvite
      ]
  , testGroup "Control" [
        testThat "Admin user can add a new user to their company" env test_addingANewCompanyAccount
      , testThat "Admin user can invite a company user to their company" env test_addingExistingCompanyUserAsCompanyAccount
      , testThat "Admin user can resend invite to a new user" env test_resendingInviteToNewCompanyAccount
      , testThat "Admin user can switch a standard user to admin" env test_switchingStandardToAdminUser
      , testThat "Admin user can switch an admin user to standard" env test_switchingAdminToStandardUser
      , testThat "Admin user can remove a company account invite" env test_removingCompanyAccountInvite
      , testThat "Admin user can remove a company account user" env test_removingCompanyAccountWorks
      , testThat "Existing private user can follow link to be taken over" env test_privateUserTakoverWorks
      , testThat "Company takeovers fail if there is no saved invite" env test_mustBeInvitedForTakeoverToWork
      ]
  ]

test_addInviteForNewEmail :: TestEnv ()
test_addInviteForNewEmail = do
  ug <- addNewUserGroup
  (user1, _) <- addNewAdminUserAndUserGroup "a@a.com" "Anna" "Android"
  _ <- dbUpdate $ AddUserGroupInvite $ mkInvite ug user1
  assertCompanyInvitesAre ug [mkInvite ug user1]

test_removingExistingInvite :: TestEnv ()
test_removingExistingInvite = do
  ug <- addNewUserGroup
  (user, _) <- addNewAdminUserAndUserGroup "a@a.com" "Anna" "Android"

  _ <- dbUpdate $ AddUserGroupInvite $ mkInvite ug user
  _ <- dbUpdate $ RemoveUserGroupInvite (get ugID ug) (userid user)
  assertCompanyInvitesAre ug []

test_removingNonExistantInvite :: TestEnv ()
test_removingNonExistantInvite = do
  ug <- addNewUserGroup
  _ <- dbUpdate $ RemoveUserGroupInvite (get ugID ug) (unsafeUserID 0)
  assertCompanyInvitesAre ug []

test_addingANewCompanyAccount :: TestEnv ()
test_addingANewCompanyAccount = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- (set ctxmaybeuser (Just user))
    <$> mkContext def

  req <- mkRequest POST [ ("add", inText "True")
                        , ("email", inText "bob@blue.com")
                        , ("fstname", inText "Bob")
                        , ("sndname", inText "Blue")
                        ]
  (res, _) <- runTestKontra req ctx $ handleAddUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)


  Just newuser <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  assertEqual "New user is in company" (get ugID ug) (usergroupid newuser)
  assertEqual "New user is standard user" False (useriscompanyadmin newuser)
  assertEqual "New user has the invited name" "Bob Blue" (getFullName newuser)

  assertCompanyInvitesAre ug [] -- We don't add an invite if we created a user

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingCompanyUserAsCompanyAccount :: TestEnv ()
test_addingExistingCompanyUserAsCompanyAccount = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (existinguser, existingug) <- addNewAdminUserAndUserGroup "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def
  req <- mkRequest POST [ ("add", inText "True")
                        , ("email", inText "bob@blue.com")
                        , ("fstname", inText "Bob")
                        , ("sndname", inText "Blue")
                        ]
  (res, _) <- runTestKontra req ctx $ handleAddUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)

  Just updatedexistinguser <- dbQuery $ GetUserByID (userid existinguser)
  assertEqual "Invited user's company stays the same" (usergroupid updatedexistinguser)
                                                      (get ugID existingug)

  assertCompanyInvitesAre ug [mkInvite ug existinguser]

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_resendingInviteToNewCompanyAccount :: TestEnv ()
test_resendingInviteToNewCompanyAccount = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just newuser <- addNewUserToUserGroup "Bob" "Blue" "bob@blue.com" (get ugID ug)
  _ <- dbUpdate $ AddUserGroupInvite $ mkInvite ug newuser

  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def

  req <- mkRequest POST [ ("resend", inText "True")
                        , ("resendid", inText $ show (userid newuser))
                        , ("resendemail", inText "bob@blue.com")
                        , ("sndname", inText "Blue")
                        ]
  (res, _) <- runTestKontra req ctx $ handleResendToUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "resent" True)

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_switchingStandardToAdminUser :: TestEnv ()
test_switchingStandardToAdminUser = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewUserToUserGroup "Bob" "Blue" "bob@blue.com" (get ugID ug)

  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def

  req <- mkRequest POST [ ("changerole", inText "True")
                        , ("changeid", inText $ show (userid standarduser))
                        , ("makeadmin", inText $ "true")
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handleChangeRoleOfUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "changed" True)

  Just updateduser <- dbQuery $ GetUserByID (userid standarduser)
  assertBool "User is now an admin" (useriscompanyadmin updateduser)
  assertEqual "User belongs to the same company" (usergroupid updateduser)
                                                 (get ugID ug)

test_switchingAdminToStandardUser :: TestEnv ()
test_switchingAdminToStandardUser = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewUserToUserGroup "Bob" "Blue" "bob@blue.com" (get ugID ug)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid standarduser) True
  Just adminuser <- dbQuery $ GetUserByID (userid user)

  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def

  req <- mkRequest POST [ ("changerole", inText "True")
                        , ("changeid", inText $ show (userid adminuser))
                        , ("makeadmin", inText $ "false")
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handleChangeRoleOfUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "changed" True)

  Just updateduser <- dbQuery $ GetUserByID (userid adminuser)
  assertBool "User is now standard" (not $ useriscompanyadmin updateduser)
  assertEqual "User belongs to the same company" (usergroupid updateduser)
                                                 (get ugID ug)

test_removingCompanyAccountInvite :: TestEnv ()
test_removingCompanyAccountInvite = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (standarduser,_) <- addNewAdminUserAndUserGroup "Bob" "Blue" "jony@blue.com"

  _ <- dbUpdate $ AddUserGroupInvite $ mkInvite ug standarduser

  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def

  req <- mkRequest POST [ ("remove", inText "True")
                        , ("removeid", inText $ show $ userid standarduser)
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handleRemoveUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "removed" True)

  assertCompanyInvitesAre ug []

test_removingCompanyAccountWorks :: TestEnv ()
test_removingCompanyAccountWorks = do
  (adminuser, ug) <- addNewAdminUserAndUserGroup "Anna" "Android" "anna@android.com"
  Just standarduser <- addNewUserToUserGroup "Bob" "Blue" "jony@blue.com" (get ugID ug)
  doc <- addRandomDocumentWithAuthorAndCondition standarduser (\d -> documentstatus d `elem` [Closed])
  let docid = documentid doc

  _ <- dbUpdate $ AddUserGroupInvite $ mkInvite ug standarduser

  ctx <- (set ctxmaybeuser (Just adminuser)) <$> mkContext def

  companydocs1 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid adminuser) [DocumentFilterUnsavedDraft False] [] maxBound
  assertEqual "Company admin sees users docs before user delete" 1 (length companydocs1)
  assertEqual "Docid matches before user delete" docid (documentid $ head companydocs1)

  req <- mkRequest POST [ ("remove", inText "True")
                        , ("removeid", inText $ show (userid standarduser))
                        , ("removeemail", inText $ "jony@blue.com")
                        ]
  (res, _) <- runTestKontra req ctx $ handleRemoveUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "removed" True)

  deleteduser <- dbQuery $ GetUserByID (userid standarduser)
  assertEqual "User has been deleted" Nothing deleteduser

  assertCompanyInvitesAre ug []

  companydocs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid adminuser) [DocumentFilterUnsavedDraft False] [] maxBound
  assertEqual "Company admin sees users docs after user delete" 1 (length companydocs)
  assertEqual "Docid matches after user delete" docid (documentid $ head companydocs)

test_privateUserTakoverWorks :: TestEnv ()
test_privateUserTakoverWorks = do
  (adminuser, ug) <- addNewAdminUserAndUserGroup "Anna" "Android" "anna@android.com"
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  docid <- documentid <$> addRandomDocumentWithAuthorAndCondition user (\d -> not $ (documentstatus d) `elem` [Preparation])


  _ <- dbUpdate $ AddUserGroupInvite $ mkInvite ug user

  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def

  req <- mkRequest POST []
  (res, _) <- runTestKontra req ctx $ handlePostBecomeUserGroupAccount (get ugID ug)
  assertBool "Response is redirect" (isRedirect LinkAccount res)
  assertBool "Response has flash message redirect" (hasFlashMessage res)
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  assertEqual "User belongs to the company" (usergroupid updateduser)
                                            (get ugID ug)
  assertBool "User is a standard user" (not $ useriscompanyadmin updateduser)

  companydocs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid adminuser) [DocumentFilterUnsavedDraft False] [] maxBound
  assertEqual "Company owns users docs" 1 (length companydocs)
  assertEqual "Docid matches" docid (documentid $ head companydocs)
  docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid user) [DocumentFilterUnsavedDraft False, DocumentFilterSignable] [] maxBound
  templates <- dbQuery $ GetTemplatesByAuthor $ userid user
  let userdocids = nub (documentid <$> docs ++ templates)
  assertEqual "User is still linked to their docs" 1 (length userdocids)
  assertEqual "Docid matches" docid (head userdocids)

test_mustBeInvitedForTakeoverToWork :: TestEnv ()
test_mustBeInvitedForTakeoverToWork = do
  ug <- addNewUserGroup
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"

  ctx <- (set ctxmaybeuser (Just user))
    <$> mkContext def

  req <- mkRequest POST []
  (l, _ctx') <- runTestKontra req ctx $
    (handlePostBecomeUserGroupAccount (get ugID ug) >> return False)
      `E.catch` (\(_::KontraError) -> return True)
  assertEqual "Exception thrown" True l
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  assertBool "User is still not in the other company" ((usergroupid user) == (usergroupid updateduser))

assertCompanyInvitesAre :: UserGroup -> [UserGroupInvite] -> TestEnv ()
assertCompanyInvitesAre ug expectedinvites = do
  actualinvites <- dbQuery $ UserGroupGetInvites (get ugID ug)
  assertEqual "Wrong company invites" (inviteSort expectedinvites)
                                      (inviteSort actualinvites)
  mapM_ assertInviteExists expectedinvites
  where
    inviteSort = sortBy (comparing inviteduserid)
    assertInviteExists expectedinvite = do
      mactualinvite <- dbQuery $ GetUserGroupInvite (get ugID ug) (inviteduserid expectedinvite)
      assertEqual "Wrong company invite" (Just expectedinvite) mactualinvite

addNewAdminUserAndUserGroup :: String -> String -> String -> TestEnv (User, UserGroup)
addNewAdminUserAndUserGroup fstname sndname email = do
  ug <- addNewUserGroup
  Just user <- addNewUserToUserGroup fstname sndname email (get ugID ug)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  return (updateduser, ug)

mkInvite :: UserGroup -> User -> UserGroupInvite
mkInvite ug user =
  UserGroupInvite {
      inviteduserid= userid user
    , invitingusergroup = get ugID ug
  }

getAccountCreatedActions :: TestEnv [UserAccountRequest]
getAccountCreatedActions = do
  expirytime <- (30 `daysAfter`) <$> currentTime
  dbQuery $ GetExpiredUserAccountRequestsForTesting expirytime
