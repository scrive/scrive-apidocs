module UserGroupAccountsTest ( companyAccountsTests
                           , addNewAdminUserAndUserGroup
                           , addNewUserToUserGroup
                           ) where

import Data.Ord
import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON.Gen
import qualified Control.Exception.Lifted as E

import AccessControl.Model
import AccessControl.Types
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
import UserGroup.Types
import UserGroupAccounts.Model
import UserGroupAccounts.UserGroupAccountsControl
import Util.HasSomeUserInfo

companyAccountsTests :: TestEnvSt -> Test
companyAccountsTests env = testGroup
  "UserGroupAccounts"
  [ testGroup
    "Model"
    [ testThat "Adding an invite for a new email works" env test_addInviteForNewEmail
    , testThat "Removing a existing invite works"       env test_removingExistingInvite
    , testThat "Removing a non-existant invite works"   env test_removingNonExistantInvite
    ]
  , testGroup
    "Control"
    [ testThat "Admin user can add a new user to their user group"
               env
               test_addingANewCompanyAccount
    , testThat "Admin user can invite an existing user to their user group"
               env
               test_addingExistingCompanyUserAsCompanyAccount
    , testThat
      "Admin user can add a new user to their user group with non-default user group targets"
      env
      test_addingANewCompanyAccountWithDifferentTarget
    , testThat
      "Admin user can invite an existing user to their user group with non-default target"
      env
      test_addingExistingCompanyUserAsCompanyAccountWithDifferentTarget
    , testThat "Admin user can resend invite to a new user"
               env
               test_resendingInviteToNewCompanyAccount
    , testThat "Admin user can switch a standard user to admin"
               env
               test_switchingStandardToAdminUser
    , testThat "Admin user can switch an admin user to standard"
               env
               test_switchingAdminToStandardUser
    , testThat "Admin user can remove a company account invite"
               env
               test_removingCompanyAccountInvite
    , testThat "Admin user can remove a company account user"
               env
               test_removingCompanyAccountWorks
    , testThat "Existing private user can follow link to be taken over"
               env
               test_privateUserTakoverWorks
    , testThat "Company takeovers fail if there is no saved invite"
               env
               test_mustBeInvitedForTakeoverToWork
    ]
  ]

test_addInviteForNewEmail :: TestEnv ()
test_addInviteForNewEmail = do
  ug         <- addNewUserGroup
  (user1, _) <- addNewAdminUserAndUserGroup "a@a.com" "Anna" "Android"
  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug user1
  assertCompanyInvitesAre ug [mkInvite ug user1]

test_removingExistingInvite :: TestEnv ()
test_removingExistingInvite = do
  ug        <- addNewUserGroup
  (user, _) <- addNewAdminUserAndUserGroup "a@a.com" "Anna" "Android"

  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug user
  void $ dbUpdate $ RemoveUserGroupInvite [ug ^. #id] (userid user)
  assertCompanyInvitesAre ug []

test_removingNonExistantInvite :: TestEnv ()
test_removingNonExistantInvite = do
  ug <- addNewUserGroup
  void $ dbUpdate $ RemoveUserGroupInvite [ug ^. #id] (unsafeUserID 0)
  assertCompanyInvitesAre ug []

test_addingANewCompanyAccount :: TestEnv ()
test_addingANewCompanyAccount = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req        <- mkRequest
    POST
    [ ("add"    , inText "True")
    , ("email"  , inText "bob@blue.com")
    , ("fstname", inText "Bob")
    , ("sndname", inText "Blue")
    ]
  (res, _) <- runTestKontra req ctx $ handleAddUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)


  Just newuser <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  assertEqual "New user is in company"        (ug ^. #id) (usergroupid newuser)
  assertEqual "New user is standard user"     False       (useriscompanyadmin newuser)
  assertEqual "New user has the invited name" "Bob Blue"  (getFullName newuser)

  assertCompanyInvitesAre ug [] -- We don't add an invite if we created a user

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingCompanyUserAsCompanyAccount :: TestEnv ()
test_addingExistingCompanyUserAsCompanyAccount = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (existinguser, existingug) <- addNewAdminUserAndUserGroup "Bob" "Blue" "bob@blue.com"
  ctx <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  req <- mkRequest
    POST
    [ ("add"    , inText "True")
    , ("email"  , inText "bob@blue.com")
    , ("fstname", inText "Bob")
    , ("sndname", inText "Blue")
    ]
  (res, _) <- runTestKontra req ctx $ handleAddUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)

  Just updatedexistinguser <- dbQuery $ GetUserByID (userid existinguser)
  assertEqual "Invited user's company stays the same"
              (usergroupid updatedexistinguser)
              (existingug ^. #id)

  assertCompanyInvitesAre ug [mkInvite ug existinguser]

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_addingANewCompanyAccountWithDifferentTarget :: TestEnv ()
test_addingANewCompanyAccountWithDifferentTarget = do
  let email = "andrzej@skrivapa.se"
  (user, _ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" email
  trgug       <- addNewCompany False
  let trgugid = trgug ^. #id

  ctx <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req <- mkRequest
    POST
    [ ("add"          , inText "True")
    , ("email"        , inText "bob@blue.com")
    , ("fstname"      , inText "Bob")
    , ("sndname"      , inText "Blue")
    , ("user_group_id", inText $ showt trgugid)
    ]

  -- user does not yet have permission to move mvuser. Failure expected.
  assertRaisesInternalError (void $ runTestKontra req ctx handleAddUserGroupAccount)

  -- user is given permission on target user group, so addition expected
  void . dbUpdate . AccessControlCreateForUser (userid user) $ UserGroupAdminAR trgugid
  (res, _) <- runTestKontra req ctx handleAddUserGroupAccount
  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)
  Just newuser <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  assertEqual "New user is in target user group" trgugid    (usergroupid newuser)
  assertEqual "New user is standard user"        False      (useriscompanyadmin newuser)
  assertEqual "New user has the invited name"    "Bob Blue" (getFullName newuser)
  assertCompanyInvitesAre trgug []

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingCompanyUserAsCompanyAccountWithDifferentTarget :: TestEnv ()
test_addingExistingCompanyUserAsCompanyAccountWithDifferentTarget = do
  (user, _ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (existinguser, existingug) <- addNewAdminUserAndUserGroup "Bob" "Blue" "bob@blue.com"
  trgug <- addNewCompany False
  let trgugid = trgug ^. #id

  ctx <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  req <- mkRequest
    POST
    [ ("add"          , inText "True")
    , ("email"        , inText "bob@blue.com")
    , ("fstname"      , inText "Bob")
    , ("sndname"      , inText "Blue")
    , ("user_group_id", inText $ showt trgugid)
    ]
  -- user does not yet have permission to move existinguser; failure expected.
  assertRaisesInternalError (void $ runTestKontra req ctx handleAddUserGroupAccount)

  -- user is given permission on target user group; invite expected
  void . dbUpdate . AccessControlCreateForUser (userid user) $ UserGroupAdminAR trgugid
  (res, _) <- runTestKontra req ctx $ handleAddUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)

  Just updatedexistinguser <- dbQuery $ GetUserByID (userid existinguser)
  assertEqual "Invited user's company stays the same"
              (usergroupid updatedexistinguser)
              (existingug ^. #id)

  assertCompanyInvitesAre trgug [mkInvite trgug existinguser]

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_resendingInviteToNewCompanyAccount :: TestEnv ()
test_resendingInviteToNewCompanyAccount = do
  (user, ug)   <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just newuser <- addNewUserToUserGroup "Bob" "Blue" "bob@blue.com" (ug ^. #id)
  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug newuser

  ctx <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req <- mkRequest
    POST
    [ ("resend"     , inText "True")
    , ("resendid"   , inText $ showt (userid newuser))
    , ("resendemail", inText "bob@blue.com")
    , ("sndname"    , inText "Blue")
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
  Just standarduser <- addNewUserToUserGroup "Bob" "Blue" "bob@blue.com" (ug ^. #id)

  ctx               <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req               <- mkRequest
    POST
    [ ("changerole", inText "True")
    , ("changeid", inText $ showt (userid standarduser))
    , ("makeadmin" , inText $ "true")
    ]
  (res, _ctx') <- runTestKontra req ctx $ handleChangeRoleOfUserGroupAccount

  assertBool "Response is proper JSON" $ res == (runJSONGen $ value "changed" True)

  Just updateduser <- dbQuery $ GetUserByID (userid standarduser)
  assertBool "User is now an admin" (useriscompanyadmin updateduser)
  assertEqual "User belongs to the same company" (usergroupid updateduser) (ug ^. #id)

test_switchingAdminToStandardUser :: TestEnv ()
test_switchingAdminToStandardUser = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewUserToUserGroup "Bob" "Blue" "bob@blue.com" (ug ^. #id)
  void $ dbUpdate $ SetUserCompanyAdmin (userid standarduser) True
  Just adminuser <- dbQuery $ GetUserByID (userid user)

  ctx            <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req            <- mkRequest
    POST
    [ ("changerole", inText "True")
    , ("changeid"  , inText $ showt (userid adminuser))
    , ("makeadmin" , inText $ "false")
    ]
  (res, _ctx') <- runTestKontra req ctx $ handleChangeRoleOfUserGroupAccount

  assertBool "Response is proper JSON" $ res == (runJSONGen $ value "changed" True)

  Just updateduser <- dbQuery $ GetUserByID (userid adminuser)
  assertBool "User is now standard" (not $ useriscompanyadmin updateduser)
  assertEqual "User belongs to the same company" (usergroupid updateduser) (ug ^. #id)

test_removingCompanyAccountInvite :: TestEnv ()
test_removingCompanyAccountInvite = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (standarduser, _) <- addNewAdminUserAndUserGroup "Bob" "Blue" "jony@blue.com"

  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug standarduser

  ctx          <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req          <- mkRequest POST [("removeid", inText $ showt $ userid standarduser)]
  (res, _ctx') <- runTestKontra req ctx $ handleRemoveUserGroupAccount

  assertBool "Response is proper JSON" $ res == (runJSONGen $ value "removed" True)

  assertCompanyInvitesAre ug []

test_removingCompanyAccountWorks :: TestEnv ()
test_removingCompanyAccountWorks = do
  (adminuser, ug) <- addNewAdminUserAndUserGroup "Anna" "Android" "anna@android.com"
  Just standarduser <- addNewUserToUserGroup "Bob" "Blue" "jony@blue.com" (ug ^. #id)
  doc <- addRandomDocument (rdaDefault standarduser) { rdaTypes    = OneOf [Signable]
                                                     , rdaStatuses = OneOf [Closed]
                                                     }
  let docid = documentid doc

  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug standarduser

  ctx          <- (set #maybeUser (Just adminuser)) <$> mkContext defaultLang

  companydocs1 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid adminuser)
                                         [DocumentFilterUnsavedDraft False]
                                         []
                                         maxBound
  assertEqual "Company admin sees users docs before user delete" 1 (length companydocs1)
  assertEqual "Docid matches before user delete" docid (documentid $ head companydocs1)

  req      <- mkRequest POST [("removeid", inText $ showt (userid standarduser))]
  (res, _) <- runTestKontra req ctx $ handleRemoveUserGroupAccount

  assertBool "Response is proper JSON" $ res == (runJSONGen $ value "removed" True)

  deleteduser <- dbQuery $ GetUserByID (userid standarduser)
  assertEqual "User has been deleted" Nothing deleteduser

  assertCompanyInvitesAre ug []

  companydocs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid adminuser)
                                        [DocumentFilterUnsavedDraft False]
                                        []
                                        maxBound
  assertEqual "Company admin sees users docs after user delete" 1 (length companydocs)
  assertEqual "Docid matches after user delete" docid (documentid $ head companydocs)

  -- test removal with access control only
  (otheruser, otherug) <- addNewAdminUserAndUserGroup "Mad Jack"
                                                      "Churchill"
                                                      "madjack@example.com"
  req'      <- mkRequest POST [("removeid", inText $ showt (userid otheruser))]

  -- shouldn't work
  (res', _) <- runTestKontra req' ctx $ handleRemoveUserGroupAccount
  -- the `True` value here is a bit confusing in this case
  assertBool "Response is proper JSON" $ res' == (runJSONGen $ value "removed" True)
  motheruserDB <- dbQuery $ GetUserByID (userid otheruser)
  assertEqual "User has NOT been deleted" (Just otheruser) motheruserDB

  -- allow adminuser control over otheruser's group
  void $ dbUpdate $ AccessControlCreateForUserGroup (ug ^. #id)
                                                    (UserAdminAR $ otherug ^. #id)

  -- now removal should work
  (res'', _) <- runTestKontra req' ctx $ handleRemoveUserGroupAccount
  assertBool "Response is proper JSON" $ res'' == (runJSONGen $ value "removed" True)
  otheruserDB' <- dbQuery $ GetUserByID (userid otheruser)
  assertEqual "User has been deleted" Nothing otheruserDB'

test_privateUserTakoverWorks :: TestEnv ()
test_privateUserTakoverWorks = do
  (adminuser, ug) <- addNewAdminUserAndUserGroup "Anna" "Android" "anna@android.com"
  Just user       <- addNewUser "Bob" "Blue" "bob@blue.com"
  docid           <- documentid <$> addRandomDocument (rdaDefault user)
    { rdaTypes    = OneOf [Signable]
    , rdaStatuses = OneOf $ documentAllStatuses \\ [Preparation]
    }
  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug user

  ctx      <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req      <- mkRequest POST []
  (res, _) <- runTestKontra req ctx $ handlePostBecomeUserGroupAccount (ug ^. #id)
  assertBool "Response is redirect"                (isRedirect LinkAccount res)
  assertBool "Response has flash message redirect" (hasFlashMessage res)
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  assertEqual "User belongs to the company" (usergroupid updateduser) (ug ^. #id)
  assertBool "User is a standard user" (not $ useriscompanyadmin updateduser)

  companydocs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid adminuser)
                                        [DocumentFilterUnsavedDraft False]
                                        []
                                        maxBound
  assertEqual "Company owns users docs" 1     (length companydocs)
  assertEqual "Docid matches"           docid (documentid $ head companydocs)
  docs <- dbQuery $ GetDocuments
    (DocumentsVisibleToUser $ userid user)
    [DocumentFilterUnsavedDraft False, DocumentFilterSignable]
    []
    maxBound
  templates <- dbQuery $ GetTemplatesByAuthor $ userid user
  let userdocids = nub (documentid <$> docs ++ templates)
  assertEqual "User is still linked to their docs" 1     (length userdocids)
  assertEqual "Docid matches" docid (head userdocids)

test_mustBeInvitedForTakeoverToWork :: TestEnv ()
test_mustBeInvitedForTakeoverToWork = do
  ug         <- addNewUserGroup
  Just user  <- addNewUser "Bob" "Blue" "bob@blue.com"

  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req        <- mkRequest POST []
  (l, _ctx') <-
    runTestKontra req ctx
    $         (handlePostBecomeUserGroupAccount (ug ^. #id) >> return False)
    `E.catch` (\(_ :: KontraError) -> return True)
  assertEqual "Exception thrown" True l
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  assertBool "User is still not in the other company"
             ((usergroupid user) == (usergroupid updateduser))

assertCompanyInvitesAre :: UserGroup -> [UserGroupInvite] -> TestEnv ()
assertCompanyInvitesAre ug expectedinvites = do
  actualinvites <- dbQuery $ UserGroupGetInvites (ug ^. #id)
  assertEqual "Wrong company invites"
              (inviteSort expectedinvites)
              (inviteSort actualinvites)
  mapM_ assertInviteExists expectedinvites
  where
    inviteSort = sortBy (comparing inviteduserid)
    assertInviteExists expectedinvite = do
      mactualinvite <- dbQuery
        $ GetUserGroupInvite (ug ^. #id) (inviteduserid expectedinvite)
      assertEqual "Wrong company invite" (Just expectedinvite) mactualinvite

mkInvite :: UserGroup -> User -> UserGroupInvite
mkInvite ug user =
  UserGroupInvite { inviteduserid = userid user, invitingusergroup = ug ^. #id }

getAccountCreatedActions :: TestEnv [UserAccountRequest]
getAccountCreatedActions = do
  expirytime <- (30 `daysAfter`) <$> currentTime
  dbQuery $ GetExpiredUserAccountRequestsForTesting expirytime
