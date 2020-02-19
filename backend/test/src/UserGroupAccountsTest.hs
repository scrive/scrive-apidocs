module UserGroupAccountsTest ( companyAccountsTests ) where

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
  ug    <- instantiateRandomUserGroup
  user1 <- instantiateUser $ randomUserTemplate { firstName      = return "a@a.com"
                                                , lastName       = return "Anna"
                                                , email          = return "Android"
                                                , isCompanyAdmin = True
                                                , signupMethod   = CompanyInvitation
                                                }
  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug user1
  assertCompanyInvitesAre ug [mkInvite ug user1]

test_removingExistingInvite :: TestEnv ()
test_removingExistingInvite = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "a@a.com"
                                               , lastName       = return "Anna"
                                               , email          = return "Android"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }

  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug user
  void $ dbUpdate $ RemoveUserGroupInvite [ug ^. #id] (user ^. #id)
  assertCompanyInvitesAre ug []

test_removingNonExistantInvite :: TestEnv ()
test_removingNonExistantInvite = do
  ug <- instantiateRandomUserGroup
  void $ dbUpdate $ RemoveUserGroupInvite [ug ^. #id] (unsafeUserID 0)
  assertCompanyInvitesAre ug []

test_addingANewCompanyAccount :: TestEnv ()
test_addingANewCompanyAccount = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }

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


  Just newuser <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  assertEqual "New user is in company"        (ug ^. #id) (newuser ^. #groupID)
  assertEqual "New user is standard user"     False       (newuser ^. #isCompanyAdmin)
  assertEqual "New user has the invited name" "Bob Blue"  (getFullName newuser)

  assertCompanyInvitesAre ug [] -- We don't add an invite if we created a user

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingCompanyUserAsCompanyAccount :: TestEnv ()
test_addingExistingCompanyUserAsCompanyAccount = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  existinguser <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                                       , lastName = return "Blue"
                                                       , email = return "bob@blue.com"
                                                       , isCompanyAdmin = True
                                                       , signupMethod = CompanyInvitation
                                                       }
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

  Just updatedexistinguser <- dbQuery $ GetUserByID (existinguser ^. #id)
  assertEqual "Invited user's company stays the same"
              (updatedexistinguser ^. #groupID)
              (existinguser ^. #groupID)

  assertCompanyInvitesAre ug [mkInvite ug existinguser]

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_addingANewCompanyAccountWithDifferentTarget :: TestEnv ()
test_addingANewCompanyAccountWithDifferentTarget = do
  let email = "andrzej@skrivapa.se"
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email          = return email
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  trgug <- instantiateRandomUserGroup
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
  void . dbUpdate . AccessControlCreateForUser (user ^. #id) $ UserGroupAdminAR trgugid
  (res, _) <- runTestKontra req ctx handleAddUserGroupAccount
  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)
  Just newuser <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  assertEqual "New user is in target user group" trgugid    (newuser ^. #groupID)
  assertEqual "New user is standard user"        False      (newuser ^. #isCompanyAdmin)
  assertEqual "New user has the invited name"    "Bob Blue" (getFullName newuser)
  assertCompanyInvitesAre trgug []

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingCompanyUserAsCompanyAccountWithDifferentTarget :: TestEnv ()
test_addingExistingCompanyUserAsCompanyAccountWithDifferentTarget = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  existinguser <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                                       , lastName = return "Blue"
                                                       , email = return "bob@blue.com"
                                                       , isCompanyAdmin = True
                                                       , signupMethod = CompanyInvitation
                                                       }
  trgug <- instantiateRandomUserGroup
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
  void . dbUpdate . AccessControlCreateForUser (user ^. #id) $ UserGroupAdminAR trgugid
  (res, _) <- runTestKontra req ctx $ handleAddUserGroupAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)

  Just updatedexistinguser <- dbQuery $ GetUserByID (existinguser ^. #id)
  assertEqual "Invited user's company stays the same"
              (updatedexistinguser ^. #groupID)
              (existinguser ^. #groupID)

  assertCompanyInvitesAre trgug [mkInvite trgug existinguser]

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_resendingInviteToNewCompanyAccount :: TestEnv ()
test_resendingInviteToNewCompanyAccount = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  newuser <- instantiateUser $ randomUserTemplate { groupID = return $ ug ^. #id }
  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug newuser

  ctx <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req <- mkRequest
    POST
    [ ("resend"     , inText "True")
    , ("resendid"   , inText $ showt (newuser ^. #id))
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
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  standarduser <- instantiateUser
    $ randomUserTemplate { groupID = return $ user ^. #groupID }
  ctx <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  req <- mkRequest
    POST
    [ ("changerole", inText "True")
    , ("changeid", inText $ showt (standarduser ^. #id))
    , ("makeadmin" , inText $ "true")
    ]
  (res, _ctx') <- runTestKontra req ctx $ handleChangeRoleOfUserGroupAccount

  assertBool "Response is proper JSON" $ res == (runJSONGen $ value "changed" True)

  Just updateduser <- dbQuery $ GetUserByID (standarduser ^. #id)
  assertBool "User is now an admin" (updateduser ^. #isCompanyAdmin)
  assertEqual "User belongs to the same company"
              (updateduser ^. #groupID)
              (user ^. #groupID)

test_switchingAdminToStandardUser :: TestEnv ()
test_switchingAdminToStandardUser = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  standarduser <- instantiateUser
    $ randomUserTemplate { groupID = return $ user ^. #groupID }
  void $ dbUpdate $ SetUserCompanyAdmin (standarduser ^. #id) True
  Just adminuser <- dbQuery $ GetUserByID (user ^. #id)

  ctx            <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req            <- mkRequest
    POST
    [ ("changerole", inText "True")
    , ("changeid"  , inText $ showt (adminuser ^. #id))
    , ("makeadmin" , inText $ "false")
    ]
  (res, _ctx') <- runTestKontra req ctx $ handleChangeRoleOfUserGroupAccount

  assertBool "Response is proper JSON" $ res == (runJSONGen $ value "changed" True)

  Just updateduser <- dbQuery $ GetUserByID (adminuser ^. #id)
  assertBool "User is now standard" (not $ updateduser ^. #isCompanyAdmin)
  assertEqual "User belongs to the same company"
              (updateduser ^. #groupID)
              (user ^. #groupID)

test_removingCompanyAccountInvite :: TestEnv ()
test_removingCompanyAccountInvite = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  standarduser <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                                       , lastName = return "Blue"
                                                       , email = return "jony@blue.com"
                                                       , isCompanyAdmin = True
                                                       , signupMethod = CompanyInvitation
                                                       }

  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug standarduser

  ctx          <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req          <- mkRequest POST [("removeid", inText $ showt $ standarduser ^. #id)]
  (res, _ctx') <- runTestKontra req ctx $ handleRemoveUserGroupAccount

  assertBool "Response is proper JSON" $ res == (runJSONGen $ value "removed" True)

  assertCompanyInvitesAre ug []

test_removingCompanyAccountWorks :: TestEnv ()
test_removingCompanyAccountWorks = do
  ug        <- instantiateRandomUserGroup
  adminuser <- instantiateUser $ randomUserTemplate { firstName      = return "Anna"
                                                    , lastName       = return "Android"
                                                    , email = return "anna@android.com"
                                                    , groupID        = return $ ug ^. #id
                                                    , isCompanyAdmin = True
                                                    , signupMethod   = CompanyInvitation
                                                    }
  standarduser <- instantiateUser $ randomUserTemplate { groupID = return $ ug ^. #id }
  doc <- addRandomDocument (rdaDefault standarduser) { rdaTypes    = OneOf [Signable]
                                                     , rdaStatuses = OneOf [Closed]
                                                     }
  let docid = documentid doc

  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug standarduser

  ctx          <- (set #maybeUser (Just adminuser)) <$> mkContext defaultLang

  companydocs1 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ adminuser ^. #id)
                                         [DocumentFilterUnsavedDraft False]
                                         []
                                         maxBound
  assertEqual "Company admin sees users docs before user delete" 1 (length companydocs1)
  assertEqual "Docid matches before user delete" docid (documentid $ head companydocs1)

  req      <- mkRequest POST [("removeid", inText $ showt (standarduser ^. #id))]
  (res, _) <- runTestKontra req ctx $ handleRemoveUserGroupAccount

  assertBool "Response is proper JSON" $ res == (runJSONGen $ value "removed" True)

  deleteduser <- dbQuery $ GetUserByID (standarduser ^. #id)
  assertEqual "User has been deleted" Nothing deleteduser

  assertCompanyInvitesAre ug []

  companydocs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ adminuser ^. #id)
                                        [DocumentFilterUnsavedDraft False]
                                        []
                                        maxBound
  assertEqual "Company admin sees users docs after user delete" 1 (length companydocs)
  assertEqual "Docid matches after user delete" docid (documentid $ head companydocs)

  -- test removal with access control only
  otherug   <- instantiateRandomUserGroup
  otheruser <- instantiateUser $ randomUserTemplate { firstName = return "Mad Jack"
                                                    , lastName = return "Churchill"
                                                    , email = return "madjack@example.com"
                                                    , groupID = return $ otherug ^. #id
                                                    , isCompanyAdmin = True
                                                    , signupMethod = CompanyInvitation
                                                    }
  req'      <- mkRequest POST [("removeid", inText $ showt (otheruser ^. #id))]

  -- shouldn't work
  (res', _) <- runTestKontra req' ctx $ handleRemoveUserGroupAccount
  -- the `True` value here is a bit confusing in this case
  assertBool "Response is proper JSON" $ res' == (runJSONGen $ value "removed" True)
  motheruserDB <- dbQuery $ GetUserByID (otheruser ^. #id)
  assertEqual "User has NOT been deleted" (Just otheruser) motheruserDB

  -- allow adminuser control over otheruser's group
  void $ dbUpdate $ AccessControlCreateForUserGroup (ug ^. #id)
                                                    (UserAdminAR $ otherug ^. #id)

  -- now removal should work
  (res'', _) <- runTestKontra req' ctx $ handleRemoveUserGroupAccount
  assertBool "Response is proper JSON" $ res'' == (runJSONGen $ value "removed" True)
  otheruserDB' <- dbQuery $ GetUserByID (otheruser ^. #id)
  assertEqual "User has been deleted" Nothing otheruserDB'

test_privateUserTakoverWorks :: TestEnv ()
test_privateUserTakoverWorks = do
  ug        <- instantiateRandomUserGroup
  adminuser <- instantiateUser $ randomUserTemplate { firstName      = return "Anna"
                                                    , lastName       = return "Android"
                                                    , email = return "anna@android.com"
                                                    , groupID        = return $ ug ^. #id
                                                    , isCompanyAdmin = True
                                                    , signupMethod   = CompanyInvitation
                                                    }
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  docid <- documentid <$> addRandomDocument (rdaDefault user)
    { rdaTypes    = OneOf [Signable]
    , rdaStatuses = OneOf $ documentAllStatuses \\ [Preparation]
    }
  void $ dbUpdate $ AddUserGroupInvite $ mkInvite ug user

  ctx      <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req      <- mkRequest POST []
  (res, _) <- runTestKontra req ctx $ handlePostBecomeUserGroupAccount (ug ^. #id)
  assertBool "Response is redirect"                (isRedirect LinkAccount res)
  assertBool "Response has flash message redirect" (hasFlashMessage res)
  Just updateduser <- dbQuery $ GetUserByID (user ^. #id)
  assertEqual "User belongs to the company" (updateduser ^. #groupID) (ug ^. #id)
  assertBool "User is a standard user" (not $ updateduser ^. #isCompanyAdmin)

  companydocs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ adminuser ^. #id)
                                        [DocumentFilterUnsavedDraft False]
                                        []
                                        maxBound
  assertEqual "Company owns users docs" 1     (length companydocs)
  assertEqual "Docid matches"           docid (documentid $ head companydocs)
  docs <- dbQuery $ GetDocuments
    (DocumentsVisibleToUser $ user ^. #id)
    [DocumentFilterUnsavedDraft False, DocumentFilterSignable]
    []
    maxBound
  templates <- dbQuery $ GetTemplatesByAuthor $ user ^. #id
  let userdocids = nub (documentid <$> docs ++ templates)
  assertEqual "User is still linked to their docs" 1     (length userdocids)
  assertEqual "Docid matches" docid (head userdocids)

test_mustBeInvitedForTakeoverToWork :: TestEnv ()
test_mustBeInvitedForTakeoverToWork = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }

  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  req        <- mkRequest POST []
  (l, _ctx') <-
    runTestKontra req ctx
    $         (handlePostBecomeUserGroupAccount (ug ^. #id) >> return False)
    `E.catch` (\(_ :: KontraError) -> return True)
  assertEqual "Exception thrown" True l
  Just updateduser <- dbQuery $ GetUserByID (user ^. #id)
  assertBool "User is still not in the other company"
             ((user ^. #groupID) == (updateduser ^. #groupID))

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
  UserGroupInvite { inviteduserid = user ^. #id, invitingusergroup = ug ^. #id }

getAccountCreatedActions :: TestEnv [UserAccountRequest]
getAccountCreatedActions = do
  expirytime <- (30 `daysAfter`) <$> currentTime
  dbQuery $ GetExpiredUserAccountRequestsForTesting expirytime
