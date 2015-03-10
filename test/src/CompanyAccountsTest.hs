module CompanyAccountsTest (companyAccountsTests) where

import Control.Applicative
import Data.List
import Data.Ord
import Happstack.Server hiding (simpleHTTP)
import Test.Framework
import Text.JSON.Gen
import qualified Control.Exception.Lifted as E

import ActionQueue.Core
import ActionQueue.UserAccountRequest
import Company.Model
import CompanyAccounts.CompanyAccountsControl
import CompanyAccounts.Model
import Context
import DB hiding (query, update)
import Doc.DocStateData
import Doc.Model
import FlashMessage
import KontraError
import Mails.Model
import MinutesTime
import Redirect
import TestingUtil
import TestKontra as T
import User.Email
import Util.HasSomeUserInfo
import Utils.Default

companyAccountsTests :: TestEnvSt -> Test
companyAccountsTests env = testGroup "CompanyAccounts" [
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
  company <- addNewCompany
  (user1, _) <- addNewAdminUserAndCompany "a@a.com" "Anna" "Android"
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company user1
  assertCompanyInvitesAre company [mkInvite company user1]

test_removingExistingInvite :: TestEnv ()
test_removingExistingInvite = do
  company <- addNewCompany
  (user, _) <- addNewAdminUserAndCompany "a@a.com" "Anna" "Android"

  _ <- dbUpdate $ AddCompanyInvite $  mkInvite company user
  _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (userid user)
  assertCompanyInvitesAre company []

test_removingNonExistantInvite :: TestEnv ()
test_removingNonExistantInvite = do
  company <- addNewCompany
  _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (unsafeUserID 0)
  assertCompanyInvitesAre company []

test_addingANewCompanyAccount :: TestEnv ()
test_addingANewCompanyAccount = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [ ("add", inText "True")
                        , ("email", inText "bob@blue.com")
                        , ("fstname", inText "Bob")
                        , ("sndname", inText "Blue")
                        ]
  (res, _) <- runTestKontra req ctx $ handleAddCompanyAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)


  Just newuser <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  assertEqual "New user is in company" (companyid company) (usercompany newuser)
  assertEqual "New user is standard user" False (useriscompanyadmin newuser)
  assertEqual "New user has the invited name" "Bob Blue" (getFullName newuser)

  assertCompanyInvitesAre company [] -- We don't add an invite if we created a user

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingCompanyUserAsCompanyAccount :: TestEnv ()
test_addingExistingCompanyUserAsCompanyAccount = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (existinguser, existingcompany) <- addNewAdminUserAndCompany "Bob" "Blue" "bob@blue.com"

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [ ("add", inText "True")
                        , ("email", inText "bob@blue.com")
                        , ("fstname", inText "Bob")
                        , ("sndname", inText "Blue")
                        ]
  (res, _) <- runTestKontra req ctx $ handleAddCompanyAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "added" True)

  Just updatedexistinguser <- dbQuery $ GetUserByID (userid existinguser)
  assertEqual "Invited user's company stays the same" (usercompany updatedexistinguser)
                                                      (companyid existingcompany)

  assertCompanyInvitesAre company [mkInvite company existinguser]

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_resendingInviteToNewCompanyAccount :: TestEnv ()
test_resendingInviteToNewCompanyAccount = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just newuser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company newuser

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [ ("resend", inText "True")
                        , ("resendid", inText $ show (userid newuser))
                        , ("resendemail", inText "bob@blue.com")
                        , ("sndname", inText "Blue")
                        ]
  (res, _) <- runTestKontra req ctx $ handleResendToCompanyAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "resent" True)


  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent" 1 (length emails)

test_switchingStandardToAdminUser :: TestEnv ()
test_switchingStandardToAdminUser = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [ ("changerole", inText "True")
                        , ("changeid", inText $ show (userid standarduser))
                        , ("makeadmin", inText $ "true")
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handleChangeRoleOfCompanyAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "changed" True)

  Just updateduser <- dbQuery $ GetUserByID (userid standarduser)
  assertBool "User is now an admin" (useriscompanyadmin updateduser)
  assertEqual "User belongs to the same company" (usercompany updateduser)
                                                 (companyid company)

test_switchingAdminToStandardUser :: TestEnv ()
test_switchingAdminToStandardUser = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid standarduser) True
  Just adminuser <- dbQuery $ GetUserByID (userid user)

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [ ("changerole", inText "True")
                        , ("changeid", inText $ show (userid adminuser))
                        , ("makeadmin", inText $ "false")
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handleChangeRoleOfCompanyAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "changed" True)

  Just updateduser <- dbQuery $ GetUserByID (userid adminuser)
  assertBool "User is now standard" (not $ useriscompanyadmin updateduser)
  assertEqual "User belongs to the same company" (usercompany updateduser)
                                                 (companyid company)

test_removingCompanyAccountInvite :: TestEnv ()
test_removingCompanyAccountInvite = do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (standarduser,_) <- addNewAdminUserAndCompany "Bob" "Blue" "jony@blue.com"

  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company standarduser

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [ ("remove", inText "True")
                        , ("removeid", inText $ show $ userid standarduser)
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handleRemoveCompanyAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "removed" True)

  assertCompanyInvitesAre company []

test_removingCompanyAccountWorks :: TestEnv ()
test_removingCompanyAccountWorks = do
  (adminuser, company) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "jony@blue.com" (companyid company)
  doc <- addRandomDocumentWithAuthorAndCondition standarduser (\d -> documentstatus d `elem` [Closed])
  let docid = documentid doc

  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company standarduser

  ctx <- (\c -> c { ctxmaybeuser = Just adminuser })
    <$> mkContext defaultValue

  companydocs1 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid adminuser)] [DocumentFilterUnsavedDraft False] [] (0,maxBound)
  assertEqual "Company admin sees users docs before user delete" 1 (length companydocs1)
  assertEqual "Docid matches before user delete" docid (documentid $ head companydocs1)

  req <- mkRequest POST [ ("remove", inText "True")
                        , ("removeid", inText $ show (userid standarduser))
                        , ("removeemail", inText $ "jony@blue.com")
                        ]
  (res, _) <- runTestKontra req ctx $ handleRemoveCompanyAccount

  assertBool "Response is propper JSON" $ res == (runJSONGen $ value "removed" True)

  deleteduser <- dbQuery $ GetUserByID (userid standarduser)
  assertEqual "User has been deleted" Nothing deleteduser

  assertCompanyInvitesAre company []

  companydocs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid adminuser)] [DocumentFilterUnsavedDraft False] [] (0,maxBound)
  assertEqual "Company admin sees users docs after user delete" 1 (length companydocs)
  assertEqual "Docid matches after user delete" docid (documentid $ head companydocs)

test_privateUserTakoverWorks :: TestEnv ()
test_privateUserTakoverWorks = do
  (adminuser, company) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  docid <- documentid <$> addRandomDocumentWithAuthorAndCondition user (\d -> not $ (documentstatus d) `elem` [Preparation])


  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company user

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST []
  (res, ctx') <- runTestKontra req ctx $ handlePostBecomeCompanyAccount (companyid company) >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message is of type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  assertEqual "User belongs to the company" (usercompany updateduser)
                                            (companyid company)
  assertBool "User is a standard user" (not $ useriscompanyadmin updateduser)

  companydocs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid adminuser)] [DocumentFilterUnsavedDraft False] [] (0,maxBound)
  assertEqual "Company owns users docs" 1 (length companydocs)
  assertEqual "Docid matches" docid (documentid $ head companydocs)
  docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid user)] [DocumentFilterUnsavedDraft False, DocumentFilterSignable] [] (0,maxBound)
  templates <- dbQuery $ GetTemplatesByAuthor $ userid user
  let userdocs = nub (docs ++ templates)
  assertEqual "User is still linked to their docs" 1 (length userdocs)
  assertEqual "Docid matches" docid (documentid $ head userdocs)

test_mustBeInvitedForTakeoverToWork :: TestEnv ()
test_mustBeInvitedForTakeoverToWork = do
  company <- addNewCompany
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST []
  (l, _ctx') <- runTestKontra req ctx $
    (handlePostBecomeCompanyAccount (companyid company) >> return False)
      `E.catch` (\(_::KontraError) -> return True)
  assertEqual "Exception thrown" True l
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  assertBool "User is still not in the other company" ((usercompany user) == (usercompany updateduser))

assertCompanyInvitesAre :: Company -> [CompanyInvite] -> TestEnv ()
assertCompanyInvitesAre company expectedinvites = do
  actualinvites <- dbQuery $ GetCompanyInvites (companyid company)
  assertEqual "Wrong company invites" (inviteSort expectedinvites)
                                      (inviteSort actualinvites)
  mapM_ assertInviteExists expectedinvites
  where
    inviteSort = sortBy (comparing inviteduserid)
    assertInviteExists expectedinvite = do
      mactualinvite <- dbQuery $ GetCompanyInvite (companyid company) (inviteduserid expectedinvite)
      assertEqual "Wrong company invite" (Just expectedinvite) mactualinvite

addNewAdminUserAndCompany :: String -> String -> String -> TestEnv (User, Company)
addNewAdminUserAndCompany fstname sndname email = do
  company <- addNewCompany
  Just user <- addNewCompanyUser fstname sndname email (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  return (updateduser, company)

mkInvite :: Company -> User -> CompanyInvite
mkInvite company user =
  CompanyInvite {
      inviteduserid= userid user
    , invitingcompany = companyid company
  }

getAccountCreatedActions :: TestEnv [UserAccountRequest]
getAccountCreatedActions = do
  expirytime <- (30 `daysAfter`) <$> currentTime
  dbQuery $ GetExpiredActions userAccountRequest expirytime
