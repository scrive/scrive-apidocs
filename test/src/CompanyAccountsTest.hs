module CompanyAccountsTest (companyAccountsTests) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error (catchError)
import Data.List
import Data.Ord
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (query)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)

import ActionSchedulerState
import Company.Model
import CompanyAccounts.CompanyAccountsControl
import CompanyAccounts.Model
import Context
import DB.Classes
import Doc.DocStateData
import Doc.Model
import FlashMessage
import Mails.Model
import MinutesTime
import Misc
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import Util.HasSomeUserInfo


companyAccountsTests :: DBEnv -> Test
companyAccountsTests env = testGroup "CompanyAccounts" [
    testGroup "Model" [
        testThat "Adding an invite for a new email works" env test_addInviteForNewEmail
      , testThat "Adding an invite for an existing email works" env test_addInviteForExistingEmail
      , testThat "Removing a existing invite works" env test_removingExistingInvite
      , testThat "Removing a non-existant invite works" env test_removingNonExistantInvite
      ]
  , testGroup "Control" [
        testCase "Admin user can add a new user to their company" $ test_addingANewCompanyAccount env
      , testCase "Admin user can invite a private user to their company" $ test_addingExistingPrivateUserAsCompanyAccount env
      , testCase "Admin user can invite a company user to their company" $ test_addingExistingCompanyUserAsCompanyAccount env
      , testCase "Admin user can resend invite to a new user" $ test_resendingInviteToNewCompanyAccount env
      , testCase "Admin user can resend invite to an existing private user" $ test_resendingInviteToPrivateUser env
      , testCase "Admin user can resend invite to an existing company user" $ test_resendingInviteToCompanyUser env
      , testCase "Admin user can switch a standard user to admin" $ test_switchingStandardToAdminUser env
      , testCase "Admin user can switch an admin user to standard" $ test_switchingAdminToStandardUser env
      , testCase "Admin user can remove a company account invite" $ test_removingCompanyAccountInvite env
      , testCase "Admin user can remove a company account user" $ test_removingCompanyAccountWorks env
      , testCase "Existing private user can follow link to be taken over" $ test_privateUserTakoverWorks env
      , testCase "Company takeovers fail if there is no saved invite" $ test_mustBeInvitedForTakeoverToWork env
      ]
  ]

test_addInviteForNewEmail :: DB ()
test_addInviteForNewEmail = do
  company <- addNewCompany
  let firstinvite = mkInvite company "a@a.com" "Anna" "Android"
      secondinvite = mkInvite company "b@b.com" "Bob" "Blue"
  _ <- dbUpdate $ AddCompanyInvite firstinvite
  _ <- dbUpdate $ AddCompanyInvite secondinvite
  assertCompanyInvitesAre company [firstinvite, secondinvite]

test_addInviteForExistingEmail :: DB ()
test_addInviteForExistingEmail = do
  company <- addNewCompany
  let firstinvite = mkInvite company "a@a.com" "Anna" "Android"
      secondinvite = mkInvite company "a@a.com" "Bob" "Blue"
  _ <- dbUpdate $ AddCompanyInvite firstinvite
  _ <- dbUpdate $ AddCompanyInvite secondinvite
  assertCompanyInvitesAre company [secondinvite]

test_removingExistingInvite :: DB ()
test_removingExistingInvite = do
  company <- addNewCompany
  let invite = mkInvite company "a@a.com" "Anna" "Android"
  _ <- dbUpdate $ AddCompanyInvite invite
  _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (invitedemail invite)
  assertCompanyInvitesAre company []

test_removingNonExistantInvite :: DB ()
test_removingNonExistantInvite = do
  company <- addNewCompany
  _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (Email "a@a.com")
  assertCompanyInvitesAre company []

test_addingANewCompanyAccount :: DBEnv -> Assertion
test_addingANewCompanyAccount env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("add", inText "True")
                        , ("email", inText "bob@blue.com")
                        , ("fstname", inText "Bob")
                        , ("sndname", inText "Blue")
                        ]
  (res, ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone

  Just newuser <- dbQuery $ GetUserByEmail Nothing (Email "bob@blue.com")
  assertEqual "New user is in company" (Just $ companyid company) (usercompany newuser)
  assertEqual "New user is standard user" False (useriscompanyadmin newuser)
  assertEqual "New user has the invited name" "Bob Blue" (getFullName newuser)

  assertCompanyInvitesAre company [mkInvite company "bob@blue.com" "Bob" "Blue"]

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetIncomingEmails
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingPrivateUserAsCompanyAccount :: DBEnv -> Assertion
test_addingExistingPrivateUserAsCompanyAccount env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just existinguser <- addNewUser "Bob" "Blue" "bob@blue.com"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("add", inText "True")
                        , ("email", inText "bob@blue.com")
                        , ("fstname", inText "Bob")
                        , ("sndname", inText "Blue")
                        ]
  (res, ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone

  Just updatedexistinguser <- dbQuery $ GetUserByID (userid existinguser)
  assertEqual "Invited user still has no company" (usercompany updatedexistinguser) Nothing

  assertCompanyInvitesAre company [mkInvite company "bob@blue.com" "Bob" "Blue"]

  emails <- dbQuery GetIncomingEmails
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingCompanyUserAsCompanyAccount :: DBEnv -> Assertion
test_addingExistingCompanyUserAsCompanyAccount env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (existinguser, existingcompany) <- addNewAdminUserAndCompany "Bob" "Blue" "bob@blue.com"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("add", inText "True")
                        , ("email", inText "bob@blue.com")
                        , ("fstname", inText "Bob")
                        , ("sndname", inText "Blue")
                        ]
  (res, ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone

  Just updatedexistinguser <- dbQuery $ GetUserByID (userid existinguser)
  assertEqual "Invited user's company stays the same" (usercompany updatedexistinguser)
                                                      (Just $ companyid existingcompany)

  assertCompanyInvitesAre company [mkInvite company "bob@blue.com" "Bob" "Blue"]

  emails <- dbQuery GetIncomingEmails
  assertEqual "An email was sent" 1 (length emails)

test_resendingInviteToNewCompanyAccount :: DBEnv -> Assertion
test_resendingInviteToNewCompanyAccount env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just newuser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("resend", inText "True")
                        , ("resendid", inText $ show (userid newuser))
                        , ("resendemail", inText "bob@blue.com")
                        , ("sndname", inText "Blue")
                        ]
  (res, ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetIncomingEmails
  assertEqual "An email was sent" 1 (length emails)

test_resendingInviteToPrivateUser :: DBEnv -> Assertion
test_resendingInviteToPrivateUser env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just _existinguser <- addNewUser "Bob" "Blue" "bob@blue.com"
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("resend", inText "True")
                        , ("resendid", inText "0")
                        , ("resendemail", inText "bob@blue.com")
                        , ("sndname", inText "Blue")
                        ]
  (res, ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone

  emails <- dbQuery GetIncomingEmails
  assertEqual "An email was sent" 1 (length emails)

test_resendingInviteToCompanyUser :: DBEnv -> Assertion
test_resendingInviteToCompanyUser env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (_existinguser, _existingcompany) <- addNewAdminUserAndCompany "Bob" "Blue" "bob@blue.com"
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("resend", inText "True")
                        , ("resendid", inText "0")
                        , ("resendemail", inText "bob@blue.com")
                        , ("sndname", inText "Blue")
                        ]
  (res, ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone

  emails <- dbQuery GetIncomingEmails
  assertEqual "An email was sent" 1 (length emails)

test_switchingStandardToAdminUser :: DBEnv -> Assertion
test_switchingStandardToAdminUser env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("changerole", inText "True")
                        , ("changeid", inText $ show (userid standarduser))
                        , ("makeadmin", inText $ "true")
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)

  Just updateduser <- dbQuery $ GetUserByID (userid standarduser)
  assertBool "User is now an admin" (useriscompanyadmin updateduser)
  assertEqual "User belongs to the same company" (usercompany updateduser)
                                                 (Just $ companyid company)

test_switchingAdminToStandardUser :: DBEnv -> Assertion
test_switchingAdminToStandardUser env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid standarduser) True
  Just adminuser <- dbQuery $ GetUserByID (userid user)

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("changerole", inText "True")
                        , ("changeid", inText $ show (userid adminuser))
                        , ("makeadmin", inText $ "false")
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)

  Just updateduser <- dbQuery $ GetUserByID (userid adminuser)
  assertBool "User is now standard" (not $ useriscompanyadmin updateduser)
  assertEqual "User belongs to the same company" (usercompany updateduser)
                                                 (Just $ companyid company)

test_removingCompanyAccountInvite :: DBEnv -> Assertion
test_removingCompanyAccountInvite env = withTestEnvironment env $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("remove", inText "True")
                        , ("removeid", inText $ "0")
                        , ("removeemail", inText $ "bob@blue.com")
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)

  assertCompanyInvitesAre company []

test_removingCompanyAccountWorks :: DBEnv -> Assertion
test_removingCompanyAccountWorks env = withTestEnvironment env $ do
  (adminuser, company) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "jony@blue.com" (companyid company)
  doc <- addRandomDocumentWithAuthorAndCondition standarduser (\d -> documentstatus d `elem` [Preparation, Closed])
  let docid = documentid doc

  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "jony@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just adminuser })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("remove", inText "True")
                        , ("removeid", inText $ show (userid standarduser))
                        , ("removeemail", inText $ "jony@blue.com")
                        ]
  (_res, ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts
  assertBool "Flash message is of type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone
  deleteduser <- dbQuery $ GetUserByID (userid standarduser)
  assertEqual "User has been deleted" Nothing deleteduser

  assertCompanyInvitesAre company []

  companydocs <- dbQuery $ GetDocumentsBySignatory $ userid adminuser
  assertEqual "Company still owns users docs" 1 (length companydocs)
  assertEqual "Docid matches" docid (documentid $ head companydocs)

test_privateUserTakoverWorks :: DBEnv -> Assertion
test_privateUserTakoverWorks env = withTestEnvironment env $ do
  (adminuser, company) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  docid <- addRandomDocumentWithAuthor user

  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST []
  (res, ctx') <- runTestKontra req ctx $ handlePostBecomeCompanyAccount (companyid company) >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message is of type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  assertEqual "User belongs to the company" (usercompany updateduser)
                                            (Just $ companyid company)
  assertBool "User is a standard user" (not $ useriscompanyadmin updateduser)
  companydocs <- dbQuery $ GetDocumentsBySignatory $ userid adminuser
  assertEqual "Company owns users docs" 1 (length companydocs)
  assertEqual "Docid matches" docid (documentid $ head companydocs)
  userdocs <- dbQuery $ GetDocumentsBySignatory $ userid user
  assertEqual "User is still linked to their docs" 1 (length userdocs)
  assertEqual "Docid matches" docid (documentid $ head userdocs)

test_mustBeInvitedForTakeoverToWork :: DBEnv -> Assertion
test_mustBeInvitedForTakeoverToWork env = withTestEnvironment env $ do
  company <- addNewCompany
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST []
  (l, _ctx') <- runTestKontra req ctx $
    (handlePostBecomeCompanyAccount (companyid company) >> return False)
      `catchError` const (return True)
  assertEqual "Exception thrown" True l
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  assertEqual "User is still not in company" Nothing (usercompany updateduser)

assertCompanyInvitesAre :: Company -> [CompanyInvite] -> DB ()
assertCompanyInvitesAre company expectedinvites = do
  actualinvites <- dbQuery $ GetCompanyInvites (companyid company)
  assertEqual "Wrong company invites" (inviteSort expectedinvites)
                                      (inviteSort actualinvites)
  mapM_ assertInviteExists expectedinvites
  where
    inviteSort = sortBy (comparing invitedemail)
    assertInviteExists expectedinvite = do
      mactualinvite <- dbQuery $ GetCompanyInvite (companyid company) (invitedemail expectedinvite)
      assertEqual "Wrong company invite" (Just expectedinvite) mactualinvite

addNewAdminUserAndCompany :: String -> String -> String -> DB (User, Company)
addNewAdminUserAndCompany fstname sndname email = do
  company <- addNewCompany
  Just user <- addNewCompanyUser fstname sndname email (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) True
  Just updateduser <- dbQuery $ GetUserByID (userid user)
  return (updateduser, company)

mkInvite :: Company -> String -> String -> String -> CompanyInvite
mkInvite company email fstname sndname =
  CompanyInvite {
      invitedemail = Email email
    , invitedfstname = fstname
    , invitedsndname = sndname
    , invitingcompany = companyid company
  }

getAccountCreatedActions :: MonadIO m => m [Action]
getAccountCreatedActions = do
  now <- getMinutesTime
  let expirytime = (24 * 60 + 1) `minutesAfter` now
  allactions <- query $ GetExpiredActions LeisureAction expirytime
  return $ filter isAccountCreated allactions

isAccountCreated :: Action -> Bool
isAccountCreated action =
  case actionType action of
    (AccountCreated _ _ ) ->  True
    _ -> False
