module CompanyAccountsTest (companyAccountsTests) where

import Control.Applicative
import Control.Monad.State
import Database.HDBC.PostgreSQL
import Data.List
import Data.Ord
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (query)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.UTF8 as BS

import ActionSchedulerState
import Company.Model
import CompanyAccounts.CompanyAccountsControl
import CompanyAccounts.Model
import Context
import DB.Classes
import Doc.DocStateData
import Doc.Transitory
import FlashMessage
import Mails.Public
import MinutesTime
import Misc
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import Util.HasSomeUserInfo


companyAccountsTests :: Connection -> Test
companyAccountsTests conn = testGroup "CompanyAccounts" [
    testGroup "Model" [
        testThat "Adding an invite for a new email works" conn test_addInviteForNewEmail
      , testThat "Adding an invite for an existing email works" conn test_addInviteForExistingEmail
      , testThat "Removing a existing invite works" conn test_removingExistingInvite
      , testThat "Removing a non-existant invite works" conn test_removingNonExistantInvite
      ]
  , testGroup "Control" [
        testCase "Admin user can add a new user to their company" $ test_addingANewCompanyAccount conn
      , testCase "Admin user can invite a private user to their company" $ test_addingExistingPrivateUserAsCompanyAccount conn
      , testCase "Admin user can invite a company user to their company" $ test_addingExistingCompanyUserAsCompanyAccount conn
      , testCase "Admin user can resend invite to a new user" $ test_resendingInviteToNewCompanyAccount conn
      , testCase "Admin user can resend invite to an existing private user" $ test_resendingInviteToPrivateUser conn
      , testCase "Admin user can resend invite to an existing company user" $ test_resendingInviteToCompanyUser conn
      , testCase "Admin user can switch a standard user to admin" $ test_switchingStandardToAdminUser conn
      , testCase "Admin user can switch an admin user to standard" $ test_switchingAdminToStandardUser conn
      , testCase "Admin user can remove a company account invite" $ test_removingCompanyAccountInvite conn
      , testCase "Admin user can remove a company account user" $ test_removingCompanyAccountWorks conn
      , testCase "Existing private user can follow link to be taken over" $ test_privateUserTakoverWorks conn
      , testCase "Company takeovers fail if there is no saved invite" $ test_mustBeInvitedForTakeoverToWork conn
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
  _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (Email $ BS.fromString "a@a.com")
  assertCompanyInvitesAre company []

test_addingANewCompanyAccount :: Connection -> Assertion
test_addingANewCompanyAccount conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

  Just newuser <- dbQuery $ GetUserByEmail Nothing (Email $ BS.fromString "bob@blue.com")
  assertEqual "New user is in company" (Just $ companyid company) (usercompany newuser)
  assertEqual "New user is standard user" False (useriscompanyadmin newuser)
  assertEqual "New user has the invited name" (BS.fromString "Bob Blue") (getFullName newuser)

  assertCompanyInvitesAre company [mkInvite company "bob@blue.com" "Bob" "Blue"]

  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)

  emails <- dbQuery GetIncomingEmails
  assertEqual "An email was sent" 1 (length emails)

test_addingExistingPrivateUserAsCompanyAccount :: Connection -> Assertion
test_addingExistingPrivateUserAsCompanyAccount conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just existinguser <- addNewUser "Bob" "Blue" "bob@blue.com"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

test_addingExistingCompanyUserAsCompanyAccount :: Connection -> Assertion
test_addingExistingCompanyUserAsCompanyAccount conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (existinguser, existingcompany) <- addNewAdminUserAndCompany "Bob" "Blue" "bob@blue.com"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

test_resendingInviteToNewCompanyAccount :: Connection -> Assertion
test_resendingInviteToNewCompanyAccount conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just newuser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

test_resendingInviteToPrivateUser :: Connection -> Assertion
test_resendingInviteToPrivateUser conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just _existinguser <- addNewUser "Bob" "Blue" "bob@blue.com"
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

test_resendingInviteToCompanyUser :: Connection -> Assertion
test_resendingInviteToCompanyUser conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  (_existinguser, _existingcompany) <- addNewAdminUserAndCompany "Bob" "Blue" "bob@blue.com"
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

test_switchingStandardToAdminUser :: Connection -> Assertion
test_switchingStandardToAdminUser conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

test_switchingAdminToStandardUser :: Connection -> Assertion
test_switchingAdminToStandardUser conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)
  _ <- dbUpdate $ SetUserCompanyAdmin (userid standarduser) True
  Just adminuser <- dbQuery $ GetUserByID (userid user)

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

test_removingCompanyAccountInvite :: Connection -> Assertion
test_removingCompanyAccountInvite conn = withTestEnvironment conn $ do
  (user, company) <- addNewAdminUserAndCompany "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("remove", inText "True")
                        , ("removeid", inText $ "0")
                        , ("removeemail", inText $ "bob@blue.com")
                        ]
  (res, _ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)

  assertCompanyInvitesAre company []

test_removingCompanyAccountWorks :: Connection -> Assertion
test_removingCompanyAccountWorks conn = withTestEnvironment conn $ do
  (adminuser, company) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  Just standarduser <- addNewCompanyUser "Bob" "Blue" "bob@blue.com" (companyid company)
  docid <- addRandomDocumentWithAuthor standarduser

  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just adminuser })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("remove", inText "True")
                        , ("removeid", inText $ show (userid standarduser))
                        , ("removeemail", inText $ "bob@blue.com")
                        ]
  (res, ctx') <- runTestKontra req ctx $ handlePostCompanyAccounts >>= sendRedirect

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message is of type indicating success" $ head (ctxflashmessages ctx') `isFlashOfType` OperationDone
  deleteduser <- dbQuery $ GetUserByID (userid standarduser)
  assertEqual "User has been deleted" Nothing deleteduser

  assertCompanyInvitesAre company []

  companydocs <- doc_query' $ GetDocumentsByCompany adminuser
  assertEqual "Company still owns users docs" 1 (length companydocs)
  assertEqual "Docid matches" docid (documentid $ head companydocs)

test_privateUserTakoverWorks :: Connection -> Assertion
test_privateUserTakoverWorks conn = withTestEnvironment conn $ do
  (adminuser, company) <- addNewAdminUserAndCompany "Anna" "Android" "anna@android.com"
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  docid <- addRandomDocumentWithAuthor user

  _ <- dbUpdate $ AddCompanyInvite $ mkInvite company "bob@blue.com" "Bob" "Blue"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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
  companydocs <- doc_query' $ GetDocumentsByCompany adminuser
  assertEqual "Company owns users docs" 1 (length companydocs)
  assertEqual "Docid matches" docid (documentid $ head companydocs)
  userdocs <- doc_query' $ GetDocumentsBySignatory user
  assertEqual "User is still linked to their docs" 1 (length userdocs)
  assertEqual "Docid matches" docid (documentid $ head userdocs)

test_mustBeInvitedForTakeoverToWork :: Connection -> Assertion
test_mustBeInvitedForTakeoverToWork conn = withTestEnvironment conn $ do
  company <- addNewCompany
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"

  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST []
  (res, _ctx') <- runTestKontra req ctx $ handlePostBecomeCompanyAccount (companyid company) >>= sendRedirect

  assertEqual "Response code is 500" 500 (rsCode res)
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
      invitedemail = Email $ BS.fromString email
    , invitedfstname = BS.fromString fstname
    , invitedsndname = BS.fromString sndname
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
