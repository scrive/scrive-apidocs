module CompanyAccounts.CompanyAccountsControl (
    handleCompanyAccounts
  , handleAddCompanyAccount
  , handleResendToCompanyAccount
  , handleChangeRoleOfCompanyAccount
  , handleRemoveCompanyAccount
  , handleGetBecomeCompanyAccount
  , handlePostBecomeCompanyAccount
  -- this shares some handy stuff with the adminonly section
  , handleCompanyAccountsForAdminOnly
  , sendTakeoverPrivateUserMail
  , sendNewCompanyUserMail
  ) where

import Control.Monad.State
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Text.JSON (JSValue(..))
import Text.JSON.Gen
import Utils.Prelude

import ActionQueue.UserAccountRequest
import DB
import Company.Model
import Company.CompanyUI
import CompanyAccounts.Model
import CompanyAccounts.CompanyAccountsView
import InputValidation
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import Happstack.Fields
import Util.FlashUtil
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import Utils.IO
import User.Action
import User.Utils
import User.UserControl
import User.History.Model
import Payments.Model
import Recurly
import Payments.Config

{- |
    Gets the ajax data for the company accounts list.
-}
handleCompanyAccounts :: Kontrakcja m => m JSValue
handleCompanyAccounts = withCompanyAdmin $ \(_user, company) -> do
  handleCompanyAccountsInternal (companyid company)

{- |
    Gets the ajax data for the company accounts list.
-}
handleCompanyAccountsForAdminOnly :: Kontrakcja m => CompanyID -> m JSValue
handleCompanyAccountsForAdminOnly cid = onlySalesOrAdmin $ do
  handleCompanyAccountsInternal cid

{- |
    This creates the JSON for either the admin only user or the logged in
    company admin.
-}
handleCompanyAccountsInternal :: Kontrakcja m => CompanyID -> m JSValue
handleCompanyAccountsInternal cid = do
  Context{ctxmaybeuser = Just user} <- getContext
  companyusers <- dbQuery $ GetCompanyAccounts cid
  deletableuserids <- map userid <$> filterM isUserDeletable companyusers
  companyinvites <- dbQuery $ GetCompanyInvites cid
  let isUser CompanyInvite{invitedemail} = unEmail invitedemail `elem` map getEmail companyusers
  let
    companyaccounts =
      map mkAccountFromUser companyusers
      ++ map mkAccountFromInvite (filter (not . isUser) companyinvites)
    mkAccountFromUser u = CompanyAccount {
        camaybeuserid = Just $ userid u
      , cafstname = getFirstName u
      , casndname = getLastName u
      , cafullname = getFullName u
      , caemail = getEmail u
      , carole = case (userhasacceptedtermsofservice u, useriscompanyadmin u) of
                  (Nothing, _) -> RolePending
                  (Just _, True) -> RoleAdmin
                  (Just _, False) -> RoleStandard
      , cadeletable = userid u `elem` deletableuserids
      , caactivated = isJust $ userhasacceptedtermsofservice u
      }
    mkAccountFromInvite i = CompanyAccount {
        camaybeuserid = Nothing
      , cafstname = invitedfstname i
      , casndname = invitedsndname i
      , cafullname = invitedfstname i ++ " " ++ invitedsndname i
      , caemail = unEmail $ invitedemail i
      , carole = RolePending
      , cadeletable = True
      , caactivated = False
      }
  params <- getListParams
  let companypage = companyAccountsSortSearchPage params companyaccounts
  runJSONGenT $ do
    objects "list" $ for (take companyAccountsPageSize $ list companypage) $ \f -> do
           value "link" $ show <$> LinkUserAdmin <$> camaybeuserid f            -- Used in admins only
           object "fields" $ do
                value "id" $ show <$> camaybeuserid f
                value "fullname" $ cafullname f
                value "email" $ caemail f
                value "role" $ show $ carole f
                value "deletable" $ cadeletable f
                value "activated" $ caactivated f
                value "isctxuser" $ Just (userid user) == camaybeuserid f
    value "paging" $ pagingParamsJSON companypage

{- |
    A special data type used for just displaying stuff in the list
    this lets make a unified list of users and pending invites
-}
data CompanyAccount = CompanyAccount
  { camaybeuserid :: Maybe UserID -- ^ the account's userid if they have one & not if they're a pending takeover invite
  , cafstname     :: String       -- ^ the account's first name
  , casndname     :: String       -- ^ the account's last name
  , cafullname    :: String       -- ^ the account's fullname
  , caemail       :: String       -- ^ the account's email
  , carole        :: Role         -- ^ the account's role
  , cadeletable   :: Bool         -- ^ can the account be deleted, or do they have pending documents?
  , caactivated   :: Bool         -- ^ is the account a full company user with accepted tos?
  }

data Role = RoleAdmin    -- ^ an admin user
          | RoleStandard -- ^ a standard user
          | RolePending  -- ^ a pending takeover request (didn't want to implement role switching for invites!)
  deriving (Eq, Ord, Show)

-- Searching, sorting and paging
companyAccountsSortSearchPage :: ListParams -> [CompanyAccount] -> PagedList CompanyAccount
companyAccountsSortSearchPage  =
  listSortSearchPage companyAccountsSortFunc companyAccountsSearchFunc companyAccountsPageSize

companyAccountsSearchFunc :: SearchingFunction CompanyAccount
companyAccountsSearchFunc s ca = accountMatch ca s
  where
      match s' m = map toUpper s' `isInfixOf` map toUpper m
      accountMatch ca' s' = match s' (cafstname ca')
                            || match s' (casndname ca')
                            || match s' (cafullname ca')
                            || match s' (caemail ca')

companyAccountsSortFunc :: SortingFunction CompanyAccount
companyAccountsSortFunc "fstname" = viewComparing cafstname
companyAccountsSortFunc "fstnameRev" = viewComparingRev cafstname
companyAccountsSortFunc "sndname" = viewComparing casndname
companyAccountsSortFunc "sndnameRev" = viewComparingRev casndname
companyAccountsSortFunc "fullname" = viewComparing cafullname
companyAccountsSortFunc "fullnameREV" = viewComparingRev cafullname
companyAccountsSortFunc "email" = viewComparing caemail
companyAccountsSortFunc "emailREV" = viewComparingRev caemail
companyAccountsSortFunc "role" = viewComparing carole
companyAccountsSortFunc "roleREV" = viewComparingRev carole
companyAccountsSortFunc "deletable" = viewComparing cadeletable
companyAccountsSortFunc "deletableREV" = viewComparingRev cadeletable
companyAccountsSortFunc "activated" = viewComparing caactivated
companyAccountsSortFunc "activatedREV" = viewComparingRev caactivated
companyAccountsSortFunc "id" = viewComparing camaybeuserid
companyAccountsSortFunc "idREV" = viewComparingRev camaybeuserid
companyAccountsSortFunc _ = const $ const EQ

companyAccountsPageSize :: Int
companyAccountsPageSize = 100

{- |
    Handles adding a company user either by creating them or
    by inviting them to be taken over.
-}
handleAddCompanyAccount :: Kontrakcja m => m JSValue
handleAddCompanyAccount = withCompanyAdmin $ \(user, company) -> do
  ctx <- getContext
  memail <- getOptionalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"
  mexistinguser <- maybe (return Nothing) (dbQuery . GetUserByEmail . Email) memail
  mexistingcompany <- maybe (return Nothing) getCompanyForUser mexistinguser

  minvitee <-
    case (memail, mexistinguser, mexistingcompany) of
      (Just email, Nothing, Nothing) -> do
        --create a new company user
        newuser' <- guardJustM $ createUser (Email email) (fstname, sndname) (Just $ companyid company) (ctxlang ctx)
        _ <- dbUpdate $ SetUserInfo (userid newuser') (userinfo newuser') {
                            userfstname = fstname
                          , usersndname = sndname
                          }
        _ <- dbUpdate $
             LogHistoryUserInfoChanged (userid newuser') (ctxipnumber ctx) (ctxtime ctx)
                                       (userinfo newuser')
                                       ((userinfo newuser') { userfstname = fstname , usersndname = sndname })
                                       (userid <$> ctxmaybeuser ctx)
        newuser <- guardJustM $ dbQuery $ GetUserByID (userid newuser')
        _ <- sendNewCompanyUserMail user company newuser
        return $ Just newuser
      (Just _email, Just existinguser, Nothing) -> do
        --send a takeover invite to the existing user
        _ <- sendTakeoverPrivateUserMail user company existinguser
        return $ Just existinguser
      (Just _email, Just existinguser, Just existingcompany) | existingcompany /= company -> do
        --this is a corner case where someone is trying to takeover someone in another company
        --we send emails to tell people, but we don't send any activation links
        _ <- sendTakeoverCompanyInternalWarningMail user company existinguser
        return $ Nothing
      _ -> return Nothing

  -- record the invite and flash a message
  if (isJust minvitee)
    then do
        email <- guardJust memail
        _ <- dbUpdate $ AddCompanyInvite CompanyInvite {
                invitedemail = Email email
              , invitedfstname = fstname
              , invitedsndname = sndname
              , invitingcompany = companyid company
              }
        runJSONGenT $ value "added" True
    else
        runJSONGenT $ value "added" False



{- |
    Handles a resend by checking for the user and invite
    and resending the invite that they would've received.
-}
handleResendToCompanyAccount :: Kontrakcja m => m JSValue
handleResendToCompanyAccount = withCompanyAdmin $ \(user, company) -> do
  resendid <- getCriticalField asValidUserID "resendid"
  resendemail <- getCriticalField asValidEmail "resendemail"
  muserbyid <- dbQuery $ GetUserByID resendid
  mcompanybyid <- maybe (return Nothing) getCompanyForUser muserbyid
  minvite <- dbQuery $ GetCompanyInvite (companyid company) (Email resendemail)
  muserbyemail <- dbQuery $ GetUserByEmail (Email resendemail)

  resent <-
    case (muserbyid, mcompanybyid, minvite, muserbyemail) of
      (Just userbyid, Just companybyid, Just _invite, Just _userbyemail) |
        company == companybyid
        && isNothing (userhasacceptedtermsofservice userbyid) -> do
          --this is an existing company user who just hasn't activated yet,
          _ <- sendNewCompanyUserMail user company userbyid
          return True
      (Nothing, Nothing, Just _invite, Just userbyemail) |
        isNothing (usercompany userbyemail) -> do
          -- this is a private user who hasn't accepted the takeover yet
          _ <- sendTakeoverPrivateUserMail user company userbyemail
          return True
      (Nothing, Nothing, Just _invite, Just userbyemail) |
        isJust (usercompany userbyemail) -> do
        -- this is a company user
          _ <- sendTakeoverCompanyInternalWarningMail user company userbyemail
          return False
      _ -> return False

  runJSONGenT $ value "resent" resent

sendNewCompanyUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendNewCompanyUserMail inviter company user = do
  ctx <- getContext
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  al <- newUserAccountRequestLink (ctxlang ctx) (userid user) CompanyInvitation
  mail <- mailNewCompanyUserInvite ctx user inviter company companyui al
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = getFullName user, email = getEmail user }]}
  return ()

sendTakeoverPrivateUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendTakeoverPrivateUserMail inviter company user = do
  ctx <- getContext
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  mail <- mailTakeoverPrivateUserInvite ctx user inviter company companyui (LinkCompanyTakeover (companyid company))
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress user] }

sendTakeoverCompanyInternalWarningMail :: Kontrakcja m => User -> Company -> User -> m ()
sendTakeoverCompanyInternalWarningMail inviter company user = do
  ctx <- getContext
  let content = "Oh dear!  " ++ getFullName inviter
                ++ " &lt;" ++ getEmail inviter ++ "&gt;"
                ++ " in company " ++ getCompanyName company
                ++ " has asked to takeover " ++ getFullName user
                ++ " &lt;" ++ getEmail user ++ "&gt;"
                ++ " who is already in a company."
  scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
        to = [MailAddress { fullname = "info@skrivapa.se", email = "info@skrivapa.se" }]
      , title = "Attempted Company Account Takeover"
      , content = content
  }

{- |
    Handles a role change by switching a user from
    admin or to company.
-}
handleChangeRoleOfCompanyAccount :: Kontrakcja m => m JSValue
handleChangeRoleOfCompanyAccount = withCompanyAdmin $ \(_user, company) -> do
  changeid <- getCriticalField asValidUserID "changeid"
  makeadmin <- getField "makeadmin"
  changeuser <- guardJustM $ dbQuery $ GetUserByID changeid
  changecompanyid <- guardJust $ usercompany changeuser
  unless (changecompanyid == companyid company) internalError --make sure user is in same company
  _ <- dbUpdate $ SetUserCompanyAdmin changeid (makeadmin == Just "true")
  runJSONGenT $ value "changed" True

{- |
    Handles deletion of a company user or the deletion of the company invite
    if they haven't yet accepted.
-}
handleRemoveCompanyAccount :: Kontrakcja m => m JSValue
handleRemoveCompanyAccount = withCompanyAdmin $ \(_user, company) -> do
  removeid <- getCriticalField asValidUserID "removeid"
  removeemail <- getCriticalField asValidEmail "removeemail"
  mremoveuser <- dbQuery $ GetUserByID removeid
  mremovecompany <- maybe (return Nothing) getCompanyForUser mremoveuser
  isdeletable <- maybe (return False) isUserDeletable mremoveuser

  case (mremoveuser, mremovecompany) of
    (Just removeuser, Just removecompany) | company == removecompany ->
         --there's an actual user to delete
         if isdeletable
           then do
             _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (Email $ getEmail removeuser)
             _ <- dbUpdate $ DeleteUser (userid removeuser)
             runJSONGenT $ value "removed" True
           else
             runJSONGenT $ value "removed" False
    _ -> do
      _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (Email removeemail)
      runJSONGenT $ value "removed" True

{- |
    This handles the company account takeover links, and replaces
    the old stuff that was based in UserID.  It checks that the logged in
    user has actually been invited to join the company in the URL.
-}
handleGetBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m (Either KontraLink String)
handleGetBecomeCompanyAccount companyid = withUserGet $ do
  _ <- guardGoodForTakeover companyid
  newcompany <- guardJustM $ dbQuery $ GetCompany companyid
  pageDoYouWantToBeCompanyAccount newcompany

handlePostBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m KontraLink
handlePostBecomeCompanyAccount cid = withUserPost $ do
  _ <- guardGoodForTakeover cid
  Context{ctxmaybeuser = Just user,
          ctxrecurlyconfig} <- getContext
  newcompany <- guardJustM $ dbQuery $ GetCompany cid
  _ <- dbUpdate $ SetUserCompany (userid user) (Just $ companyid newcompany)
  -- if we are inviting a user with a plan to join the company, we
  -- should delete their personal plan
  mplan <- dbQuery $ GetPaymentPlan (Left $ userid user)
  case mplan of
    Just pp -> do
      _ <- liftIO $ deleteAccount curl_exe (recurlyAPIKey ctxrecurlyconfig) (show $ ppAccountCode pp)
      _ <- dbUpdate $ DeletePaymentPlan (Left $ userid user)
      return ()
    Nothing -> return ()
  addFlashM $ flashMessageUserHasBecomeCompanyAccount newcompany
  return $ LinkAccount

{- |
    This checks that the logged in user is suitable for being
    taken over by a company.  This means that they are a private user
    and that they were actually invited by the company.
-}
guardGoodForTakeover :: Kontrakcja m => CompanyID -> m ()
guardGoodForTakeover companyid = do
  Context{ctxmaybeuser = Just user} <- getContext
  _ <- unless (isNothing (usercompany user)) internalError
  _ <- guardJustM $ dbQuery $ GetCompanyInvite companyid (Email $ getEmail user)
  return ()
