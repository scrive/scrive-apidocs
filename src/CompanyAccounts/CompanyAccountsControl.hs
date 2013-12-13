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
  , sendTakeoverSingleUserMail
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
import Util.HasSomeUserInfo
import Util.MonadUtils
import User.Action
import User.Utils
import User.UserControl
import User.History.Model
import Payments.Model
import MinutesTime
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
  companyinvites <- dbQuery $ GetCompanyInvitesWithUsersData cid
  let isUser (civ,_,_,_) = (inviteduserid civ) `elem` map userid companyusers
  let
    companyaccounts =
      map mkAccountFromUser companyusers
      ++ map mkAccountFromInvite (filter (not . isUser) companyinvites)
    mkAccountFromUser u = CompanyAccount {
        camaybeuserid = userid u
      , cafstname = getFirstName u
      , casndname = getLastName u
      , cafullname = getFullName u
      , caemail = getEmail u
      , carole = if (useriscompanyadmin u)
                  then RoleAdmin
                  else RoleStandard
      , cadeletable = userid u `elem` deletableuserids
      , caactivated = isJust $ userhasacceptedtermsofservice u
      , catos = userhasacceptedtermsofservice u
      }
    mkAccountFromInvite (i,fn,ln,em) = CompanyAccount {
        camaybeuserid = inviteduserid i
      , cafstname = fn
      , casndname = ln
      , cafullname = fn ++ " " ++ ln
      , caemail = em
      , carole = RoleInvite
      , cadeletable = True
      , caactivated = False
      , catos = Nothing
      }
  params <- getListParams
  let companypage = companyAccountsSortSearchPage params companyaccounts
  runJSONGenT $ do
    objects "list" $ for (take companyAccountsPageSize $ list companypage) $ \f -> do
           value "link" $ show $ LinkUserAdmin $ camaybeuserid f            -- Used in admins only
           object "fields" $ do
                value "id" $ show $ camaybeuserid f
                value "fullname" $ cafullname f
                value "email" $ caemail f
                value "role" $ show $ carole f
                value "deletable" $ cadeletable f
                value "activated" $ caactivated f
                value "isctxuser" $ userid user == camaybeuserid f
                value "tos"       $ formatMinutesTimeRealISO <$> (catos f)

    value "paging" $ pagingParamsJSON companypage

{- |
    A special data type used for just displaying stuff in the list
    this lets make a unified list of users and pending invites
-}
data CompanyAccount = CompanyAccount
  { camaybeuserid :: UserID       -- ^ the account's or invites userid
  , cafstname     :: String       -- ^ the account's or invitesfirst name
  , casndname     :: String       -- ^ the account's or inviteslast name
  , cafullname    :: String       -- ^ the account's or invites fullname
  , caemail       :: String       -- ^ the account's or invites email
  , carole        :: Role         -- ^ the account's role (always Standard for invites)
  , cadeletable   :: Bool         -- ^ can the account be deleted, or do they have pending documents (always True for invites)?
  , caactivated   :: Bool         -- ^ is the account a full company user with accepted tos? (always False for invites)
  , catos         :: Maybe MinutesTime -- ^ TOS time if any (always Nothing for invites)
  }

data Role = RoleAdmin    -- ^ an admin user
          | RoleStandard -- ^ a standard user
          | RoleInvite   -- ^ an invite of a user that is in different company
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
  email <-  guardJustM $ getOptionalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"
  mexistinguser <- dbQuery $ GetUserByEmail $ Email email
  case (mexistinguser) of
      (Nothing) -> do
        --create a new company user
        newuser' <- guardJustM $ createUser (Email email) (fstname, sndname) (companyid company,False) (ctxlang ctx)
        _ <- dbUpdate $
             LogHistoryUserInfoChanged (userid newuser') (ctxipnumber ctx) (ctxtime ctx)
                                       (userinfo newuser')
                                       ((userinfo newuser') { userfstname = fstname , usersndname = sndname })
                                       (userid <$> ctxmaybeuser ctx)
        newuser <- guardJustM $ dbQuery $ GetUserByID (userid newuser')
        _ <- sendNewCompanyUserMail user company newuser
        runJSONGenT $ value "added" True
      (Just existinguser) ->
        if (usercompany existinguser == companyid company)
           then runJSONGenT $ value "added" False >> value "samecompany" True
           else do
            -- If user exists we allow takeover only if he is the only user in his company
            users <- dbQuery $ GetCompanyAccounts $ usercompany existinguser
            mpaymentplan <- dbQuery $ GetPaymentPlan $ usercompany existinguser
            case (users,fromMaybe NoProvider (ppPaymentPlanProvider <$> mpaymentplan)) of
              ([_],NoProvider) -> do
                        _ <- sendTakeoverSingleUserMail user company existinguser
                        _ <- dbUpdate $ AddCompanyInvite $ CompanyInvite  (userid existinguser) (companyid company)
                        runJSONGenT $ value "added" True
              _ -> runJSONGenT $ value "added" False

{- |
    Handles a resend by checking for the user and invite
    and resending the invite that they would've received.
-}
handleResendToCompanyAccount :: Kontrakcja m => m JSValue
handleResendToCompanyAccount = withCompanyAdmin $ \(user, company) -> do
  resendid <- getCriticalField asValidUserID "resendid"
  newuser <- guardJustM $ dbQuery $ GetUserByID resendid
  if (usercompany newuser /= companyid company)
     then  do
       -- We need to check if there is a company invitation, and if it is we send email again
       _ <- guardJustM $ dbQuery $ GetCompanyInvite (companyid company) resendid
       sendTakeoverSingleUserMail user company newuser
     else  do
       -- Else we just send an email
       sendNewCompanyUserMail user company newuser
  runJSONGenT $ value "resent" True

sendNewCompanyUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendNewCompanyUserMail inviter company user = do
  ctx <- getContext
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  al <- newUserAccountRequestLink (ctxlang ctx) (userid user) CompanyInvitation
  mail <- mailNewCompanyUserInvite ctx user inviter company companyui al
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = getFullName user, email = getEmail user }]}
  return ()

sendTakeoverSingleUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendTakeoverSingleUserMail inviter company user = do
  ctx <- getContext
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  mail <- mailTakeoverSingleUserInvite ctx user inviter company companyui (LinkCompanyTakeover (companyid company))
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress user] }



{- |
    Handles a role change by switching a user from
    admin or to company.
-}
handleChangeRoleOfCompanyAccount :: Kontrakcja m => m JSValue
handleChangeRoleOfCompanyAccount = withCompanyAdmin $ \(_user, company) -> do
  changeid <- getCriticalField asValidUserID "changeid"
  makeadmin <- getField "makeadmin"
  changeuser <- guardJustM $ dbQuery $ GetUserByID changeid
  unless (usercompany changeuser == companyid company) internalError --make sure user is in same company
  _ <- dbUpdate $ SetUserCompanyAdmin changeid (makeadmin == Just "true")
  runJSONGenT $ value "changed" True

{- |
    Handles deletion of a company user or the deletion of the company invite
    if they haven't yet accepted.
-}
handleRemoveCompanyAccount :: Kontrakcja m => m JSValue
handleRemoveCompanyAccount = withCompanyAdmin $ \(_user, company) -> do
  removeuid <- getCriticalField asValidUserID "removeid"
  removeuser <- guardJustM $ dbQuery $ GetUserByID $ removeuid
  isdeletable <- isUserDeletable removeuser
  case (companyid company == usercompany removeuser,isdeletable) of
    (True,True) -> do
             _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (userid removeuser)
             _ <- dbUpdate $ DeleteUser (userid removeuser)
             runJSONGenT $ value "removed" True
    (True,False) -> do
             runJSONGenT $ value "removed" False
    _            -> do
             _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (userid removeuser)
             runJSONGenT $ value "removed" True

{- |
    This handles the company account takeover links, and replaces
    the old stuff that was based in UserID.  It checks that the logged in
    user has actually been invited to join the company in the URL.
-}
handleGetBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m (Either KontraLink (Either KontraLink String))
handleGetBecomeCompanyAccount companyid = withUserGet $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  invite <- dbQuery $ GetCompanyInvite companyid (userid user)
  case invite of
       Nothing -> do
        addFlashM $ flashMessageBecomeCompanyLogInDifferentUser
        return $ Left LinkAccount
       _ -> do
        newcompany <- guardJustM $ dbQuery $ GetCompany companyid
        Right <$> pageDoYouWantToBeCompanyAccount newcompany

handlePostBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m KontraLink
handlePostBecomeCompanyAccount cid = withUserPost $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  _ <- guardJustM $ dbQuery $ GetCompanyInvite cid (userid user)
  newcompany <- guardJustM $ dbQuery $ GetCompany cid
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) False
  _ <- dbUpdate $ SetUserCompany (userid user) (companyid newcompany)
  -- if we are inviting a user with a plan to join the company, we
  -- should delete their personal plan
  addFlashM $ flashMessageUserHasBecomeCompanyAccount newcompany
  return $ LinkAccount

