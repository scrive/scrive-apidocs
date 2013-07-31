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
        _ <- dbUpdate $ AddCompanyInvite CompanyInvite {
                invitedemail = Email email
              , invitedfstname = fstname
              , invitedsndname = sndname
              , invitingcompany = companyid company
              }
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
                        _ <- dbUpdate $ AddCompanyInvite CompanyInvite {
                            invitedemail = Email email
                          , invitedfstname = fstname
                          , invitedsndname = sndname
                          , invitingcompany = companyid company
                          }
                        runJSONGenT $ value "added" True
              _ -> runJSONGenT $ value "added" False

{- |
    Handles a resend by checking for the user and invite
    and resending the invite that they would've received.
-}
handleResendToCompanyAccount :: Kontrakcja m => m JSValue
handleResendToCompanyAccount = withCompanyAdmin $ \(user, company) -> do
  resendemail <- getCriticalField asValidEmail "resendemail"
  _ <- guardJustM $ dbQuery $ GetCompanyInvite (companyid company) (Email resendemail)
  newuser <- guardJustM $ dbQuery $ GetUserByEmail (Email resendemail)
  if (usercompany newuser /= companyid company)
     then  sendTakeoverSingleUserMail user company newuser
     else  sendNewCompanyUserMail user company newuser
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
  removeemail <- getCriticalField asValidEmail "removeemail"
  mremoveuser <- dbQuery $ GetUserByEmail $ Email removeemail
  mremovecompany <- case mremoveuser of
                           Just u -> Just <$> getCompanyForUser u
                           Nothing -> return Nothing
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
  user <- guardJustM $ ctxmaybeuser <$> getContext
  newcompany <- guardJustM $ dbQuery $ GetCompany cid
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) False
  _ <- dbUpdate $ SetUserCompany (userid user) (companyid newcompany)
  -- if we are inviting a user with a plan to join the company, we
  -- should delete their personal plan
  addFlashM $ flashMessageUserHasBecomeCompanyAccount newcompany
  return $ LinkAccount

{- |
    This checks that the logged in user is suitable for being
    taken over by a company.  This means that they are a private user
    and that they were actually invited by the company.
-}
guardGoodForTakeover :: Kontrakcja m => CompanyID -> m ()
guardGoodForTakeover companyid = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  _ <- guardJustM $ dbQuery $ GetCompanyInvite companyid (Email $ getEmail user)
  return ()
