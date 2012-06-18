module CompanyAccounts.CompanyAccountsControl (
    handleGetCompanyAccounts
  , handleCompanyAccounts
  , handlePostCompanyAccounts
  , handleAddCompanyAccount
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
import Happstack.Server hiding (simpleHTTP)
import Text.JSON (JSValue(..), toJSObject, toJSString)

import ActionQueue.UserAccountRequest
import AppView
import DB
import Company.CompanyControl (withCompanyAdmin)
import Company.Model
import CompanyAccounts.Model
import CompanyAccounts.CompanyAccountsView
import Doc.Model
import Doc.DocStateData
import InputValidation
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import Misc
import Util.Actor
import Util.FlashUtil
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import User.Action
import User.Utils
import User.UserControl
import User.UserView
import User.History.Model

{- |
    Handles the showing of the company accounts page.
    This page shows the list of users that are members of a company
    and a list of the pending takeover invites.
-}
handleGetCompanyAccounts :: Kontrakcja m => m (Either KontraLink Response)
handleGetCompanyAccounts = withUserGet $ withCompanyAdmin $ \_ -> do
  content <- viewCompanyAccounts
  renderFromBody kontrakcja content

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
  params <- getListParamsNew
  let companypage = companyAccountsSortSearchPage params companyaccounts
  return $ JSObject $ toJSObject [("list",
                                   JSArray $
                                   map (\f -> JSObject $
                                              toJSObject [ ("link", JSString $ toJSString $ show $ LinkUserAdmin $ camaybeuserid f), -- | Used in admins only
                                                           ("fields",
                                                           JSObject $ toJSObject [
                                                              ("id", JSString $ toJSString $ maybe "0" show $ camaybeuserid f)
                                                            , ("fullname", JSString $ toJSString $ cafullname f)
                                                            , ("email", JSString $ toJSString $ caemail f)
                                                            , ("role", JSString $ toJSString $ show $ carole f)
                                                            , ("deletable", JSBool $ cadeletable f)
                                                            , ("activated", JSBool $ caactivated f)
                                                            , ("isctxuser", JSBool $ Just (userid user) == camaybeuserid f)])])
                                   (list companypage))
                                 ,("paging", pagingParamsJSON companypage)]

{- |
    A special data type used for just displaying stuff in the list
    this lets make a unified list of users and pending invites
-}
data CompanyAccount = CompanyAccount {
    camaybeuserid :: Maybe UserID --the account's userid if they have one & not if they're a pending takeover invite
  , cafstname :: String --the account's first name
  , casndname :: String --the account's last name
  , cafullname :: String --the account's fullname
  , caemail :: String --the account's email
  , carole :: Role --the account's role
  , cadeletable :: Bool --can the account be deleted, or do they have pending documents?
  , caactivated :: Bool --is the account a full company user with accepted tos?
  }

data Role = RoleAdmin --an admin user
            | RoleStandard --a standard user
            | RolePending --a pending takeover request (didn't want to implement role switching for invites!)
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

--

{- |
    Handles a post from the company accounts page and routes
    it through to either add, remove, change role, or send a resend an invite
    depending on the flags set within the post variables.
-}
handlePostCompanyAccounts :: Kontrakcja m => m KontraLink
handlePostCompanyAccounts = withCompanyAdmin $ \_ -> do
  add <- isFieldSet "add"
  resend <- isFieldSet "resend"
  changerole <- isFieldSet "changerole"
  remove <- isFieldSet "remove"
  case True of
    _ | add -> handleAddCompanyAccount
    _ | resend -> handleResendToCompanyAccount
    _ | changerole -> handleChangeRoleOfCompanyAccount
    _ | remove -> handleRemoveCompanyAccount
    _ -> return ()
  LinkCompanyAccounts <$> getListParamsForSearch

{- |
    Handles adding a company user either by creating them or
    by inviting them to be taken over.
-}
handleAddCompanyAccount :: Kontrakcja m => m ()
handleAddCompanyAccount = withCompanyAdmin $ \(user, company) -> do
  ctx <- getContext
  memail <- getOptionalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"
  mexistinguser <- maybe (return Nothing) (dbQuery . GetUserByEmail Nothing . Email) memail
  mexistingcompany <- maybe (return Nothing) getCompanyForUser mexistinguser

  minvitee <-
    case (memail, mexistinguser, mexistingcompany) of
      (Just email, Nothing, Nothing) -> do
        --create a new company user
        newuser' <- guardJustM $ createUser (Email email) (fstname, sndname) (Just $ companyid company) (ctxlocale ctx)
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
        return $ Just existinguser
      _ -> return Nothing

  -- record the invite and flash a message
  when (isJust minvitee) $ do
    email <- guardJust memail
    _ <- dbUpdate $ AddCompanyInvite CompanyInvite {
            invitedemail = Email email
          , invitedfstname = fstname
          , invitedsndname = sndname
          , invitingcompany = companyid company
          }
    addFlashM flashMessageCompanyAccountInviteSent

{- |
    Handles a resend by checking for the user and invite
    and resending the invite that they would've received.
-}
handleResendToCompanyAccount :: Kontrakcja m => m ()
handleResendToCompanyAccount = withCompanyAdmin $ \(user, company) -> do
  resendid <- getCriticalField asValidUserID "resendid"
  resendemail <- getCriticalField asValidEmail "resendemail"
  muserbyid <- dbQuery $ GetUserByID resendid
  mcompanybyid <- maybe (return Nothing) getCompanyForUser muserbyid
  minvite <- dbQuery $ GetCompanyInvite (companyid company) (Email resendemail)
  muserbyemail <- dbQuery $ GetUserByEmail Nothing (Email resendemail)

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
          return True
      _ -> return False

  when resent $ addFlashM flashMessageCompanyAccountInviteResent

sendNewCompanyUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendNewCompanyUserMail inviter company user = do
  ctx <- getContext
  al <- newUserAccountRequestLink $ userid user
  mail <- mailNewCompanyUserInvite (ctxhostpart ctx) user inviter company al
  scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [MailAddress { fullname = getFullName user, email = getEmail user }]}
  return ()

sendTakeoverPrivateUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendTakeoverPrivateUserMail inviter company user = do
  ctx <- getContext
  mail <- mailTakeoverPrivateUserInvite (ctxhostpart ctx) user inviter company (LinkCompanyTakeover (companyid company))
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
                ++ "  " ++ getFullName user
                ++ " has been emailed about the problem and advised to contact us if they want to move accounts."
  scheduleEmailSendout (ctxmailsconfig ctx) $ emptyMail {
        to = [MailAddress { fullname = "info@skrivapa.se", email = "info@skrivapa.se" }]
      , title = "Attempted Company Account Takeover"
      , content = content
  }

{- |
    Handles a role change by switching a user from
    admin or to company.
-}
handleChangeRoleOfCompanyAccount :: Kontrakcja m => m ()
handleChangeRoleOfCompanyAccount = withCompanyAdmin $ \(_user, company) -> do
  changeid <- getCriticalField asValidUserID "changeid"
  makeadmin <- getField "makeadmin"
  changeuser <- guardJustM $ dbQuery $ GetUserByID changeid
  changecompanyid <- guardJust $ usercompany changeuser
  unless (changecompanyid == companyid company) internalError --make sure user is in same company
  _ <- dbUpdate $ SetUserCompanyAdmin changeid (makeadmin == Just "true")
  return ()

{- |
    Handles deletion of a company user or the deletion of the company invite
    if they haven't yet accepted.
-}
handleRemoveCompanyAccount :: Kontrakcja m => m ()
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
             addFlashM flashMessageCompanyAccountDeleted
           else
             addFlashM flashMessageUserHasLiveDocs
    _ -> do
      _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (Email removeemail)
      addFlashM flashMessageCompanyAccountDeleted

{- |
    This handles the company account takeover links, and replaces
    the old stuff that was based in UserID.  It checks that the logged in
    user has actually been invited to join the company in the URL.
-}
handleGetBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m (Either KontraLink Response)
handleGetBecomeCompanyAccount companyid = withUserGet $ do
  _ <- guardGoodForTakeover companyid
  Context{ctxmaybeuser = Just user} <- getContext
  mcompany <- getCompanyForUser user
  newcompany <- guardJustM $ dbQuery $ GetCompany companyid
  addFlashM $ modalDoYouWantToBeCompanyAccount newcompany
  content <- showUser user mcompany False
  renderFromBody kontrakcja content

handlePostBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m KontraLink
handlePostBecomeCompanyAccount cid = withUserPost $ do
  _ <- guardGoodForTakeover cid
  Context{ctxmaybeuser = Just user} <- getContext
  newcompany <- guardJustM $ dbQuery $ GetCompany cid
  _ <- dbUpdate $ SetUserCompany (userid user) (Just $ companyid newcompany)
  _ <- resaveDocsForUser (userid user)
  addFlashM $ flashMessageUserHasBecomeCompanyAccount newcompany
  return $ LinkAccount

{- |
    Resaving the user's documents means that their new company
    will take them over.
    This is pretty icky - it'd be way nicer if it wasn't like this.
-}
resaveDocsForUser :: Kontrakcja m => UserID -> m ()
resaveDocsForUser uid = do
  user <- guardJustM $ dbQuery $ GetUserByID uid
  userdocs <- dbQuery $ GetDocumentsByAuthor uid
  attachments <- dbQuery $ GetAttachmentsByAuthor uid
  time <- ctxtime <$> getContext
  let actor = systemActor time
  mapM_ (\doc -> dbUpdate $ AdminOnlySaveForUser (documentid doc) user actor) (userdocs ++ attachments)
  return ()

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
