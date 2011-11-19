module CompanyAccounts.CompanyAccountsControl (
    handleGetCompanyAccounts
  , handleCompanyAccounts
  , handlePostCompanyAccounts
  , handleAddCompanyAccount
  , handleGetBecomeCompanyAccount
  , handlePostBecomeCompanyAccount
  -- these should be deleted once we know people
  -- aren't still using these old takeover links
  , handleGetBecomeCompanyAccountOld
  , handlePostBecomeCompanyAccountOld
  ) where

import Control.Monad.State
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import Text.JSON (JSValue(..), toJSObject, toJSString)

import AppView
import DB.Classes
import Company.Model
import CompanyAccounts.Model
import CompanyAccounts.CompanyAccountsView
import Doc.Transitory
import InputValidation
import Kontra
import KontraLink
import ListUtil
import Mails.SendMail
import Misc
import Util.FlashUtil
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.MonadUtils
import User.Model
import User.Utils
import User.UserControl
import User.UserView

{- |
    Handles the showing of the company accounts page.
    This page shows the list of users that are members of a company
    and a list of the pending takeover invites.
-}
handleGetCompanyAccounts :: Kontrakcja m => m (Either KontraLink Response)
handleGetCompanyAccounts = withUserGet $ withCompanyAdmin $ \_ -> do
  content <- viewCompanyAccounts
  renderFromBody TopAccount kontrakcja content

{- |
    Gets the ajax data for the company accounts list.
-}
handleCompanyAccounts :: Kontrakcja m => m JSValue
handleCompanyAccounts = withCompanyAdmin $ \(user, company) -> do
  companyusers <- runDBQuery $ GetCompanyAccounts (companyid company)
  deletableuserids <- map userid <$> filterM isUserDeletable companyusers
  companyinvites <- runDBQuery $ GetCompanyInvites (companyid company)
  let isUser CompanyInvite{invitedemail} = (unEmail invitedemail) `elem` (map getEmail companyusers)
  let
    companyaccounts =
      (map mkAccountFromUser companyusers)
      ++ (map mkAccountFromInvite $ filter (not . isUser) companyinvites)
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
      , cafullname = (invitedfstname i)
                     `BS.append` (BS.fromString " ")
                     `BS.append` (invitedsndname i)
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
                                              toJSObject [("fields",
                                                           JSObject $ toJSObject [
                                                              ("id", JSString $ toJSString $ maybe "0" show $ camaybeuserid f)
                                                            , ("fullname", JSString $ toJSString $ BS.toString $ cafullname f)
                                                            , ("email", JSString $ toJSString $ BS.toString $ caemail f)
                                                            , ("role", JSString $ toJSString $ show $ carole f)
                                                            , ("deletable", JSBool $ cadeletable f)
                                                            , ("activated", JSBool $ caactivated f)
                                                            , ("isctxuser", JSBool $ (Just $ userid user) == (camaybeuserid f))])])
                                   (list companypage))
                                 ,("paging", pagingParamsJSON companypage)]

{- |
    A special data type used for just displaying stuff in the list
    this lets make a unified list of users and pending invites
-}
data CompanyAccount = CompanyAccount {
    camaybeuserid :: Maybe UserID --the account's userid if they have one & not if they're a pending takeover invite
  , cafstname :: BS.ByteString --the account's first name
  , casndname :: BS.ByteString --the account's last name
  , cafullname :: BS.ByteString --the account's fullname
  , caemail :: BS.ByteString --the account's email
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
      match s' m = isInfixOf (map toUpper s') (map toUpper (BS.toString m))
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
companyAccountsSortFunc _ = const $ const EQ

companyAccountsPageSize :: Int
companyAccountsPageSize = 20

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
  memail <- getOptionalField asValidEmail "email"
  fstname <- fromMaybe BS.empty <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe BS.empty <$> getOptionalField asValidName "sndname"
  mexistinguser <- maybe (return Nothing) (runDBQuery . GetUserByEmail Nothing . Email) memail
  mexistingcompany <- maybe (return Nothing) getCompanyForUser mexistinguser

  minvitee <-
    case (memail, mexistinguser, mexistingcompany) of
      (Just email, Nothing, Nothing) -> do
        --create a new company user
        newuser' <- guardJustM $ createUser (Email email) fstname sndname (Just company)
        _ <- runDBUpdate $ SetUserInfo (userid newuser') (userinfo newuser') {
                            userfstname = fstname
                          , usersndname = sndname
                          }
        newuser <- guardJustM $ runDBQuery $ GetUserByID (userid newuser')
        _ <- sendNewCompanyUserMail user company newuser
        return $ Just newuser
      (Just _email, Just existinguser, Nothing) -> do
        --send a takeover invite to the existing user
        _ <- sendTakeoverPrivateUserMail user company existinguser
        return $ Just existinguser
      (Just _email, Just existinguser, Just existingcompany) | existingcompany /= company -> do
        --this is a corner case where someone is trying to takeover someone in another company
        --we send emails to tell people, but we don't send any activation links
        _ <- sendTakeoverCompanyUserMail user company existinguser
        _ <- sendTakeoverCompanyInternalWarningMail user company existinguser
        return $ Just existinguser
      _ -> return Nothing

  -- record the invite and flash a message
  when (isJust minvitee) $ do
    email <- guardJust memail
    _ <- runDBUpdate $ AddCompanyInvite $ CompanyInvite{
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
  resendid <- getCriticalField asValidNumber "resendid"
  resendemail <- getCriticalField asValidEmail "resendemail"
  muserbyid <- runDBQuery $ GetUserByID (UserID resendid)
  mcompanybyid <- maybe (return Nothing) getCompanyForUser muserbyid
  minvite <- runDBQuery $ GetCompanyInvite (companyid company) (Email resendemail)
  muserbyemail <- runDBQuery $ GetUserByEmail Nothing (Email resendemail)

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
          _ <- sendTakeoverCompanyUserMail user company userbyemail
          _ <- sendTakeoverCompanyInternalWarningMail user company userbyemail
          return True
      _ -> return False

  when (resent) $ do
    addFlashM flashMessageCompanyAccountInviteResent

sendNewCompanyUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendNewCompanyUserMail inviter company user = do
  ctx <- getContext
  al <- newAccountCreatedLink user
  mail <- mailNewCompanyUserInvite (ctxhostpart ctx) (getSmartName inviter) (getCompanyName company) (getEmail user) (getFullName user) al
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress { fullname = getFullName user, email = getEmail user }]}
  return ()

sendTakeoverPrivateUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendTakeoverPrivateUserMail inviter company user = do
  ctx <- getContext
  mail <- mailTakeoverPrivateUserInvite (ctxhostpart ctx) user inviter company
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress user] }

sendTakeoverCompanyUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendTakeoverCompanyUserMail inviter company user = do
  ctx <- getContext
  mail <- mailTakeoverCompanyUserInfo user inviter company
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress user] }

sendTakeoverCompanyInternalWarningMail :: Kontrakcja m => User -> Company -> User -> m ()
sendTakeoverCompanyInternalWarningMail inviter company user = do
  ctx <- getContext
  let content = "Oh dear!  " ++ (BS.toString $ getFullName inviter)
                ++ " &lt;" ++ (BS.toString $ getEmail inviter) ++ "&gt;"
                ++ " in company " ++ (BS.toString $ getCompanyName company)
                ++ " has asked to takeover " ++ (BS.toString $ getFullName user)
                ++ " &lt;" ++ (BS.toString $ getEmail user) ++ "&gt;"
                ++ " who is already in a company."
                ++ "  " ++ (BS.toString $ getFullName user)
                ++ " has been emailed about the problem and advised to contact us if they want to move accounts."
  scheduleEmailSendout (ctxesenforcer ctx) $ emptyMail {
        to = [MailAddress { fullname = BS.fromString "info@skrivapa.se", email = BS.fromString "info@skrivapa.se" }]
      , title = BS.fromString $ "Attempted Company Account Takeover"
      , content = BS.fromString $ content
  }

{- |
    Handles a role change by switching a user from
    admin or to company.
-}
handleChangeRoleOfCompanyAccount :: Kontrakcja m => m ()
handleChangeRoleOfCompanyAccount = withCompanyAdmin $ \(_user, company) -> do
  changeid <- getCriticalField asValidNumber "changeid"
  makeadmin <- getFieldUTF "makeadmin"
  changeuser <- guardJustM $ runDBQuery $ GetUserByID (UserID changeid)
  changecompanyid <- guardJust $ usercompany changeuser
  guard $ changecompanyid == companyid company --make sure user is in same company
  _ <- runDBUpdate $ SetUserCompanyAdmin (UserID changeid) (makeadmin == Just (BS.fromString "true"))
  return ()

{- |
    Handles deletion of a company user or the deletion of the company invite
    if they haven't yet accepted.
-}
handleRemoveCompanyAccount :: Kontrakcja m => m ()
handleRemoveCompanyAccount = withCompanyAdmin $ \(_user, company) -> do
  removeid <- getCriticalField asValidNumber "removeid"
  removeemail <- getCriticalField asValidEmail "removeemail"
  mremoveuser <- runDBQuery $ GetUserByID (UserID removeid)
  mremovecompany <- maybe (return Nothing) getCompanyForUser mremoveuser
  isdeletable <- maybe (return False) isUserDeletable mremoveuser

  case (mremoveuser, mremovecompany) of
    (Just removeuser, Just removecompany) | company == removecompany -> do
         --there's an actual user to delete
         if isdeletable
           then do
             _ <- runDBUpdate $ RemoveCompanyInvite (companyid company) (Email $ getEmail removeuser)
             _ <- runDBUpdate $ DeleteUser (userid removeuser)
             addFlashM flashMessageCompanyAccountDeleted
           else do
             addFlashM flashMessageUserHasLiveDocs
    _ -> do
      _ <- runDBUpdate $ RemoveCompanyInvite (companyid company) (Email removeemail)
      addFlashM flashMessageCompanyAccountDeleted

{- |
    This handles the company account takeover links that we've sent out.
    You need a userid to use it, and it's only secure by obscurity:
    if you were a hacker you could join a company by guessing user ids, bleurgh

    Deprecated!!
 -}
handleGetBecomeCompanyAccountOld :: Kontrakcja m => UserID -> m (Either KontraLink Response)
handleGetBecomeCompanyAccountOld inviterid = withUserGet $ do
  inviter <- guardJustM $ runDBQuery $ GetUserByID inviterid
  company <- guardJustM $ getCompanyForUser inviter
  addFlashM $ modalDoYouWantToBeCompanyAccount company
  Context{ctxmaybeuser = Just user} <- getContext
  mcompany <- getCompanyForUser user
  content <- showUser user mcompany False
  renderFromBody TopAccount kontrakcja content

handlePostBecomeCompanyAccountOld :: Kontrakcja m => UserID -> m KontraLink
handlePostBecomeCompanyAccountOld inviterid = withUserPost $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  inviter <- guardJustM $ runDBQuery $ GetUserByID inviterid
  company <- guardJustM $ getCompanyForUser inviter
  _ <- runDBUpdate $ SetUserCompany (userid user) (Just $ companyid company)
  _ <- resaveDocsForUser (userid user)
  addFlashM $ flashMessageUserHasBecomeCompanyAccount company
  return $ LinkAccount False

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
  newcompany <- guardJustM $ runDBQuery $ GetCompany companyid
  addFlashM $ modalDoYouWantToBeCompanyAccount newcompany
  content <- showUser user mcompany False
  renderFromBody TopAccount kontrakcja content

handlePostBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m KontraLink
handlePostBecomeCompanyAccount cid = withUserPost $ do
  _ <- guardGoodForTakeover cid
  Context{ctxmaybeuser = Just user} <- getContext
  newcompany <- guardJustM $ runDBQuery $ GetCompany cid
  _ <- runDBUpdate $ SetUserCompany (userid user) (Just $ companyid newcompany)
  _ <- resaveDocsForUser (userid user)
  addFlashM $ flashMessageUserHasBecomeCompanyAccount newcompany
  return $ LinkAccount False

{- |
    Resaving the user's documents means that their new company
    will take them over.
    This is pretty icky - it'd be way nicer if it wasn't like this.
-}
resaveDocsForUser :: Kontrakcja m => UserID -> m ()
resaveDocsForUser uid = do
  user <- runDBOrFail $ dbQuery $ GetUserByID uid
  userdocs <- doc_query $ GetDocumentsByUser user
  mapM_ (\doc -> doc_update $ AdminOnlySaveForUser (documentid doc) user) userdocs
  return ()

{- |
    This checks that the logged in user is suitable for being
    taken over by a company.  This means that they are a private user
    and that they were actually invited by the company.
-}
guardGoodForTakeover :: Kontrakcja m => CompanyID -> m ()
guardGoodForTakeover companyid = do
  Context{ctxmaybeuser = Just user} <- getContext
  _ <- guard $ isNothing (usercompany user)
  _ <- guardJustM $ runDBQuery $ GetCompanyInvite companyid (Email $ getEmail user)
  return ()

{- |
    Guards that there is a user that is logged in and is the admin
    of a company.  The user and company are passed as params to the
    given action, to save you having to look them up yourself.
-}
withCompanyAdmin :: Kontrakcja m => ((User, Company) -> m a) -> m a
withCompanyAdmin action = do
  Context{ ctxmaybeuser } <- getContext
  user <- guardJust ctxmaybeuser
  company <- guardJustM $ getCompanyForUser user
  if useriscompanyadmin user then action (user, company) else mzero
