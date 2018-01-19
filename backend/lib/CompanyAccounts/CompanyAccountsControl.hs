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

import Data.Char
import Text.JSON (JSValue(..))
import Text.JSON.Gen

import Company.CompanyUI.Model
import Company.Model
import CompanyAccounts.CompanyAccountsView
import CompanyAccounts.Model
import DB
import Happstack.Fields
import InputValidation
import InternalResponse
import Kontra
import KontraLink
import Mails.SendMail
import MinutesTime
import User.Action
import User.CallbackScheme.Model
import User.Email
import User.History.Model
import User.UserAccountRequest
import User.UserControl
import User.Utils
import Util.HasSomeUserInfo
import Util.MonadUtils

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
  Just user <- get ctxmaybeuser <$> getContext
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
      , cafullname = getFullName u
      , caemail = getEmail u
      , carole = if (useriscompanyadmin u)
                  then RoleAdmin
                  else RoleStandard
      , cadeletable = userid u `elem` deletableuserids
      , caactivated = isJust $ userhasacceptedtermsofservice u
      , catos = userhasacceptedtermsofservice u
      , catotpactive = usertotpactive u
      }
    mkAccountFromInvite (i,fn,ln,em) = CompanyAccount {
        camaybeuserid = inviteduserid i
      , cafullname = fn ++ " " ++ ln
      , caemail = em
      , carole = RoleInvite
      , cadeletable = True
      , caactivated = False
      , catos = Nothing
      , catotpactive = False
      }

  textFilter <- getField "text" >>= \case
    Nothing -> return $ const True
    Just s -> return  $ companyAccountsTextSearch s

  (sorting :: CompanyAccount -> CompanyAccount -> Ordering) <- getField "sorting" >>= \case
    Nothing  -> return $ \_ _ -> EQ
    Just s   -> return $ companyAccountsSorting s

  sortingWithOrder <- getField "order" >>= \case
    Just "ascending"   -> return sorting
    _ -> return $ flip sorting

  runJSONGenT $ do
    objects "accounts" $ for (sortBy sortingWithOrder $ filter textFilter companyaccounts) $ \f -> do
      value "id" $ show $ camaybeuserid f
      value "fullname" $ cafullname f
      value "email" $ caemail f
      value "role" $ show $ carole f
      value "deletable" $ cadeletable f
      value "activated" $ caactivated f
      value "isctxuser" $ userid user == camaybeuserid f
      value "tos"       $ formatTimeISO <$> (catos f)
      value "twofactor_active" $ catotpactive f

{- |
    A special data type used for just displaying stuff in the list
    this lets make a unified list of users and pending invites
-}
data CompanyAccount = CompanyAccount
  { camaybeuserid :: UserID       -- ^ the account's or invites userid
  , cafullname    :: String       -- ^ the account's or invites fullname
  , caemail       :: String       -- ^ the account's or invites email
  , carole        :: Role         -- ^ the account's role (always Standard for invites)
  , cadeletable   :: Bool         -- ^ can the account be deleted, or do they have pending documents (always True for invites)?
  , caactivated   :: Bool         -- ^ is the account a full company user with accepted tos? (always False for invites)
  , catos         :: Maybe UTCTime -- ^ TOS time if any (always Nothing for invites)
  , catotpactive  :: Bool         -- ^ Whether TOTP two-factor authentication is active
  }

data Role = RoleAdmin    -- ^ an admin user
          | RoleStandard -- ^ a standard user
          | RoleInvite   -- ^ an invite for a user that is in different company
  deriving (Eq, Ord, Show)


companyAccountsTextSearch :: String -> CompanyAccount -> Bool
companyAccountsTextSearch s ca = match (cafullname ca) || match (caemail ca)
  where match m = map toUpper s `isInfixOf` map toUpper m

companyAccountsSorting :: String -> CompanyAccount -> CompanyAccount -> Ordering
companyAccountsSorting "fullname" = companyAccountsSortingBy cafullname
companyAccountsSorting "email" = companyAccountsSortingBy caemail
companyAccountsSorting "role" = companyAccountsSortingBy carole
companyAccountsSorting "deletable" = companyAccountsSortingBy cadeletable
companyAccountsSorting "activated" = companyAccountsSortingBy caactivated
companyAccountsSorting "id" = companyAccountsSortingBy camaybeuserid
companyAccountsSorting "twofactor_active" = companyAccountsSortingBy catotpactive
companyAccountsSorting _ = const $ const EQ

companyAccountsSortingBy :: Show a => (CompanyAccount -> a) -> CompanyAccount -> CompanyAccount -> Ordering
companyAccountsSortingBy f ca1 ca2 = compare (map toUpper $ show $ f ca1) (map toUpper $ show $ f ca2)

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
        newuser' <- guardJustM $ createUser (Email email) (fstname, sndname) (companyid company,False) (get ctxlang ctx) CompanyInvitation
        _ <- dbUpdate $
             LogHistoryUserInfoChanged (userid newuser') (get ctxipnumber ctx) (get ctxtime ctx)
                                       (userinfo newuser')
                                       ((userinfo newuser') { userfstname = fstname , usersndname = sndname })
                                       (userid <$> get ctxmaybeuser ctx)
        newuser <- guardJustM $ dbQuery $ GetUserByID (userid newuser')
        _ <- sendNewCompanyUserMail user company newuser
        runJSONGenT $ value "added" True
      (Just existinguser) ->
        if (usercompany existinguser == companyid company)
           then runJSONGenT $ value "added" False >> value "samecompany" True
           else do
            -- If user exists we allow takeover only if he is the only user in his company
            users <- dbQuery $ GetCompanyAccounts $ usercompany existinguser
            if (length users == 1)
              then do
                _ <- sendTakeoverSingleUserMail user company existinguser
                _ <- dbUpdate $ AddCompanyInvite $ CompanyInvite  (userid existinguser) (companyid company)
                runJSONGenT $ value "added" True
              else do
                runJSONGenT $ value "added" False

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
       -- We need to check if there is a company invitation, and if it is, we send takeover email again
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
  uar <- newUserAccountRequest $ userid user
  let al = LinkAccountCreated (lang $ usersettings user) (uarUserID uar) (uarToken uar) CompanyInvitation
  mail <- mailNewCompanyUserInvite ctx user inviter company companyui al (uarExpires uar)
  scheduleEmailSendout $ mail { to = [MailAddress { fullname = getFullName user, email = getEmail user }]}
  return ()

sendTakeoverSingleUserMail :: Kontrakcja m => User -> Company -> User -> m ()
sendTakeoverSingleUserMail inviter company user = do
  ctx <- getContext
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  mail <- mailTakeoverSingleUserInvite ctx user inviter company companyui (LinkCompanyTakeover (companyid company))
  scheduleEmailSendout $ mail { to = [getMailAddress user] }



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
            -- We remove user, so we also want to drop all invites - they should be invalid at this point anyway.
             _ <- dbUpdate $ RemoveUserCompanyInvites (userid removeuser)
             _ <- dbUpdate $ DeleteUserCallbackScheme $ userid removeuser
             _ <- dbUpdate $ DeleteUser (userid removeuser)
             runJSONGenT $ value "removed" True
    (True,False) -> do
             runJSONGenT $ value "removed" False
    _            -> do
             -- We remove only this invite - user account in different company will still be valid
             _ <- dbUpdate $ RemoveCompanyInvite (companyid company) (userid removeuser)
             runJSONGenT $ value "removed" True

{- |
    This handles the company account takeover links, and replaces
    the old stuff that was based in UserID.  It checks that the logged in
    user has actually been invited to join the company in the URL.
-}
handleGetBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m InternalKontraResponse
handleGetBecomeCompanyAccount companyid = withUser $ \user -> do
  invite <- dbQuery $ GetCompanyInvite companyid (userid user)
  case invite of
    Nothing -> do
      flashmessage <- flashMessageBecomeCompanyLogInDifferentUser
      return $ internalResponseWithFlash flashmessage LinkAccount
    _ -> do
      ctx <- getContext
      newcompany <- guardJustM $ dbQuery $ GetCompany companyid
      internalResponse <$> (pageDoYouWantToBeCompanyAccount ctx newcompany)

handlePostBecomeCompanyAccount :: Kontrakcja m => CompanyID -> m InternalKontraResponse
handlePostBecomeCompanyAccount cid = withUser $ \user -> do
  _ <- guardJustM $ dbQuery $ GetCompanyInvite cid (userid user)
  newcompany <- guardJustM $ dbQuery $ GetCompany cid
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user) False
  _ <- dbUpdate $ SetUserCompany (userid user) (companyid newcompany)
  _ <- dbUpdate $ RemoveCompanyInvite cid (userid user)
  -- if we are inviting a user with a plan to join the company, we
  -- should delete their personal plan
  flashmessage <- flashMessageUserHasBecomeCompanyAccount newcompany
  return $ internalResponseWithFlash flashmessage LinkAccount
