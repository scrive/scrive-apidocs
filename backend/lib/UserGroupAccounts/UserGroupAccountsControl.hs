module UserGroupAccounts.UserGroupAccountsControl (
    handleUserGroupAccounts
  , handleAddUserGroupAccount
  , handleResendToUserGroupAccount
  , handleChangeRoleOfUserGroupAccount
  , handleRemoveUserGroupAccount
  , handleGetBecomeUserGroupAccount
  , handlePostBecomeUserGroupAccount
  -- this shares some handy stuff with the adminonly section
  , handleUserGroupAccountsForAdminOnly
  , sendTakeoverSingleUserMail
  , sendNewUserGroupUserMail
  ) where

import Data.Char
import Text.JSON (JSValue(..))
import Text.JSON.Gen

import AccessControl.Types
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
import UserGroup.Model
import UserGroup.Types
import UserGroupAccounts.Model
import UserGroupAccounts.UserGroupAccountsView
import Util.HasSomeUserInfo
import Util.MonadUtils

-- | Get the ajax data for the company accounts list.
handleUserGroupAccounts :: Kontrakcja m => m JSValue
handleUserGroupAccounts = withCompanyAdmin $ \(_user, ug) -> do
  handleUserGroupAccountsInternal . get ugID $ ug

-- | Get the ajax data for the company accounts list.
handleUserGroupAccountsForAdminOnly :: Kontrakcja m => UserGroupID -> m JSValue
handleUserGroupAccountsForAdminOnly ugid = onlySalesOrAdmin $ do
  handleUserGroupAccountsInternal ugid

-- | Create the JSON for either the admin only user or the logged in
-- company admin.
handleUserGroupAccountsInternal :: Kontrakcja m => UserGroupID -> m JSValue
handleUserGroupAccountsInternal ugid = do
  muser <- get ctxmaybeuser <$> getContext
  user <- case muser of
    Nothing   -> unexpectedError
                 "handleUserGroupAccountsInternal: No user in Context!"
    Just user -> return user
  users <- dbQuery . UserGroupGetUsers $ ugid
  deletableuserids <- map userid <$> filterM isUserDeletable users
  invites <- dbQuery $ UserGroupGetInvitesWithUsersData ugid
  let isUser (invite,_,_,_) = (inviteduserid invite) `elem` map userid users
  let
    companyaccounts =
      map mkAccountFromUser users
      ++ map mkAccountFromInvite (filter (not . isUser) invites)
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
handleAddUserGroupAccount :: Kontrakcja m => m JSValue
handleAddUserGroupAccount = withCompanyAdmin $ \(ctxuser, ug) -> do
  ctx <- getContext
  email <-  guardJustM $ getOptionalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"
  let deftrgugid = get ugID ug
  trgugid <- fromMaybe deftrgugid <$>
               getOptionalField asValidUserGroupID "user_group_id"
  mexistinguser <- dbQuery $ GetUserByEmail $ Email email
  let acc = mkAccPolicy [ (CreateA, UserR, trgugid) ]
      err = unexpectedError "Insufficient privileges"  -- XXXFREDRIK???
      adminUser = maybe err id (get ctxmaybeuser ctx)
  roles <- dbQuery . GetRoles $ adminUser
  accessControl roles acc err $ do
    case mexistinguser of
        Nothing -> do
          --create a new company user
          newuser' <- guardJustM $ createUser (Email email)
                                              (fstname, sndname)
                                              (trgugid, False)
                                              (get ctxlang ctx)
                                              CompanyInvitation
          void $ dbUpdate $
               LogHistoryUserInfoChanged (userid newuser') (get ctxipnumber ctx) (get ctxtime ctx)
                                         (userinfo newuser')
                                         ((userinfo newuser') { userfstname = fstname , usersndname = sndname })
                                         (userid <$> get ctxmaybeuser ctx)
          newuser <- guardJustM $ dbQuery $ GetUserByID (userid newuser')
          void $ sendNewUserGroupUserMail ctxuser ug newuser
          runJSONGenT $ value "added" True
        Just existinguser -> do
          let originugid = usergroupid existinguser
              exuserid = userid existinguser
          if (originugid == trgugid)
             then
               runJSONGenT $ do
                 value "added" False
                 value "samecompany" True
             else do
               hasOriginDelete <- do  -- UpdateA? XXXFREDRIK
                 let policy = mkAccPolicy [(DeleteA, UserR, originugid)]
                 roleHasPermission roles policy
               if (hasOriginDelete)
                 then do
                   -- just move the user; XXXFREDRIK
                   -- - possibly orphaning current user group?
                   -- - should we delete it in that case?
                   (dbUpdate $ SetUserUserGroup exuserid trgugid) >>= \case
                     False ->
                       runJSONGenT $ value "added" False
                     True ->
                       runJSONGenT $ do
                         value "added" True
                         value "samecompany" False
                         value "previous_user_group_id" (show originugid)
                 else do
                   -- If user exists we allow takeover only if he is the only user in his company
                   users <- dbQuery . UserGroupGetUsers . usergroupid $ existinguser
                   if (length users == 1)
                     then do
                       void $ sendTakeoverSingleUserMail ctxuser ug existinguser
                       void $ dbUpdate $ AddUserGroupInvite $ UserGroupInvite (userid existinguser) trgugid
                       runJSONGenT $ value "added" True
                     else do
                       runJSONGenT $ value "added" False

{- |
    Handles a resend by checking for the user and invite
    and resending the invite that they would've received.
-}
handleResendToUserGroupAccount :: Kontrakcja m => m JSValue
handleResendToUserGroupAccount = withCompanyAdmin $ \(user, ug) -> do
  resendid <- getCriticalField asValidUserID "resendid"
  newuser <- guardJustM $ dbQuery $ GetUserByID resendid
  let ugid = get ugID ug
  if (usergroupid newuser /= ugid)
     then  do
       -- We need to check if there is a company invitation, and if it is, we send takeover email again
       void $ guardJustM $ dbQuery $ GetUserGroupInvite ugid resendid
       sendTakeoverSingleUserMail user ug newuser
     else  do
       -- Else we just send an email
       sendNewUserGroupUserMail user ug newuser
  runJSONGenT $ value "resent" True

sendNewUserGroupUserMail :: Kontrakcja m => User -> UserGroup -> User -> m ()
sendNewUserGroupUserMail inviter ug user = do
  ctx <- getContext
  uar <- newUserAccountRequest $ userid user
  let al = LinkAccountCreated (lang $ usersettings user) (uarUserID uar) (uarToken uar) CompanyInvitation
  mail <- mailNewUserGroupUserInvite ctx user inviter ug al (uarExpires uar)
  scheduleEmailSendout $ mail { to = [MailAddress { fullname = getFullName user, email = getEmail user }]}
  return ()

sendTakeoverSingleUserMail :: Kontrakcja m => User -> UserGroup -> User -> m ()
sendTakeoverSingleUserMail inviter ug user = do
  ctx <- getContext
  mail <- mailTakeoverSingleUserInvite ctx user inviter ug (LinkCompanyTakeover . get ugID $ ug)
  scheduleEmailSendout $ mail { to = [getMailAddress user] }



{- |
    Handles a role change by switching a user from
    admin or to company.
-}
handleChangeRoleOfUserGroupAccount :: Kontrakcja m => m JSValue
handleChangeRoleOfUserGroupAccount = withCompanyAdmin $ \(_user, ug) -> do
  changeid <- getCriticalField asValidUserID "changeid"
  makeadmin <- getField "makeadmin"
  changeuser <- guardJustM $ dbQuery $ GetUserByID changeid
  unless (usergroupid changeuser == get ugID ug) internalError --make sure user is in same company
  void $ dbUpdate $ SetUserCompanyAdmin changeid (makeadmin == Just "true")
  runJSONGenT $ value "changed" True

{- |
    Handles deletion of a company user or the deletion of the company invite
    if they haven't yet accepted.
-}
handleRemoveUserGroupAccount :: Kontrakcja m => m JSValue
handleRemoveUserGroupAccount = withCompanyAdmin $ \(user, ug) -> do
  removeuid <- getCriticalField asValidUserID "removeid"
  removeuser <- guardJustM $ dbQuery $ GetUserByID $ removeuid
  isdeletable <- isUserDeletable removeuser
  ctx <- getContext
  case (get ugID ug == usergroupid removeuser,isdeletable) of
    (True,True) -> do
            -- We remove user, so we also want to drop all invites - they should be invalid at this point anyway.
             void $ dbUpdate $ RemoveUserUserGroupInvites (userid removeuser)
             void $ dbUpdate $ DeleteUserCallbackScheme $ userid removeuser
             void $ dbUpdate $ DeleteUser (userid removeuser)
             void $ dbUpdate $ LogHistoryAccountDeleted (userid removeuser) (userid user) (get ctxipnumber ctx) (get ctxtime ctx)
             runJSONGenT $ value "removed" True
    (True,False) -> do
             runJSONGenT $ value "removed" False
    _            -> do
             -- We remove only this invite - user account in different company will still be valid
             void $ dbUpdate $ RemoveUserGroupInvite (get ugID ug) (userid removeuser)
             runJSONGenT $ value "removed" True

{- |
    This handles the company account takeover links, and replaces
    the old stuff that was based in UserID.  It checks that the logged in
    user has actually been invited to join the company in the URL.
-}
handleGetBecomeUserGroupAccount :: Kontrakcja m => UserGroupID -> m InternalKontraResponse
handleGetBecomeUserGroupAccount ugid = withUser $ \user -> do
  invite <- dbQuery $ GetUserGroupInvite ugid (userid user)
  case invite of
    Nothing -> do
      flashmessage <- flashMessageBecomeCompanyLogInDifferentUser
      return $ internalResponseWithFlash flashmessage LinkAccount
    _ -> do
      ug  <- guardJustM $ dbQuery $ UserGroupGet ugid
      ctx <- getContext
      internalResponse <$> (pageDoYouWantToBeCompanyAccount ctx ug)

handlePostBecomeUserGroupAccount :: Kontrakcja m => UserGroupID -> m InternalKontraResponse
handlePostBecomeUserGroupAccount ugid = withUser $ \user -> do
  void $ guardJustM $ dbQuery $ GetUserGroupInvite ugid (userid user)
  newug <- guardJustM $ dbQuery $ UserGroupGet ugid
  void $ dbUpdate $ SetUserCompanyAdmin (userid user) False
  void $ dbUpdate $ SetUserUserGroup (userid user) (get ugID newug)
  void $ dbUpdate $ RemoveUserGroupInvite ugid (userid user)
  -- if we are inviting a user with a plan to join the company, we
  -- should delete their personal plan
  flashmessage <- flashMessageUserHasBecomeCompanyAccount newug
  return $ internalResponseWithFlash flashmessage LinkAccount
