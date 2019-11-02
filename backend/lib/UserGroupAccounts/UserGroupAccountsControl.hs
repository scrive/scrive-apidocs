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

import Control.Monad.Extra (concatForM)
import Data.Char
import Text.JSON (JSValue(..))
import Text.JSON.Gen
import qualified Data.Text as T

import AccessControl.Model
import AccessControl.Types
import API.V2.Utils (accessControlLoggedIn)
import DB
import Folder.Model
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
handleUserGroupAccounts = withUserAndGroup $ \(user, _) -> do
  -- When a user has additional UserAdminAR role stored in database, return all the users,
  -- which he can administer.
  -- This intentionally uses UserAdminAR role instead of checking for (ReadA, UserR) permission,
  -- because we want to avoid huge list of users for "partner admins" (UserGroupAdminART).
  roles0 <- dbQuery . GetRoles $ user
  roles  <- addInheritedRoles roles0
  let userAdminUgId = \case
        UserAdminAR ugid -> Just ugid
        _                -> Nothing
  handleUserGroupAccountsInternal . mapMaybe userAdminUgId . fmap accessRoleTarget $ roles

-- | Get the ajax data for the company accounts list.
handleUserGroupAccountsForAdminOnly :: Kontrakcja m => UserGroupID -> m JSValue
handleUserGroupAccountsForAdminOnly ugid = onlySalesOrAdmin $ do
  handleUserGroupAccountsInternal [ugid]

-- | Create the JSON for either the admin only user or the logged in
-- company admin.
handleUserGroupAccountsInternal :: Kontrakcja m => [UserGroupID] -> m JSValue
handleUserGroupAccountsInternal ugids = do
  muser <- view #maybeUser <$> getContext
  user  <- case muser of
    Nothing   -> unexpectedError "handleUserGroupAccountsInternal: No user in Context!"
    Just user -> return user
  ugs          <- forM ugids $ guardJustM . dbQuery . UserGroupGet
  usersWithUgs <- concatForM ugs $ \ug -> do
    users <- dbQuery . UserGroupGetUsers $ ug ^. #id
    return . zip users $ repeat ug
  deletableuserids <- map userid <$> filterM isUserDeletable (map fst usersWithUgs)
  invitesWithUgs   <- concatForM ugs $ \ug -> do
    invites <- dbQuery . UserGroupGetInvitesWithUsersData $ ug ^. #id
    return . zip invites $ repeat ug
  let isUser ((invite, _, _, _), _) =
        (inviteduserid invite) `elem` map (userid . fst) usersWithUgs
  let companyaccounts = map mkAccountFromUser usersWithUgs
        <> map mkAccountFromInvite (filter (not . isUser) invitesWithUgs)
      mkAccountFromUser (u, ug) = CompanyAccount
        { camaybeuserid = userid u
        , cafullname    = getFullName u
        , cafstname     = userfstname $ userinfo u
        , casndname     = usersndname $ userinfo u
        , caemail       = getEmail u
        , caphone       = userphone $ userinfo u
        , cassn         = userpersonalnumber $ userinfo u
        , caposition    = usercompanyposition $ userinfo u
        , carole        = if (useriscompanyadmin u) then RoleAdmin else RoleStandard
        , cadeletable   = userid u `elem` deletableuserids
        , caactivated   = isJust $ userhasacceptedtermsofservice u
        , catos         = userhasacceptedtermsofservice u
        , catotpactive  = usertotpactive u
        , calang        = Just $ lang $ usersettings u
        , caugname      = ug ^. #name
        }
      mkAccountFromInvite ((i, fn, ln, em), ug) = CompanyAccount
        { camaybeuserid = inviteduserid i
        , cafullname    = fn <> " " <> ln
        , cafstname     = fn
        , casndname     = ln
        , caemail       = em
        , caphone       = ""
        , cassn         = ""
        , caposition    = ""
        , carole        = RoleInvite
        , cadeletable   = True
        , caactivated   = False
        , catos         = Nothing
        , catotpactive  = False
        , calang        = Nothing
        , caugname      = ug ^. #name
        }

  textFilter <- getField "text" >>= \case
    Nothing -> return $ const True
    Just s  -> return $ companyAccountsTextSearch s

  (sorting :: CompanyAccount -> CompanyAccount -> Ordering) <-
    getField "sorting" >>= \case
      Nothing -> return $ \_ _ -> EQ
      Just s  -> return $ companyAccountsSorting s

  sortingWithOrder <- getField "order" >>= \case
    Just "ascending" -> return sorting
    _                -> return $ flip sorting

  runJSONGenT $ do
    objects "accounts"
      $ for (sortBy sortingWithOrder $ filter textFilter companyaccounts)
      $ \f -> do
          value "id" $ show $ camaybeuserid f
          value "fullname" $ cafullname f
          value "fstname" $ cafstname f
          value "sndname" $ casndname f
          value "email" $ caemail f
          value "personalnumber" $ cassn f
          value "phone" $ caphone f
          value "companyposition" $ caposition f
          value "role" $ show $ carole f
          value "deletable" $ cadeletable f
          value "activated" $ caactivated f
          value "isctxuser" $ userid user == camaybeuserid f
          value "tos" $ formatTimeISO <$> (catos f)
          value "twofactor_active" $ catotpactive f
          value "lang" $ codeFromLang <$> calang f
          value "company_name" $ caugname f

{- |
    A special data type used for just displaying stuff in the list
    this lets make a unified list of users and pending invites
-}
data CompanyAccount = CompanyAccount
  { camaybeuserid :: UserID       -- ^ the account's or invites userid
  , cafullname    :: Text       -- ^ the account's or invites fullname
  , cafstname     :: Text       -- ^ the account's or invites fstname
  , casndname     :: Text       -- ^ the account's or invites sndname
  , caemail       :: Text       -- ^ the account's or invites email
  , caphone       :: Text       -- ^ the account's or invites phone (user account only)
  , cassn         :: Text       -- ^ the account's or invites ssn (user account only)
  , caposition    :: Text       -- ^ the account's or invites company position (user account only)
  , carole        :: Role         -- ^ the account's role (always Standard for invites)
  , cadeletable   :: Bool         -- ^ can the account be deleted, or do they have pending documents (always True for invites)?
  , caactivated   :: Bool         -- ^ is the account a full company user with accepted tos? (always False for invites)
  , catos         :: Maybe UTCTime -- ^ TOS time if any (always Nothing for invites)
  , catotpactive  :: Bool         -- ^ Whether TOTP two-factor authentication is active
  , calang        :: Maybe Lang         -- ^ the account's language
  , caugname      :: Text       -- ^ name of user's userGroup
  }

data Role = RoleAdmin    -- ^ an admin user
          | RoleStandard -- ^ a standard user
          | RoleInvite   -- ^ an invite for a user that is in different company
  deriving (Eq, Ord, Show)


companyAccountsTextSearch :: Text -> CompanyAccount -> Bool
companyAccountsTextSearch s ca = match (cafullname ca) || match (caemail ca)
  where match m = T.toUpper s `T.isInfixOf` T.toUpper m

companyAccountsSorting :: Text -> CompanyAccount -> CompanyAccount -> Ordering
companyAccountsSorting "fullname"         = companyAccountsSortingBy cafullname
companyAccountsSorting "email"            = companyAccountsSortingBy caemail
companyAccountsSorting "role"             = companyAccountsSortingBy carole
companyAccountsSorting "deletable"        = companyAccountsSortingBy cadeletable
companyAccountsSorting "activated"        = companyAccountsSortingBy caactivated
companyAccountsSorting "id"               = companyAccountsSortingBy camaybeuserid
companyAccountsSorting "twofactor_active" = companyAccountsSortingBy catotpactive
companyAccountsSorting "company_name"     = companyAccountsSortingBy caugname
companyAccountsSorting _                  = const $ const EQ

companyAccountsSortingBy
  :: Show a => (CompanyAccount -> a) -> CompanyAccount -> CompanyAccount -> Ordering
companyAccountsSortingBy f ca1 ca2 =
  compare (map toUpper $ show $ f ca1) (map toUpper $ show $ f ca2)

{- |
    Handles adding a company user either by creating them or
    by inviting them to be taken over.
-}
handleAddUserGroupAccount :: Kontrakcja m => m JSValue
handleAddUserGroupAccount = withUserAndGroup $ \(user, ug) -> do
  ctx     <- getContext
  email   <- guardJustM $ getOptionalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"

  mtrgug  <- getOptionalField asValidUserGroupID "user_group_id" >>= \case
    Nothing      -> return Nothing -- non-existing parameter is OK
    Just trgugid -> dbQuery (UserGroupGet trgugid) >>= \case
      Nothing    -> internalError -- non-existing UserGroup is not OK
      Just trgug -> return $ Just trgug
  let trgugid = fromMaybe ug mtrgug ^. #id
      acc     = mkAccPolicy [(CreateA, UserR, trgugid)]
  roles <- dbQuery . GetRoles $ user
  -- use internalError here, because that's what withCompanyAdmin uses
  accessControl roles acc internalError $ dbQuery (GetUserByEmail $ Email email) >>= \case
    Nothing -> do
      --create a new company user
      newuser' <- guardJustM $ createUser (Email email)
                                          (fstname, sndname)
                                          (trgugid, False)
                                          (ctx ^. #lang)
                                          CompanyInvitation
      void $ dbUpdate $ LogHistoryUserInfoChanged
        (userid newuser')
        (ctx ^. #ipAddr)
        (ctx ^. #time)
        (userinfo newuser')
        ((userinfo newuser') { userfstname = fstname, usersndname = sndname })
        (userid <$> ctx ^. #maybeUser)
      newuser <- guardJustM $ dbQuery $ GetUserByID (userid newuser')
      void $ sendNewUserGroupUserMail user ug newuser
      runJSONGenT $ value "added" True
    Just existinguser -> if (usergroupid existinguser == trgugid)
      then runJSONGenT $ do
        value "added"       False
        value "samecompany" True
      else do
                             -- If user exists we allow takeover only if he is the only user in his company
        users <- dbQuery . UserGroupGetUsers . usergroupid $ existinguser
        if (length users == 1)
          then do
            void $ sendTakeoverSingleUserMail user ug existinguser
            void $ dbUpdate $ AddUserGroupInvite $ UserGroupInvite (userid existinguser)
                                                                   trgugid
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
  newuser  <- guardJustM $ dbQuery $ GetUserByID resendid
  let ugid = ug ^. #id
  if (usergroupid newuser /= ugid)
    then do
       -- We need to check if there is a company invitation, and if it is, we send takeover email again
      void $ guardJustM $ dbQuery $ GetUserGroupInvite ugid resendid
      sendTakeoverSingleUserMail user ug newuser
    else do
       -- Else we just send an email
      sendNewUserGroupUserMail user ug newuser
  runJSONGenT $ value "resent" True

sendNewUserGroupUserMail :: Kontrakcja m => User -> UserGroup -> User -> m ()
sendNewUserGroupUserMail inviter ug user = do
  ctx <- getContext
  uar <- newUserAccountRequest $ userid user
  let al = LinkAccountCreated (lang $ usersettings user)
                              (uarUserID uar)
                              (uarToken uar)
                              CompanyInvitation
  mail <- mailNewUserGroupUserInvite ctx user inviter ug al (uarExpires uar)
  scheduleEmailSendout
    $ mail { to = [MailAddress { fullname = getFullName user, email = getEmail user }] }
  return ()

sendTakeoverSingleUserMail :: Kontrakcja m => User -> UserGroup -> User -> m ()
sendTakeoverSingleUserMail inviter ug user = do
  ctx  <- getContext
  mail <- mailTakeoverSingleUserInvite ctx user inviter ug (LinkCompanyTakeover $ ug ^. #id)
  scheduleEmailSendout $ mail { to = [getMailAddress user] }

{- |
    Handles a role change by switching a user from
    admin or to company.
-}
handleChangeRoleOfUserGroupAccount :: Kontrakcja m => m JSValue
handleChangeRoleOfUserGroupAccount = do
  changeid  <- getCriticalField asValidUserID "changeid"
  ugid      <- usergroupid <$> (guardJustM $ dbQuery . GetUserByID $ changeid)
  makeadmin <- getField "makeadmin"
  -- cf. `roleToAccessPolicyReq`
  let acc =
        [ mkAccPolicyItem (CreateA, UserPolicyR, changeid)
        , mkAccPolicyItem (CreateA, UserGroupPolicyR, ugid)
        ]
  accessControlLoggedIn acc $ do
    void $ dbUpdate $ SetUserCompanyAdmin changeid (makeadmin == Just "true")
    runJSONGenT $ value "changed" True

{- |
    Handles deletion of a company user or the deletion of the company invite
    if they haven't yet accepted.
-}
handleRemoveUserGroupAccount :: Kontrakcja m => m JSValue
handleRemoveUserGroupAccount = withUserAndRoles $ \(user, roles) -> do
  removeuid  <- getCriticalField asValidUserID "removeid"
  removeuser <- guardJustM $ dbQuery $ GetUserByID $ removeuid
  let acc = [mkAccPolicyItem (DeleteA, UserR, usergroupid removeuser)]
  -- Even if we don't execute the main action for whatever reason we remove all invites
  -- that we possibly can, restricted by the caller's permissions.
  accessControl roles acc (removeInvitesOnly roles removeuser) $ do
    isdeletable <- isUserDeletable removeuser
    ctx         <- getContext
    case isdeletable of
      True -> do
              -- We remove user, so we also want to drop all invites - they should be invalid at this point anyway.
        void $ dbUpdate $ RemoveUserUserGroupInvites (userid removeuser)
        void $ dbUpdate $ DeleteUserCallbackScheme $ userid removeuser
        void $ dbUpdate $ DeleteUser (userid removeuser)
        void $ dbUpdate $ LogHistoryAccountDeleted (userid removeuser)
                                                   (userid user)
                                                   (ctx ^. #ipAddr)
                                                   (ctx ^. #time)
        runJSONGenT $ value "removed" True
      _ -> do
        runJSONGenT $ value "removed" False
  where
    removeInvitesOnly :: Kontrakcja m => [AccessRole] -> User -> m JSValue
    removeInvitesOnly roles removeForUser = do
      -- get all user groups for which delete is allowed
      allRoles <- addInheritedRoles roles
      let allPerms = join $ map (hasPermissions . accessRoleTarget) allRoles
          deleteUserPerms =
            filter (\p -> (p `hasAction` DeleteA) && (p `hasResource` UserR)) allPerms
          ugids = catMaybes $ map extractResourceRef deleteUserPerms
        -- We remove only the invites, possibly making user account in other some user
        -- group still be valid.
      void $ dbUpdate $ RemoveUserGroupInvite ugids (userid removeForUser)
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
      internalResponse <$> T.unpack <$> (pageDoYouWantToBeCompanyAccount ctx ug)

handlePostBecomeUserGroupAccount
  :: Kontrakcja m => UserGroupID -> m InternalKontraResponse
handlePostBecomeUserGroupAccount ugid = withUser $ \user -> do
  void $ guardJustM $ dbQuery $ GetUserGroupInvite ugid (userid user)
  newug <- guardJustM $ dbQuery $ UserGroupGet ugid
  (folderID <$>) <$> (dbQuery $ FolderGetUserGroupHome ugid) >>= \case
    Nothing         -> internalError
    Just newugfdrid -> do
      let uid = userid user
      void $ dbUpdate $ SetUserCompanyAdmin (userid user) False
      void $ dbUpdate $ SetUserUserGroup (userid user) (newug ^. #id)
      let newhomefdr = set #folderParentID (Just newugfdrid) defaultFolder
      newhomefdrid <- folderID <$> (dbUpdate $ FolderCreate newhomefdr)
      void $ dbUpdate . SetUserHomeFolder uid $ newhomefdrid
      void $ dbUpdate $ RemoveUserGroupInvite [ugid] (userid user)
      -- if we are inviting a user with a plan to join the company, we
      -- should delete their personal plan
      flashmessage <- flashMessageUserHasBecomeCompanyAccount newug
      return $ internalResponseWithFlash flashmessage LinkAccount
