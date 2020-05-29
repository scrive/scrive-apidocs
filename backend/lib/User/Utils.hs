module User.Utils (
      guardLoggedInOrThrowInternalError
    , withTosCheck
    , with2FACheck
    , withSalesOrAdminOnly
    , withUser
    , withUserAndGroup
    , withUserAndGroupWithParents
    , withUserAndRoles
    , withUserOrAdminOnly
    , withCompanyAdmin
    , withCompanyAdminOrAdminOnly
    , moveUserToUserGroupWithDocuments
) where

import AccessControl.Model
import AccessControl.Types (AccessRole)
import DB
import Folder.Model
import InternalResponse
import Kontra
import KontraLink
import User.Model
import User.UserView
import UserGroup.Model
import UserGroup.Types
import Util.MonadUtils

{- |
   Guard against a GET/POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUser :: Kontrakcja m => (User -> m InternalKontraResponse) -> m InternalKontraResponse
withUser action = do
  ctx <- getContext
  case ctx ^. #maybeUser of
    Just user -> action user
    Nothing   -> do
      flashmessage <- flashMessageLoginRedirect
      return $ internalResponseWithFlash flashmessage (LinkLogin (ctx ^. #lang))

{- |
  Guard against a GET/POST with no logged in user.
  If they are not logged in, return an internal error with proper err code
-}
guardLoggedInOrThrowInternalError :: Kontrakcja m => m a -> m a
guardLoggedInOrThrowInternalError action = do
  ctx <- getContext
  case ctx ^. #maybeUser of
    Just _user -> action
    Nothing    -> internalError

{- |
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
withTosCheck
  :: Kontrakcja m
  => (User -> m InternalKontraResponse)
  -> User
  -> m InternalKontraResponse
withTosCheck action user = case user ^. #hasAcceptedTOS of
  Just _  -> action user
  Nothing -> return $ internalResponse LinkAcceptTOS

{- |
   Guard against a GET with logged in users who should have 2FA enabled, but they have not.
   If they have not, redirect to their account page.
-}
with2FACheck
  :: Kontrakcja m
  => (User -> m InternalKontraResponse)
  -> User
  -> m InternalKontraResponse
with2FACheck action user = do
  ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
  let userMustHaveTotpEnabled =
        (ugwpSettings ugwp ^. #totpIsMandatory) || user ^. #totpIsMandatory
  if userMustHaveTotpEnabled && not (user ^. #totpActive)
    then do
      flashmessage <- flashMessageTotpMustBeActivated
      return $ internalResponseWithFlash flashmessage LinkAccount
    else action user

{- |
    Guards that there is a user that is logged in and they
    are in a user group.  The user and user group are passed as params
    to the given action, to save you having to look them up yourself.
-}
withUserAndGroup :: Kontrakcja m => ((User, UserGroup) -> m a) -> m a
withUserAndGroup action = do
  maybeuser <- view #maybeUser <$> getContext
  user      <- guardJust maybeuser
  ug        <- dbQuery . UserGroupGetByUserID $ user ^. #id
  action (user, ug)

{- |
    Guards that there is a user that is logged in and they are in a user group.
    The user and user group (with parents) are passed as params to the given
    action, to save you having to look them up yourself.
-}
withUserAndGroupWithParents
  :: Kontrakcja m => ((User, UserGroupWithParents) -> m a) -> m a
withUserAndGroupWithParents action = do
  maybeuser <- view #maybeUser <$> getContext
  user      <- guardJust maybeuser
  ugwp      <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
  action (user, ugwp)

{- |
    Guards that there is a user that is logged in. The user and roles are passed as params
    to the given action, to save you having to look them up yourself.
-}
withUserAndRoles :: Kontrakcja m => ((User, [AccessRole]) -> m a) -> m a
withUserAndRoles action = do
  muser <- view #maybeUser <$> getContext
  user  <- guardJust muser
  roles <- dbQuery $ GetRoles user
  action (user, roles)

{- |
    Guards that there is a logged in company admin.
-}
withCompanyAdmin :: Kontrakcja m => ((User, UserGroup) -> m a) -> m a
withCompanyAdmin action = withUserAndGroup
  $ \(user, ug) -> if user ^. #isCompanyAdmin then action (user, ug) else internalError

withCompanyAdminOrAdminOnly
  :: Kontrakcja m => Maybe UserGroupID -> (UserGroup -> m a) -> m a
withCompanyAdminOrAdminOnly Nothing action = withCompanyAdmin (action . snd)
withCompanyAdminOrAdminOnly (Just ugid) action =
  onlySalesOrAdmin $ guardJustM (dbQuery (UserGroupGet ugid)) >>= action

withUserOrAdminOnly :: Kontrakcja m => Maybe UserGroupID -> (UserGroup -> m a) -> m a
withUserOrAdminOnly Nothing action = withUserAndGroup (action . snd)
withUserOrAdminOnly (Just ugid) action =
  onlySalesOrAdmin $ guardJustM (dbQuery (UserGroupGet ugid)) >>= action

withSalesOrAdminOnly :: Kontrakcja m => UserGroupID -> (UserGroup -> m a) -> m a
withSalesOrAdminOnly ugid action =
  onlySalesOrAdmin $ guardJustM (dbQuery (UserGroupGet ugid)) >>= action

-- [CORE-2185] Workaround to automatically move a user's authored documents in their
-- old home folder when moving them to a new home folder. In the long term the
-- front end should have proper UI for moving users with their documents.
moveUserToUserGroupWithDocuments :: Kontrakcja m => User -> UserGroupID -> m ()
moveUserToUserGroupWithDocuments user targetUserGroupId = do
  targetFolderId <- moveUserToUserGroup (user ^. #id) targetUserGroupId
  case user ^. #homeFolderID of
    Just sourceFolderId ->
      dbUpdate $ MoveAuthorDocuments (user ^. #id) sourceFolderId targetFolderId
    Nothing -> return ()

moveUserToUserGroup :: Kontrakcja m => UserID -> UserGroupID -> m FolderID
moveUserToUserGroup uid newugid =
  (view #id <$>) <$> dbQuery (FolderGetUserGroupHome newugid) >>= \case
    Nothing         -> internalError
    Just newugfdrid -> do
      void . dbUpdate $ SetUserUserGroup uid newugid
      void . dbUpdate $ SetUserCompanyAdmin uid False
      let newhomefdr = set #parentID (Just newugfdrid) defaultFolder
      newhomefdrid <- view #id <$> dbUpdate (FolderCreate newhomefdr)
      void . dbUpdate . SetUserHomeFolder uid $ newhomefdrid
      return newhomefdrid
