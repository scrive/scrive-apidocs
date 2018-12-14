module User.Utils (
      guardLoggedInOrThrowInternalError
    , withUserTOS
    , withUser
    , withUserAndGroup
    , withCompanyAdmin
    , withCompanyAdminOrAdminOnly
) where

import Data.Time.Clock (UTCTime)

import DB
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
    case get ctxmaybeuser ctx of
      Just user -> action user
      Nothing   -> do
       flashmessage <- flashMessageLoginRedirect
       return $ internalResponseWithFlash flashmessage
         (LinkLogin (get ctxlang ctx))

{- |
  Guard against a GET/POST with no logged in user.
  If they are not logged in, return an internal error with proper err code
-}
guardLoggedInOrThrowInternalError :: Kontrakcja m => m a -> m a
guardLoggedInOrThrowInternalError action = do
   ctx <- getContext
   case get ctxmaybeuser ctx of
     Just _user -> action
     Nothing    -> internalError

{- |
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
withUserTOS :: Kontrakcja m => ((User, UTCTime) -> m InternalKontraResponse) -> m InternalKontraResponse
withUserTOS action = withUser $ \user -> do
  case userhasacceptedtermsofservice user of
    Just tosaccepttime -> action (user, tosaccepttime)
    Nothing -> return $ internalResponse (LinkAcceptTOS)

{- |
    Guards that there is a user that is logged in and they
    are in a company.  The user and company are passed as params
    to the given action, to save you having to look them up yourself.
-}
withUserAndGroup :: Kontrakcja m => ((User, UserGroup) -> m a) -> m a
withUserAndGroup action = do
  maybeuser <- get ctxmaybeuser <$> getContext
  user      <- guardJust maybeuser
  ug        <- dbQuery . UserGroupGetByUserID . userid $ user
  action (user, ug)

{- |
    Guards that there is a logged in company admin.
-}
withCompanyAdmin :: Kontrakcja m => ((User, UserGroup) -> m a) -> m a
withCompanyAdmin action = withUserAndGroup $ \(user, ug) ->
  if useriscompanyadmin user then action (user, ug) else internalError

withCompanyAdminOrAdminOnly :: Kontrakcja m => Maybe UserGroupID -> (UserGroup -> m a) -> m a
withCompanyAdminOrAdminOnly Nothing action = withCompanyAdmin (action . snd)
withCompanyAdminOrAdminOnly (Just ugid) action = onlySalesOrAdmin $
  guardJustM (dbQuery (UserGroupGet ugid)) >>= action
