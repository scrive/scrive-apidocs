module User.Utils (
      getCompanyForUser
    , guardLoggedInOrThrowInternalError
    , withUserTOS
    , withUser
    , withUserCompany
    , withCompanyAdmin
    , withCompanyAdminOrAdminOnly
) where

import Control.Monad.Catch
import Data.Time.Clock (UTCTime)

import Company.Model
import DB
import InternalResponse
import Kontra
import KontraLink
import KontraPrelude
import User.Model
import User.UserView
import Util.MonadUtils

{- |
    This looks up the company for the given user.
-}
getCompanyForUser :: (MonadDB m, MonadThrow m) => User -> m Company
getCompanyForUser user = dbQuery $ GetCompanyByUserID $ userid user

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
withUserCompany :: Kontrakcja m => ((User, Company) -> m a) -> m a
withUserCompany action = do
  maybeuser <- get ctxmaybeuser <$> getContext
  user      <- guardJust maybeuser
  company   <- getCompanyForUser user
  action (user, company)

{- |
    Guards that there is a logged in company admin.
-}
withCompanyAdmin :: Kontrakcja m => ((User, Company) -> m a) -> m a
withCompanyAdmin action = withUserCompany $ \(user, company) ->
  if useriscompanyadmin user then action (user, company) else internalError

withCompanyAdminOrAdminOnly :: Kontrakcja m => Maybe CompanyID -> (Company -> m a) -> m a
withCompanyAdminOrAdminOnly Nothing action = withCompanyAdmin (action . snd)
withCompanyAdminOrAdminOnly (Just cid) action = onlySalesOrAdmin $
  guardJustM (dbQuery (GetCompany cid)) >>= action
