module User.Utils (
      getCompanyForUser
    , checkUserTOSGet
    , withUser
    , withCompanyUser
    , withCompanyAdmin
    , withCompanyAdminOrAdminOnly
)where

import Control.Monad.Catch
import Data.Functor

import Company.Model
import DB
import Kontra
import KontraLink
import KontraPrelude
import User.Model
import Util.MonadUtils

{- |
    This looks up the company for the given user, if the user doesn't
    have a company then it returns Nothing.
-}
getCompanyForUser :: (MonadDB m, MonadThrow m) => User -> m Company
getCompanyForUser user = dbQuery $ GetCompanyByUserID $ userid user

{- |
   Guard against a POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUser :: Kontrakcja m => m a -> m (Either (FlashMessage, KontraLink) a)
withUser action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
         Just _  -> Right <$> action
         Nothing ->
           flashmessage <- flashMessageLoginRedirect
           return $ Left (flashmessage, LinkLogin (ctxlang ctx))

{- |
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
checkUserTOSGet :: Kontrakcja m => m a -> m (Either KontraLink a)
checkUserTOSGet action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
        Just (User{userhasacceptedtermsofservice = Just _}) -> Right <$> action
        Just _ -> return $ Left $ LinkAcceptTOS
        Nothing -> return $ Left $ LinkLogin (ctxlang ctx) NotLogged

{- |
    Guards that there is a user that is logged in and they
    are in a company.  The user and company are passed as params
    to the given action, to save you having to look them up yourself.
-}
withCompanyUser :: Kontrakcja m => ((User, Company) -> m a) -> m a
withCompanyUser action = do
  Context{ ctxmaybeuser } <- getContext
  user <- guardJust ctxmaybeuser
  company <- getCompanyForUser user
  action (user, company)

{- |
    Guards that there is a logged in company admin.
-}
withCompanyAdmin :: Kontrakcja m => ((User, Company) -> m a) -> m a
withCompanyAdmin action = withCompanyUser $ \(user, company) ->
  if useriscompanyadmin user then action (user, company) else internalError

withCompanyAdminOrAdminOnly :: Kontrakcja m => Maybe CompanyID -> (Company -> m a) -> m a
withCompanyAdminOrAdminOnly Nothing action = withCompanyAdmin (action . snd)
withCompanyAdminOrAdminOnly (Just cid) action = onlySalesOrAdmin $
  guardJustM (dbQuery (GetCompany cid)) >>= action
