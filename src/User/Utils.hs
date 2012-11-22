module User.Utils where

import Control.Monad.State
import Control.Arrow (first)
import Data.Functor

import DB
import Doc.DocStateData
import Company.Model
import Kontra
import KontraLink
import User.Model
import Util.SignatoryLinkUtils
import Util.MonadUtils

{- |
    This looks up the company for the given user, if the user doesn't
    have a company then it returns Nothing.
-}
getCompanyForUser :: MonadDB m => User -> m (Maybe Company)
getCompanyForUser = maybe (return Nothing) (dbQuery . GetCompany) . usercompany

{- |
   Guard against a POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserPost :: Kontrakcja m => m KontraLink -> m KontraLink
withUserPost action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
         Just _  -> action
         Nothing -> return $ LinkLogin (ctxlang ctx) NotLogged

{- |
   Guard against a GET with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserGet :: Kontrakcja m => m a -> m (Either KontraLink a)
withUserGet action = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Just _  -> Right <$> action
    Nothing -> return $ Left $ LinkLogin (ctxlang ctx) NotLogged

{- |
     Takes a document and a action
     Runs an action only if current user (from context) is author of document
| -}
withDocumentAuthor :: Kontrakcja m => Document -> m a -> m a
withDocumentAuthor document action = do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  sl <- guardJust $ getAuthorSigLink document
  unless (isSigLinkFor user sl) internalError
  action

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
  company <- guardJustM $ getCompanyForUser user
  action (user, company)

{- |
    Guards that there is a logged in company admin.
-}
withCompanyAdmin :: Kontrakcja m => ((User, Company) -> m a) -> m a
withCompanyAdmin action = withCompanyUser $ \(user, company) ->
  if useriscompanyadmin user then action (user, company) else internalError


withCompanyUserOrAdminOnly :: Kontrakcja m => Maybe CompanyID -> ((Bool, Company) -> m a) -> m a
withCompanyUserOrAdminOnly Nothing action = withCompanyUser (action . first useriscompanyadmin)
withCompanyUserOrAdminOnly (Just cid) action = onlySalesOrAdmin $
  guardJustM (dbQuery (GetCompany cid)) >>= curry action True

withCompanyAdminOrAdminOnly :: Kontrakcja m => Maybe CompanyID -> (Company -> m a) -> m a
withCompanyAdminOrAdminOnly Nothing action = withCompanyAdmin (action . snd)
withCompanyAdminOrAdminOnly (Just cid) action = onlySalesOrAdmin $
  guardJustM (dbQuery (GetCompany cid)) >>= action