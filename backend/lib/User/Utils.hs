module User.Utils (
      getCompanyForUser
    , withUserTOS
    , withUser
    , withUserCompany
    , withCompanyAdmin
    , withCompanyAdminOrAdminOnly
    , Redir
)where

import Control.Monad.Catch
import Data.Functor
import Data.Time.Clock (UTCTime)

import Company.Model
import DB
import FlashMessage
import Kontra
import KontraLink
import KontraPrelude
import User.Model
import User.UserView
import Util.MonadUtils

type Redir a = Either (Maybe FlashMessage, KontraLink) a

{- |
    This looks up the company for the given user, if the user doesn't
    have a company then it returns Nothing.
-}
getCompanyForUser :: (MonadDB m, MonadThrow m) => User -> m Company
getCompanyForUser user = dbQuery $ GetCompanyByUserID $ userid user

{- |
   Guard against a GET/POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUser :: Kontrakcja m => (User -> m a) -> m (Redir a)
withUser action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
      Just user -> Right <$> action user
      Nothing   -> do
       flashmessage <- flashMessageLoginRedirect
       return $ Left (Just flashmessage, LinkLogin (ctxlang ctx))

{- |
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
withUserTOS :: Kontrakcja m => ((User, UTCTime) -> m a) -> m (Redir a)
withUserTOS action = (join <$>) $ withUser $ \user -> do
  case userhasacceptedtermsofservice user of
    Just tosaccepttime -> Right <$> action (user, tosaccepttime)
    Nothing -> return $ Left (Nothing, LinkAcceptTOS)

{- |
    Guards that there is a user that is logged in and they
    are in a company.  The user and company are passed as params
    to the given action, to save you having to look them up yourself.
-}
withUserCompany :: Kontrakcja m => ((User, Company) -> m a) -> m a
withUserCompany action = do
  Context{ ctxmaybeuser } <- getContext
  user <- guardJust ctxmaybeuser
  company <- getCompanyForUser user
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
