module Company.CompanyControl (
    handleGetCompany
  , handlePostCompany
  , handleGetCompanyJSON

  , withCompanyAdmin
  ) where

import Control.Monad.State
import Happstack.Server hiding (simpleHTTP)
import Text.JSON
import Text.JSON.String
import Text.JSON.Types
import Util.JSON
import qualified Data.ByteString.UTF8 as BS

import AppView
import DB.Classes
import Company.CompanyView
import Company.Model
import Kontra
import KontraLink
import Misc
import Redirect
import User.Model
import User.Utils
import Util.HasSomeCompanyInfo
import Util.MonadUtils

handleGetCompany :: Kontrakcja m => m Response
handleGetCompany = withCompanyUser $ \(_user, company) -> do
  content <- viewCompanySettings company
  renderFromBody TopAccount kontrakcja content

handlePostCompany :: Kontrakcja m => m KontraLink
handlePostCompany = withCompanyAdmin $ \(_user, company) -> do
  rawcompanyjson <- guardJustM $ getField "company"
  companyjson <- guardRight $ runGetJSON readJSValue rawcompanyjson
  cui <- guardRight $ companyUiFromJSON companyjson
  _ <- runDBUpdate $ UpdateCompanyUI (companyid company) cui
  return LinkAccountCompany

companyUiFromJSON :: JSValue -> Either String CompanyUI
companyUiFromJSON jsv = do
  JSString (JSONString barsbackground) <- jsget "barsbackground" jsv
  return CompanyUI {
    companybarsbackground = if barsbackground==""
                               then Nothing
                               else Just (BS.fromString barsbackground)
  , companylogo = Nothing
  }

handleGetCompanyJSON :: Kontrakcja m => m JSValue
handleGetCompanyJSON = withCompanyUser $ \(user, company) ->
  return $ companyJSON company (useriscompanyadmin user)

companyJSON :: Company -> Bool -> JSValue
companyJSON company editable =
  JSObject $ toJSObject
               [ ("company",
                   JSObject $ toJSObject [
                     ("id", JSString $ toJSString $ show $ companyid company)
                   , ("name", JSString $ toJSString $ BS.toString $  getCompanyName company)
                   , ("number", JSString $ toJSString $ BS.toString $  getCompanyNumber company)
                   , ("address", JSString $ toJSString $ BS.toString $  companyaddress $ companyinfo $ company)
                   , ("zip", JSString $ toJSString $ BS.toString $  companyzip $ companyinfo $ company)
                   , ("city", JSString $ toJSString $ BS.toString $  companycity $ companyinfo $ company)
                   , ("country", JSString $ toJSString $ BS.toString $  companycountry $ companyinfo $ company)
                   , ("barsbackground", JSString $ toJSString $ maybe "" BS.toString $ companybarsbackground $ companyui $ company)
                   , ("editable", JSBool $ editable)
                 ])
               ]
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
    Guards that there is a user that is logged in and is the admin
    of a company.  The user and company are passed as params to the
    given action, to save you having to look them up yourself.
-}
withCompanyAdmin :: Kontrakcja m => ((User, Company) -> m a) -> m a
withCompanyAdmin action = withCompanyUser $ \(user, company) ->
  if useriscompanyadmin user then action (user, company) else mzero
