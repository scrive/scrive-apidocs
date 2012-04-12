module Company.CompanyControl (
    handleGetCompany
  , handlePostCompany
  , handleGetCompanyJSON
  , handleCompanyLogo

  , withCompanyAdmin
  ) where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP)
import Text.JSON
import Text.JSON.String
import Text.JSON.Types
import Util.JSON
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map

import DB.Classes
import DB.Types
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
import qualified Log

handleGetCompany :: Kontrakcja m => m String
handleGetCompany = withCompanyUser $ \_ -> viewCompanySettings

handlePostCompany :: Kontrakcja m => m KontraLink
handlePostCompany = withCompanyAdmin $ \(_user, company) -> do
  iscompanyjson <- isFieldSet "company"
  cui' <-
    if iscompanyjson
      then do
        rawcompanyjson <- guardJustM $ getField "company"
        companyjson <- guardRight $ runGetJSON readJSValue rawcompanyjson
        jsoncui <- guardRight $ companyUiFromJSON companyjson
        Log.debug $ "using json " ++ (show $ jsoncui)
        return $ jsoncui{ companylogo = companylogo $ companyui company }
      else
        return $ companyui company
  cui <- setCompanyLogoFromRequest cui'
  Log.debug $ "company UI " ++ (show $ companyid company) ++ " updated to " ++ (show cui)
  _ <- dbUpdate $ UpdateCompanyUI (companyid company) cui
  return LinkAccountCompany

setCompanyLogoFromRequest :: Kontrakcja m => CompanyUI -> m CompanyUI
setCompanyLogoFromRequest cui = do
  mlogo <- fmap Binary <$> getFileField "logo"
  mislogo <- getField "islogo"
  case (mislogo, mlogo) of
    -- islogo = False so if there is a stored logo remove it
    (Just "false", _) -> do
      return cui{ companylogo = Nothing }
    -- they uploaded a logo so store it
    (_, Just logo) -> do
      return cui{ companylogo = Just logo }
    -- just keep the logo however it currently is
    _ -> do
      return cui

companyUiFromJSON :: JSValue -> Either String CompanyUI
companyUiFromJSON jsv = do
  jsonbb <- jsget "barsbackground" jsv
  jsonbtc <- jsget "barstextcolour" jsv
  return CompanyUI {
    companybarsbackground = maybeS jsonbb
  , companybarstextcolour = maybeS jsonbtc
  , companylogo = Nothing
  }
  where
    maybeS (JSString (JSONString val)) | not (null val) = Just val
    maybeS _ = Nothing

handleCompanyLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanyLogo cid = do
  mimg <- join <$> fmap (companylogo . companyui) <$> (dbQuery $ GetCompany cid)
  return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $
    Response 200 Map.empty nullRsFlags (BSL.fromChunks $ map unBinary $ maybeToList mimg) Nothing

handleGetCompanyJSON :: Kontrakcja m => m JSValue
handleGetCompanyJSON = withCompanyUser $ \(user, company) ->
  return $ companyJSON company (useriscompanyadmin user)

companyJSON :: Company -> Bool -> JSValue
companyJSON company editable =
  JSObject $ toJSObject
               [ ("company",
                   JSObject $ toJSObject [
                     ("id", JSString $ toJSString $ show $ companyid company)
                   , ("name", JSString $ toJSString $ getCompanyName company)
                   , ("number", JSString $ toJSString $ getCompanyNumber company)
                   , ("address", JSString $ toJSString $ companyaddress $ companyinfo $ company)
                   , ("zip", JSString $ toJSString $ companyzip $ companyinfo $ company)
                   , ("city", JSString $ toJSString $ companycity $ companyinfo $ company)
                   , ("country", JSString $ toJSString $ companycountry $ companyinfo $ company)
                   , ("barsbackground", JSString $ toJSString $ fromMaybe "" $ companybarsbackground $ companyui $ company)
                   , ("barstextcolour", JSString $ toJSString $ fromMaybe "" $ companybarstextcolour $ companyui $ company)
                   , ("logo", JSString $ toJSString $ maybe "" (const $ show $ LinkCompanyLogo $ companyid company) $ companylogo $ companyui $ company)
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
    Guards that there is a logged in company admin.
-}
withCompanyAdmin :: Kontrakcja m => ((User, Company) -> m a) -> m a
withCompanyAdmin action = withCompanyUser $ \(user, company) ->
  if useriscompanyadmin user then action (user, company) else internalError
