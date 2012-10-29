module Company.CompanyControl (
    handlePostCompany
  , handleGetCompanyJSON
  , handleCompanyLogo
  , routes
  , adminRoutes
  , withCompanyAdmin
  ) where

import Control.Monad.State
import Control.Arrow (first)
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (dir, simpleHTTP)
import Happstack.StaticRouting (Route, dir, choice)
import Text.JSON
import Text.JSON.String
import Text.JSON.FromJSValue
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map

import DB
import Administration.AdministrationView (adminCompanyBrandingPage)
import Company.Model
import Kontra
import KontraLink
import Happstack.Fields
import Redirect
import Routing (hGet, hPost, toK0, toK1)
import User.Model
import User.Utils
import Util.HasSomeCompanyInfo
import Util.MonadUtils
import qualified Log
import Text.JSON.Gen 

routes :: Route (KontraPlus Response)
routes = choice
  [ hPost $ toK0 $ handlePostCompany Nothing
  , dir "json" $ hGet $ toK0 $ handleGetCompanyJSON Nothing
  , hGet $ toK1 $ handleCompanyLogo
  ]

adminRoutes :: Route (KontraPlus Response)
adminRoutes = choice
  [ hGet $ toK1 $ handleAdminGetCompany
  , hPost $ toK1 $ handlePostCompany . Just
  , dir "json" $ hGet $ toK1 $ handleGetCompanyJSON . Just
  ]

handleAdminGetCompany :: Kontrakcja m => CompanyID -> m String
handleAdminGetCompany cid = withCompanyAdminOrAdminOnly (Just cid) $
  const $ adminCompanyBrandingPage cid

handlePostCompany :: Kontrakcja m => Maybe CompanyID -> m KontraLink
handlePostCompany mcid = withCompanyAdminOrAdminOnly mcid $ \company -> do
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
  return $ LinkAccountCompany mcid

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
companyUiFromJSON jsv = maybe (Left "Unable to parse JSON!") Right $ do
  jsonbb <- fromJSValueField "barsbackground" jsv
  jsonbtc <- fromJSValueField "barstextcolour" jsv
  return CompanyUI {
    companybarsbackground = maybeS jsonbb
  , companybarstextcolour = maybeS jsonbtc
  , companylogo = Nothing
  }
  where
    maybeS ""  = Nothing
    maybeS str = Just str

handleCompanyLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanyLogo cid = do
  mimg <- join <$> fmap (companylogo . companyui) <$> (dbQuery $ GetCompany cid)
  return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $
    Response 200 Map.empty nullRsFlags (BSL.fromChunks $ map unBinary $ maybeToList mimg) Nothing

handleGetCompanyJSON :: Kontrakcja m => Maybe CompanyID -> m JSValue
handleGetCompanyJSON mcid = withCompanyUserOrAdminOnly mcid $ \(editable, company) -> runJSONGenT $ do
  object "company" $ do
    value "id" $ show $ companyid company
    value "name" $ getCompanyName company
    value "number" $ getCompanyNumber company
    value "address" $ companyaddress $ companyinfo $ company
    value "zip" $ companyzip $ companyinfo $ company
    value "city" $ companycity $ companyinfo $ company
    value "country" $ companycountry $ companyinfo $ company
    value "barsbackground" $ fromMaybe "" $ companybarsbackground $ companyui $ company
    value "barstextcolour" $ fromMaybe "" $ companybarstextcolour $ companyui $ company
    value "logo" $ maybe "" (const $ show $ LinkCompanyLogo $ companyid company) $ companylogo $ companyui $ company
    value "editable" editable

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
