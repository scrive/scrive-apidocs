module Company.CompanyControl (
    handlePostCompany
  , handleGetCompanyJSON
  , handleCompanyLogo
  , routes
  , adminRoutes
  , withCompanyAdmin
  , handleSerializeImage
  ) where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (dir, simpleHTTP)
import Happstack.StaticRouting (Route, dir, choice)
import Text.JSON
import Text.JSON.String
import Text.JSON.FromJSValue
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as Map

import DB
import Administration.AdministrationView (adminCompanyBrandingPage)
import Company.Model
import Kontra
import KontraLink
import Happstack.Fields
import Redirect
import Routing (hGet, hPost, toK0, toK1)
import User.Utils
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

handleSerializeImage :: Kontrakcja m => m JSValue
handleSerializeImage = do
  guardLoggedIn
  logo <- guardJustM $ getFileField "logo"
  runJSONGenT $ value "logo_base64" $ showJSON $ B64.encode logo

handlePostCompany :: Kontrakcja m => Maybe CompanyID -> m KontraLink
handlePostCompany mcid = withCompanyAdminOrAdminOnly mcid $ \company -> do
  iscompanyjson <- isFieldSet "company"
  cui <-
    if iscompanyjson
      then do
        rawcompanyjson <- guardJustM $ getField "company"
        companyjson <- guardRight $ runGetJSON readJSValue rawcompanyjson
        jsoncui <- guardRight $ companyUiFromJSON companyjson $ companyui company
        Log.debug $ "using json " ++ (show $ jsoncui)
        return jsoncui
      else
        return $ companyui company
  Log.debug $ "company UI " ++ (show $ companyid company) ++ " updated to " ++ (show cui)
  _ <- dbUpdate $ UpdateCompanyUI (companyid company) cui
  return $ LinkAccountCompany mcid

companyUiFromJSON :: JSValue -> CompanyUI -> Either String CompanyUI
companyUiFromJSON jsv cui = maybe (Left "Unable to parse JSON!") Right $ do
  jsonbb <- fromJSValueField "barsbackground" jsv
  jsonbtc <- fromJSValueField "barstextcolour" jsv
  jsonlogochanged <- fromJSValueField "logochanged" jsv
  jsonlogo <- fromJSValueField "logo" jsv
  let logo = if jsonlogochanged then
                 fmap (Binary . B64.decodeLenient) $ maybeS jsonlogo
             else
                 companylogo cui
  return CompanyUI {
    companybarsbackground = maybeS jsonbb
  , companybarstextcolour = maybeS jsonbtc
  , companylogo = logo
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
    value "barsbackground" $ fromMaybe "" $ companybarsbackground $ companyui $ company
    value "barstextcolour" $ fromMaybe "" $ companybarstextcolour $ companyui $ company
    value "logo" $ maybe "" (const $ show $ LinkCompanyLogo $ companyid company) $ companylogo $ companyui $ company
    value "editable" editable
