module Company.CompanyControl (
    handlePostCompany
  , handleGetCompanyJSON
  , handleCompanySignViewLogo
  , handleCompanyEmailLogo
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
  , dir "signview" $ hGet $ toK1 $ handleCompanySignViewLogo
  , dir "custom" $ hGet $ toK1 $ handleCompanyCustomLogo
  , dir "email" $ hGet $ toK1 $ handleCompanyEmailLogo
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
        jsoncui <- companyUiFromJSON companyjson
        Log.debug $ "using json " ++ (show $ jsoncui)
        return jsoncui
      else
        return $ companyui company
  Log.debug $ "company UI " ++ (show $ companyid company) ++ " updated to " ++ (show cui)
  _ <- dbUpdate $ UpdateCompanyUI (companyid company) cui
  return $ LinkAccountCompany mcid

companyUiFromJSON :: Kontrakcja m => JSValue ->  m CompanyUI
companyUiFromJSON jsv = withJSValue jsv $ do
  jsoncompanyemailfont <- fromJSValueField "companyemailfont"
  jsoncompanyemailbordercolour <- fromJSValueField "companyemailbordercolour"
  jsoncompanyemailbuttoncolour <- fromJSValueField "companyemailbuttoncolour"
  jsoncompanyemailemailbackgroundcolour <- fromJSValueField "companyemailemailbackgroundcolour"
  jsoncompanyemailbackgroundcolour <- fromJSValueField "companyemailbackgroundcolour"
  jsoncompanyemailtextcolour <- fromJSValueField "companyemailtextcolour"
  jsoncompanyemaillogo <- fromJSValueField "companyemaillogo"
  jsoncompanysignviewlogo <- fromJSValueField "companysignviewlogo"
  jsoncompanysignviewtextcolour <- fromJSValueField "companysignviewtextcolour"
  jsoncompanysignviewtextfont <- fromJSValueField "companysignviewtextfont"
  jsoncompanysignviewbarscolour <- fromJSValueField "companysignviewbarscolour"
  jsoncompanysignviewbarstextcolour <- fromJSValueField "companysignviewbarstextcolour"
  jsoncompanysignviewbackgroundcolour <- fromJSValueField "companysignviewbackgroundcolour"
  jsoncompanycustomlogo <- fromJSValueField "companycustomlogo"
  jsoncompanycustombarscolour <- fromJSValueField "companycustombarscolour"
  jsoncompanycustombarstextcolour <- fromJSValueField "companycustombarstextcolour"
  jsoncompanycustombarssecondarycolour <- fromJSValueField "companycustombarssecondarycolour"
  jsoncompanycustombackgroundcolour <- fromJSValueField "companycustombackgroundcolour"


  return $ CompanyUI {
    companyemailfont = maybeS jsoncompanyemailfont
  , companyemailbordercolour = maybeS jsoncompanyemailbordercolour
  , companyemailbuttoncolour = maybeS jsoncompanyemailbuttoncolour
  , companyemailemailbackgroundcolour = maybeS jsoncompanyemailemailbackgroundcolour
  , companyemailbackgroundcolour = maybeS jsoncompanyemailbackgroundcolour
  , companyemailtextcolour = maybeS jsoncompanyemailtextcolour
  , companyemaillogo = (Binary . B64.decodeLenient) <$> BS.fromString <$>  drop 1 <$> dropWhile ((/=) ',') <$> maybeS jsoncompanyemaillogo
  , companysignviewlogo = (Binary . B64.decodeLenient) <$> BS.fromString <$>  drop 1 <$> dropWhile ((/=) ',')  <$> maybeS jsoncompanysignviewlogo
  , companysignviewtextcolour = maybeS jsoncompanysignviewtextcolour
  , companysignviewtextfont = maybeS jsoncompanysignviewtextfont
  , companysignviewbarscolour = maybeS jsoncompanysignviewbarscolour
  , companysignviewbarstextcolour = maybeS jsoncompanysignviewbarstextcolour
  , companysignviewbackgroundcolour = maybeS jsoncompanysignviewbackgroundcolour
  , companycustomlogo = (Binary . B64.decodeLenient) <$> BS.fromString <$>  drop 1 <$> dropWhile ((/=) ',') <$> maybeS jsoncompanycustomlogo
  , companycustombarscolour = maybeS jsoncompanycustombarscolour
  , companycustombarstextcolour = maybeS jsoncompanycustombarstextcolour
  , companycustombarssecondarycolour = maybeS jsoncompanycustombarssecondarycolour
  , companycustombackgroundcolour = maybeS jsoncompanycustombackgroundcolour
  }
  where
    maybeS (Just "")  = Nothing
    maybeS str = str

handleCompanyLogo :: Kontrakcja m => (CompanyUI -> Maybe Binary) -> CompanyID -> m Response
handleCompanyLogo field cid = do
  mimg <- join <$> fmap (field . companyui) <$> (dbQuery $ GetCompany cid)
  return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $
    Response 200 Map.empty nullRsFlags (BSL.fromChunks $ map unBinary $ maybeToList mimg) Nothing

handleCompanySignViewLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanySignViewLogo = handleCompanyLogo companysignviewlogo

handleCompanyCustomLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanyCustomLogo = handleCompanyLogo companycustomlogo

handleCompanyEmailLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanyEmailLogo = handleCompanyLogo companyemaillogo

handleGetCompanyJSON :: Kontrakcja m => Maybe CompanyID -> m JSValue
handleGetCompanyJSON mcid = withCompanyUserOrAdminOnly mcid $ \(editable, company) -> runJSONGenT $ do
    value "companyemailfont" $ fromMaybe "" $ companyemailfont $ companyui company
    value "companyemailbordercolour" $ fromMaybe "" $ companyemailbordercolour $ companyui company
    value "companyemailbuttoncolour" $ fromMaybe "" $ companyemailbuttoncolour $ companyui company
    value "companyemailemailbackgroundcolour" $ fromMaybe "" $ companyemailemailbackgroundcolour $ companyui company
    value "companyemailbackgroundcolour" $ fromMaybe "" $ companyemailbackgroundcolour $ companyui company
    value "companyemailtextcolour" $ fromMaybe "" $ companyemailtextcolour $ companyui company
    value "companyemaillogo" $ fromMaybe "" $ ((++) "data:image/png;base64,")  <$> BS.toString . B64.encode . unBinary <$> (companyemaillogo $ companyui $ company)
    value "companysignviewlogo" $ fromMaybe ""  $ ((++) "data:image/png;base64,")  <$> BS.toString .  B64.encode . unBinary <$> (companysignviewlogo $ companyui $ company)
    value "companysignviewtextcolour" $ fromMaybe "" $ companysignviewtextcolour $ companyui company
    value "companysignviewtextfont" $ fromMaybe "" $ companysignviewtextfont $ companyui company
    value "companysignviewbarscolour" $ fromMaybe "" $ companysignviewbarscolour $ companyui company
    value "companysignviewbarstextcolour" $ fromMaybe "" $ companysignviewbarstextcolour $ companyui company
    value "companysignviewbackgroundcolour" $ fromMaybe "" $ companysignviewbackgroundcolour $ companyui company
    value "companycustomlogo" $ fromMaybe ""  $ ((++) "data:image/png;base64,")  <$> BS.toString .  B64.encode . unBinary <$> (companycustomlogo $ companyui $ company)
    value "companycustombarscolour" $ fromMaybe "" $ companycustombarscolour $ companyui company
    value "companycustombarstextcolour" $ fromMaybe "" $ companycustombarstextcolour $ companyui company
    value "companycustombarssecondarycolour" $ fromMaybe "" $ companycustombarssecondarycolour $ companyui company
    value "companycustombackgroundcolour" $ fromMaybe "" $ companycustombackgroundcolour $ companyui company


    value "editable" editable
    value "ipmasklist" $ show <$> (companyipaddressmasklist $ companyinfo company)
