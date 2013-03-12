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
  jsoncompanyemailheaderfont <- fromJSValueField "companyemailheaderfont" jsv
  jsoncompanyemailfont <- fromJSValueField "companyemailfont" jsv
  jsoncompanyemailbordercolour <- fromJSValueField "companyemailbordercolour" jsv
  jsoncompanyemailbuttoncolour <- fromJSValueField "companyemailbuttoncolour" jsv
  jsoncompanyemailemailbackgroundcolour <- fromJSValueField "companyemailemailbackgroundcolour" jsv
  jsoncompanyemailbackgroundcolour <- fromJSValueField "companyemailbackgroundcolour" jsv
  jsoncompanyemailtextcolour <- fromJSValueField "companyemailtextcolour" jsv
  jsoncompanyemaillogo <- fromJSValueField "companyemaillogo" jsv
  jsoncompanysignviewlogo <- fromJSValueField "companysignviewlogo" jsv
  jsoncompanysignviewtextcolour <- fromJSValueField "companysignviewtextcolour" jsv
  jsoncompanysignviewtextfont <- fromJSValueField "companysignviewtextfont" jsv
  jsoncompanysignviewfootertextcolour <- fromJSValueField "companysignviewfootertextcolour" jsv
  jsoncompanysignviewfootertextfont <- fromJSValueField "companysignviewfootertextfont" jsv
  jsoncompanysignviewheadertextcolour <- fromJSValueField "companysignviewheadertextcolour" jsv
  jsoncompanysignviewheadertextfont <- fromJSValueField "companysignviewheadertextfont" jsv
  jsoncompanysignviewheaderbackgroundcolour <- fromJSValueField "companysignviewheaderbackgroundcolour" jsv
  jsoncompanysignviewfooterbackgroundcolour <- fromJSValueField "companysignviewfooterbackgroundcolour" jsv
  jsoncompanysignviewbackgroundcolour <- fromJSValueField "companysignviewbackgroundcolour" jsv
  jsonsignviewlogochanged <- fromJSValueField "signviewlogochanged" jsv
  jsonemaillogochanged <- fromJSValueField "emaillogochanged" jsv

  let signviewlogo = if jsonsignviewlogochanged then
                         fmap (Binary . B64.decodeLenient) $ maybeS jsoncompanysignviewlogo
                     else
                         companysignviewlogo cui
      emaillogo = if jsonemaillogochanged then
                      fmap (Binary . B64.decodeLenient) $ maybeS jsoncompanyemaillogo
                  else
                      companyemaillogo cui
  return CompanyUI {
    companyemailheaderfont = maybeS jsoncompanyemailheaderfont
  , companyemailfont = maybeS jsoncompanyemailfont
  , companyemailbordercolour = maybeS jsoncompanyemailbordercolour
  , companyemailbuttoncolour = maybeS jsoncompanyemailbuttoncolour
  , companyemailemailbackgroundcolour = maybeS jsoncompanyemailemailbackgroundcolour
  , companyemailbackgroundcolour = maybeS jsoncompanyemailbackgroundcolour
  , companyemailtextcolour = maybeS jsoncompanyemailtextcolour
  , companyemaillogo = emaillogo
  , companysignviewlogo = signviewlogo
  , companysignviewtextcolour = maybeS jsoncompanysignviewtextcolour
  , companysignviewtextfont = maybeS jsoncompanysignviewtextfont
  , companysignviewfootertextcolour = maybeS jsoncompanysignviewfootertextcolour
  , companysignviewfootertextfont = maybeS jsoncompanysignviewfootertextfont
  , companysignviewheadertextcolour = maybeS jsoncompanysignviewheadertextcolour
  , companysignviewheadertextfont = maybeS jsoncompanysignviewheadertextfont
  , companysignviewheaderbackgroundcolour = maybeS jsoncompanysignviewheaderbackgroundcolour
  , companysignviewfooterbackgroundcolour = maybeS jsoncompanysignviewfooterbackgroundcolour
  , companysignviewbackgroundcolour = maybeS jsoncompanysignviewbackgroundcolour
  }
  where
    maybeS ""  = Nothing
    maybeS str = Just str

handleCompanyLogo :: Kontrakcja m => (CompanyUI -> Maybe Binary) -> CompanyID -> m Response
handleCompanyLogo field cid = do
  mimg <- join <$> fmap (field . companyui) <$> (dbQuery $ GetCompany cid)
  return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $
    Response 200 Map.empty nullRsFlags (BSL.fromChunks $ map unBinary $ maybeToList mimg) Nothing

handleCompanySignViewLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanySignViewLogo = handleCompanyLogo companysignviewlogo

handleCompanyEmailLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanyEmailLogo = handleCompanyLogo companyemaillogo

handleGetCompanyJSON :: Kontrakcja m => Maybe CompanyID -> m JSValue
handleGetCompanyJSON mcid = withCompanyUserOrAdminOnly mcid $ \(editable, company) -> runJSONGenT $ do
    value "companyemailheaderfont" $ fromMaybe "" $ companyemailheaderfont $ companyui company
    value "companyemailfont" $ fromMaybe "" $ companyemailfont $ companyui company
    value "companyemailbordercolour" $ fromMaybe "" $ companyemailbordercolour $ companyui company
    value "companyemailbuttoncolour" $ fromMaybe "" $ companyemailbuttoncolour $ companyui company
    value "companyemailemailbackgroundcolour" $ fromMaybe "" $ companyemailemailbackgroundcolour $ companyui company
    value "companyemailbackgroundcolour" $ fromMaybe "" $ companyemailbackgroundcolour $ companyui company
    value "companyemailtextcolour" $ fromMaybe "" $ companyemailtextcolour $ companyui company
    value "companyemaillogo" $ maybe "" (const $ show $ LinkCompanyEmailLogo $ companyid company) $ companyemaillogo $ companyui $ company
    value "companysignviewlogo" $ maybe "" (const $ show $ LinkCompanySignViewLogo $ companyid company) $ companysignviewlogo $ companyui $ company
    value "companysignviewtextcolour" $ fromMaybe "" $ companysignviewtextcolour $ companyui company
    value "companysignviewtextfont" $ fromMaybe "" $ companysignviewtextfont $ companyui company
    value "companysignviewfootertextcolour" $ fromMaybe "" $ companysignviewfootertextcolour $ companyui company
    value "companysignviewfootertextfont" $ fromMaybe "" $ companysignviewfootertextfont $ companyui company
    value "companysignviewheadertextcolour" $ fromMaybe "" $ companysignviewheadertextcolour $ companyui company
    value "companysignviewheadertextfont" $ fromMaybe "" $ companysignviewheadertextfont $ companyui company
    value "companysignviewheaderbackgroundcolour" $ fromMaybe "" $ companysignviewheaderbackgroundcolour $ companyui company
    value "companysignviewfooterbackgroundcolour" $ fromMaybe "" $ companysignviewfooterbackgroundcolour $ companyui company
    value "companysignviewbackgroundcolour" $ fromMaybe "" $ companysignviewbackgroundcolour $ companyui company
    value "editable" editable
    value "ipmasklist" $ show <$> (companyipaddressmasklist $ companyinfo company)
