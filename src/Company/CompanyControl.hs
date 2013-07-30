module Company.CompanyControl (
    handlePostCompany
  , handleGetCompanyJSON
  , handleCompanySignViewLogo
  , handleCompanyEmailLogo
  , routes
  , adminRoutes
  , withCompanyAdmin
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
import Company.Model
import Company.CompanyUI
import Kontra
import KontraLink
import Happstack.Fields
import Routing (hGet, hPost, toK0, toK1)
import User.Utils
import Util.MonadUtils
import Utils.String
import Text.JSON.Gen
import BrandedDomains

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
  [ hPost $ toK1 $ handlePostCompany . Just
  , dir "json" $ hGet $ toK1 $ handleGetCompanyJSON . Just
  ]

handlePostCompany :: Kontrakcja m => Maybe CompanyID -> m KontraLink
handlePostCompany mcid = withCompanyAdminOrAdminOnly mcid $ \company -> do
  iscompanyjson <- isFieldSet "company"
  when iscompanyjson $ do
    rawcompanyjson <- guardJustM $ getField "company"
    companyjson <- guardRight $ runGetJSON readJSValue rawcompanyjson
    jsoncui <- companyUiFromJSON companyjson
    _ <- dbUpdate $ SetCompanyUI (companyid company) jsoncui
    return ()
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
    companyuicompanyid = unsafeCompanyID 0
  , companyemailfont = maybeS jsoncompanyemailfont
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

handleCompanyLogo :: Kontrakcja m => (CompanyUI -> Maybe Binary) -> CompanyID -> m Response
handleCompanyLogo field cid = do
  mimg <- field <$> (dbQuery $ GetCompanyUI cid)
  return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $
    Response 200 Map.empty nullRsFlags (BSL.fromChunks $ map unBinary $ maybeToList mimg) Nothing

handleCompanySignViewLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanySignViewLogo = handleCompanyLogo companysignviewlogo

handleCompanyCustomLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanyCustomLogo = handleCompanyLogo companycustomlogo

handleCompanyEmailLogo :: Kontrakcja m => CompanyID -> m Response
handleCompanyEmailLogo = handleCompanyLogo companyemaillogo

handleGetCompanyJSON :: Kontrakcja m => Maybe CompanyID -> m JSValue
handleGetCompanyJSON mcid = do
  ctx <- getContext
  withCompanyUserOrAdminOnly mcid $ \(editable, company) -> do
    companyui <- dbQuery $ GetCompanyUI (companyid company)
    runJSONGenT $ do
    value "companyemailfont" $ fromMaybe "" $ companyemailfont $ companyui
    value "companyemailbordercolour" $ fromMaybe "" $ companyemailbordercolour $ companyui
    value "companyemailbuttoncolour" $ fromMaybe "" $ companyemailbuttoncolour $ companyui
    value "companyemailemailbackgroundcolour" $ fromMaybe "" $ companyemailemailbackgroundcolour $ companyui
    value "companyemailbackgroundcolour" $ fromMaybe "" $ companyemailbackgroundcolour $ companyui
    value "companyemailtextcolour" $ fromMaybe "" $ companyemailtextcolour $ companyui
    value "companyemaillogo" $ fromMaybe "" $ ((++) "data:image/png;base64,")  <$> BS.toString . B64.encode . unBinary <$> (companyemaillogo $ companyui)
    value "companysignviewlogo" $ fromMaybe ""  $ ((++) "data:image/png;base64,")  <$> BS.toString .  B64.encode . unBinary <$> (companysignviewlogo $ companyui)
    value "companysignviewtextcolour" $ fromMaybe "" $ companysignviewtextcolour $ companyui
    value "companysignviewtextfont" $ fromMaybe "" $ companysignviewtextfont $ companyui
    value "companysignviewbarscolour" $ fromMaybe "" $ companysignviewbarscolour $ companyui
    value "companysignviewbarstextcolour" $ fromMaybe "" $ companysignviewbarstextcolour $ companyui
    value "companysignviewbackgroundcolour" $ fromMaybe "" $ companysignviewbackgroundcolour $ companyui
    value "companycustomlogo" $ fromMaybe ""  $ ((++) "data:image/png;base64,")  <$> BS.toString .  B64.encode . unBinary <$> (companycustomlogo $ companyui)
    value "companycustombarscolour" $ fromMaybe "" $ companycustombarscolour $ companyui
    value "companycustombarstextcolour" $ fromMaybe "" $ companycustombarstextcolour $ companyui
    value "companycustombarssecondarycolour" $ fromMaybe "" $ companycustombarssecondarycolour $ companyui
    value "companycustombackgroundcolour" $ fromMaybe "" $ companycustombackgroundcolour $ companyui
    value "domaincustomlogo" $ fromMaybe "" $ bdlogolink <$> currentBrandedDomain ctx
    value "domainbarscolour" $ fromMaybe "" $ bdbarscolour <$> currentBrandedDomain ctx
    value "domainbarstextcolour" $ fromMaybe "" $ bdbarstextcolour <$> currentBrandedDomain ctx
    value "domainbarssecondarycolour" $ fromMaybe "" $ bdbarssecondarycolour <$> currentBrandedDomain ctx
    value "domainbackgroundcolour" $ fromMaybe "" $ bdbackgroundcolour <$> currentBrandedDomain ctx
    value "domainmailsbackgroundcolor" $ fromMaybe "" $ bdmailsbackgroundcolor <$> currentBrandedDomain ctx
    value "domainmailsbuttoncolor" $ fromMaybe "" $ bdmailsbuttoncolor <$> currentBrandedDomain ctx
    value "domainmailstextcolor" $ fromMaybe "" $ bdmailstextcolor <$> currentBrandedDomain ctx
    value "editable" editable
    value "ipmasklist" $ show <$> (companyipaddressmasklist $ companyinfo company)
