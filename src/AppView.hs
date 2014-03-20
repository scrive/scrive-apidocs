{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   Defines the App level views.
-}
module AppView( kontrakcja
              , renderFromBody
              , renderFromBodyWithFields
              , renderFromBodyThin
              , notFoundPage
              , internalServerErrorPage
              , simpleJsonResponse
              , simpleHtmlResponse
              , simpleHtmlResonseClrFlash
              , priceplanPage
              , unsupportedBrowserPage
              , standardPageFields
              , contextInfoFields
              , renderTemplateAsPage
              , localizationScript
              , analyticsLoaderScript
              , brandingFields
              , companyForPage
              , companyUIForPage
              , handleTermsOfService
              , enableCookiesPage
              ) where

import FlashMessage
import Kontra
import KontraLink

import Control.Applicative
import Data.Maybe
import Happstack.Server.SimpleHTTP
import Text.StringTemplates.Templates
import User.Lang
import qualified Text.StringTemplates.Fields as F
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified Data.ByteString.UTF8 as BS (fromString,toString)
import qualified Data.ByteString.Base16 as B16
import Data.Char
import Data.String.Utils hiding (join)
import Version
import Text.JSON
import Utils.HTTP
import Analytics.Include
import DB
import Company.Model
import Company.CompanyUI
import User.Model
import Control.Monad
import BrandedDomains

{- |
   The name of our application (the codebase is known as kontrakcja,
   and this is the pretty public name)
-}
kontrakcja :: String
kontrakcja = "Scrive"

-- * Main Implementation

{- |
   Renders some page body xml into a complete reponse
-}
renderFromBody :: Kontrakcja m
               => String
               -> String
               -> m Response
renderFromBody title content = renderFromBodyWithFields title content (return ())


{- |
   Renders some page body xml into a complete reponse. It can take aditional fields to be passed to a template
-}
renderFromBodyWithFields :: Kontrakcja m
               => String
               -> String
               -> Fields m ()
               -> m Response
renderFromBodyWithFields title content fields = do
  ctx <- getContext
  ad <- getAnalyticsData
  res <- simpleHtmlResponse =<< pageFromBody False ctx ad title content fields
  clearFlashMsgs
  return res


{- |
   Renders some page body xml into a complete reponse
-}
renderFromBodyThin :: Kontrakcja m
               => String
               -> String
               -> m Response
renderFromBodyThin title content = do
  ctx <- getContext
  ad <- getAnalyticsData
  res <- simpleHtmlResponse =<< pageFromBody True ctx ad title content (return ())
  clearFlashMsgs
  return res

{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: Kontrakcja m
             => Bool
             -> Context
             -> AnalyticsData
             -> String
             -> String
             -> Fields m ()
             -> m String
pageFromBody thin ctx ad title bodytext fields = do
  mcompanyui <- companyUIForPage
  mbd <- return $ currentBrandedDomain ctx
  renderTemplate "wholePage" $ do
    F.value "content" bodytext
    F.value "thin" thin
    standardPageFields ctx title ad
    F.valueM "httplink" $ getHttpHostpart
    brandingFields mbd mcompanyui
    fields

companyForPage  :: Kontrakcja m => m (Maybe Company)
companyForPage = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
       Nothing -> return Nothing
       Just user -> fmap Just $ dbQuery $ GetCompanyByUserID (userid user)

companyUIForPage  :: Kontrakcja m => m (Maybe CompanyUI)
companyUIForPage = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
       Just User{usercompany = cid} -> Just <$> (dbQuery $ GetCompanyUI cid)
       _ -> return Nothing

brandingFields ::  Kontrakcja m => Maybe BrandedDomain -> Maybe CompanyUI -> Fields m ()
brandingFields mbd mcompanyui = do
  F.value "customlogo" $ (isJust mbd) || (isJust $ (join $ companycustomlogo <$> mcompanyui))
  F.value "customlogolink" $ if (isJust $ (join $ companycustomlogo <$> mcompanyui))
                                then show <$> LinkCompanyCustomLogo <$> companyuicompanyid <$> mcompanyui
                                else bdlogolink <$> mbd
  F.value "custombarscolour" $ mcolour bdbarscolour companycustombarscolour
  F.value "custombarstextcolour" $ mcolour bdbarstextcolour companycustombarstextcolour
  F.value "custombarshighlightcolour" $ mcolour bdbarssecondarycolour companycustombarssecondarycolour
  F.value "customsignviewprimarycolour" $ mcolour bdsignviewprimarycolour companysignviewprimarycolour
  F.value "customsignviewprimarytextcolour" $ mcolour bdsignviewprimarytextcolour companysignviewprimarytextcolour
  F.value "customsignviewsecondarycolour" $ mcolour bdsignviewsecondarycolour companysignviewsecondarycolour
  F.value "customsignviewsecondarytextcolour" $ mcolour bdsignviewsecondarytextcolour companysignviewsecondarytextcolour
  F.value "custombackground" $ mcolour bdbackgroundcolour companycustombackgroundcolour
  F.value "customdomainbdbuttonclass" $ bdbuttonclass <$> mbd
  F.value "customservicelinkcolour" $ bdservicelinkcolour <$> mbd
  F.value "hasbrandeddomain" $ isJust mbd
 where
   mcolour df cuf =  (join $ cuf <$> mcompanyui) `mplus` (df <$> mbd)


notFoundPage :: Kontrakcja m => m Response
notFoundPage = renderTemplate_ "notFound" >>= renderFromBody kontrakcja

internalServerErrorPage :: Kontrakcja m => m Response
internalServerErrorPage = renderTemplate_ "internalServerError" >>= renderFromBody kontrakcja

priceplanPage :: Kontrakcja m => m Response
priceplanPage = do
  ctx <- getContext
  case currentBrandedDomain ctx of
       Nothing -> renderTemplate_ "priceplanPage" >>= renderFromBody kontrakcja
       Just bd -> do
          ad <- getAnalyticsData
          content <- renderTemplate "priceplanPageWithBranding" $ do
            F.value "logolink" $ bdlogolink bd
            F.value "background" $ bdbackgroundcolorexternal $ bd
            F.value "buttoncolorclass" $ bdbuttonclass bd
            F.value "headercolour" $ bdheadercolour bd
            F.value "textcolour" $ bdtextcolour bd
            F.value "pricecolour" $ bdpricecolour bd
            standardPageFields ctx kontrakcja ad
          simpleHtmlResonseClrFlash content


unsupportedBrowserPage :: Kontrakcja m => m Response
unsupportedBrowserPage = do
  res <- renderTemplate "unsupportedBrowser" $ return ()
  simpleHtmlResponse res

enableCookiesPage :: Kontrakcja m => m Response
enableCookiesPage = simpleHtmlResponse =<< renderTemplate "enableCookies" (return ())

handleTermsOfService :: Kontrakcja m => m Response
handleTermsOfService = withAnonymousContext $ do
  ctx <- getContext
  case currentBrandedDomain ctx of
       Nothing -> renderTemplate_ "termsOfService" >>= renderFromBody kontrakcja
       Just bd -> do
          ad <- getAnalyticsData
          content <- renderTemplate "termsOfServiceWithBranding" $ do
            F.value "logolink" $ bdlogolink bd
            F.value "background" $ bdbackgroundcolorexternal $ bd
            F.value "buttoncolorclass" $ bdbuttonclass bd
            F.value "headercolour" $ bdheadercolour bd
            F.value "textcolour" $ bdtextcolour bd
            F.value "pricecolour" $ bdpricecolour bd
            standardPageFields ctx kontrakcja ad
          simpleHtmlResonseClrFlash content

{- |
    Render a template as an entire page.
-}
renderTemplateAsPage :: Kontrakcja m => Context -> String -> Bool -> (Fields m ()) -> m String
renderTemplateAsPage ctx templateName showCreateAccount f = do
  ad <- getAnalyticsData
  renderTemplate templateName $ do
    contextInfoFields ctx
    mainLinksFields $ ctxlang ctx
    langSwitcherFields ctx
    F.value "showCreateAccount" $ showCreateAccount && (isNothing $ ctxmaybeuser ctx)
    F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
    F.object "analytics" $ analyticsTemplates ad
    f

standardPageFields :: TemplatesMonad m => Context -> String -> AnalyticsData -> Fields m ()
standardPageFields ctx title ad = do
  F.value "title" title
  mainLinksFields $ ctxlang ctx
  langSwitcherFields ctx
  contextInfoFields ctx
  F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
  F.object "analytics" $ analyticsTemplates ad
  F.value "homebase" $ ctxhomebase ctx

-- Official documentation states that JSON mime type is
-- 'application/json'. IE8 for anything that starts with
-- 'application/*' invokes 'Download file...' dialog box and does not
-- allow JavaScript XHR to see the response. Therefore we have to
-- ignore the standard and output something that matches 'text/*', we
-- use 'text/javascript' for this purpose.
--
-- If future we should return 'application/json' for all browsers
-- except for IE8. We do not have access to 'Agent' string at this
-- point though, so we go this hackish route for everybody.
simpleJsonResponse :: (Kontrakcja m, JSON a) => a -> m Response
simpleJsonResponse = ok . toResponseBS (BS.fromString "text/html; charset=utf-8") . BSL.fromString . encode

{- |
   Changing our pages into reponses, and clearing flash messages.
   For HTML response we don't allow framing to skip problems with clickjacking.
-}
simpleHtmlResponse :: Kontrakcja m => String -> m Response
simpleHtmlResponse s = ok $ (setHeaderBS "X-Frame-Options" "SAMEORIGIN") $ toResponseBS (BS.fromString "text/html;charset=utf-8") $ BSL.fromString s


{- | Sames as simpleHtmlResponse, but clears also flash messages and modals -}
simpleHtmlResonseClrFlash :: Kontrakcja m => String -> m Response
simpleHtmlResonseClrFlash rsp = do
  res <- simpleHtmlResponse rsp
  clearFlashMsgs
  return res

{- |
   Defines the main links as fields handy for substituting into templates.
-}
mainLinksFields :: Monad m => Lang -> Fields m ()
mainLinksFields lang = do
  F.value "linkaccount"          $ show (LinkAccount)
  F.value "linkissue"            $ show LinkArchive
  F.value "linklogin"            $ show (LinkLogin lang LoginTry)
  F.value "linklogout"           $ show LinkLogout

langSwitcherFields :: Monad m => Context -> Fields m ()
langSwitcherFields Context{ctxlang} = do
  F.value "langswedish" $ getLang ctxlang == LANG_SV
  F.value "langenglish" $ getLang ctxlang == LANG_EN

{- |
   Defines some standard context information as fields handy for substitution
   into templates.
-}
contextInfoFields :: TemplatesMonad m => Context -> Fields m ()
contextInfoFields ctx@Context{ ctxlang } = do
  F.value "logged" $ isJust (ctxmaybeuser ctx)
  F.objects "flashmessages" $ map flashMessageFields $ ctxflashmessages ctx
  F.value "hostpart" $ ctxhostpart ctx
  F.value "resourcehostpart" $ ctxresourcehostpart ctx
  F.value "production" (ctxproduction ctx)
  F.value "ctxlang" $ codeFromLang ctxlang


flashMessageFields :: (Monad m) => FlashMessage -> Fields m ()
flashMessageFields flash = do
  F.value "type" $  case flashType flash of
    SigningRelated  -> "blue"
    OperationDone   -> "green"
    OperationFailed -> "red"
  F.value "message" $ replace "\"" "'" $ filter (not . isControl) $ flashMessage flash

localizationScript :: Kontrakcja m => String -> m Response
localizationScript _ = do
   script <- renderTemplate_ "javascriptLocalisation"
   ok $ toResponseBS (BS.fromString "text/javascript;charset=utf-8") $ BSL.fromString script

analyticsLoaderScript :: Kontrakcja m => m Response
analyticsLoaderScript = do
   ad <- getAnalyticsData
   script <- renderTemplate "analyticsLoaderBase" $ do
             F.object "analytics" $ analyticsTemplates ad
   ok $ toResponseBS (BS.fromString "text/javascript;charset=utf-8") $ BSL.fromString script
   
