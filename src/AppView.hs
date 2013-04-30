{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   Defines the App level views.
-}
module AppView( kontrakcja
              , renderFromBody
              , renderFromBodyThin
              , notFoundPage
              , internalServerErrorPage
              , simpleJsonResponse
              , simpleHtmlResponse
              , simpleHtmlResonseClrFlash
              , priceplanPage
              , standardPageFields
              , contextInfoFields
              , renderTemplateAsPage
              , localizationScript
              , brandingFields
              , companyForPage
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
import qualified Static.Resources as SR
import qualified Data.ByteString.Base16 as B16
import Data.Char
import Data.String.Utils hiding (join)
import Version
import Text.JSON
import Utils.HTTP
import Analytics.Include
import DB
import Company.Model
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
renderFromBody title content = do
  ctx <- getContext
  ad <- getAnalyticsData
  res <- simpleHtmlResponse =<< pageFromBody False ctx ad title content
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
  res <- simpleHtmlResponse =<< pageFromBody True ctx ad title content
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
             -> m String
pageFromBody thin ctx ad title bodytext = do
  mcompany <- companyForPage
  mbd <- return $ currentBrandedDomain ctx
  renderTemplate "wholePage" $ do
    F.value "content" bodytext
    F.value "thin" thin
    standardPageFields ctx title ad
    F.valueM "httplink" $ getHttpHostpart
    brandingFields mbd mcompany

companyForPage  :: Kontrakcja m => m (Maybe Company)
companyForPage = do
  ctx <- getContext
  case (ctxmaybeuser ctx) of
       Nothing -> return Nothing
       Just user -> do
         mcompany <- dbQuery $ GetCompanyByUserID (userid user)
         case mcompany of
              Nothing -> return Nothing
              Just company -> return $ Just $ company

brandingFields ::  Kontrakcja m => Maybe BrandedDomain -> Maybe Company -> Fields m ()
brandingFields mbd mcompany = do
  F.value "customlogo" $ (isJust mbd) || (isJust $ (join $ companycustomlogo <$> companyui <$> mcompany))
  F.value "customlogolink" $ if (isJust $ (join $ companycustomlogo <$> companyui <$> mcompany))
                                then show <$> LinkCompanyCustomLogo <$> companyid <$> mcompany
                                else bdlogolink <$> mbd
  F.value "custombarscolour" $ mcolour bdbarscolour companycustombarscolour
  F.value "custombarstextcolour" $ mcolour bdbarstextcolour companycustombarstextcolour
  F.value "custombarshighlightcolour" $ mcolour bdbarssecondarycolour companycustombarssecondarycolour
  F.value "custombackground" $ mcolour bdbackgroundcolour companycustombackgroundcolour
  F.value "customdomainlogolink" $ bdlogolink <$> mbd

 where
   mcolour df cuf =  (join $ cuf <$> companyui <$> mcompany) `mplus` (df <$> mbd)


notFoundPage :: Kontrakcja m => m Response
notFoundPage = renderTemplate_ "notFound" >>= renderFromBody kontrakcja

internalServerErrorPage :: Kontrakcja m => m Response
internalServerErrorPage = renderTemplate_ "internalServerError" >>= renderFromBody kontrakcja

priceplanPage :: Kontrakcja m => m Response
priceplanPage = renderTemplate_ "priceplanPage" >>= renderFromBody kontrakcja

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
    F.value "staticResources" $ SR.htmlImportList "systemPage" (ctxstaticresources ctx)
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
  F.value "staticResources" $ SR.htmlImportList "systemPage" (ctxstaticresources ctx)
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
-}
simpleHtmlResponse :: Kontrakcja m => String -> m Response
simpleHtmlResponse = ok . toResponseBS (BS.fromString "text/html;charset=utf-8") . BSL.fromString
    -- change this to HtmlString from helpers package
    -- (didn't want to connect it one day before prelaunch)

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


flashMessageFields :: TemplatesMonad m => FlashMessage -> Fields m ()
flashMessageFields flash = do
  F.valueM "type" $ (\t -> case t of
    SigningRelated  -> "blue"
    OperationDone   -> "green"
    OperationFailed -> "red"
    _               -> "") <$> ftype
  F.valueM "message" $ do
      isFModal <- (== Modal) <$> ftype
      if (isFModal )
         then filter (not . isControl) <$> msg
         else replace "\"" "'" <$> filter (not . isControl) <$> msg
  F.valueM "isModal" $ (== Modal) <$> ftype
  where
    fm :: TemplatesMonad m => m (FlashType, String)
    fm = fromJust . unFlashMessage <$> instantiate flash
    ftype :: TemplatesMonad m => m FlashType
    ftype = fst <$> fm
    msg :: TemplatesMonad m => m String
    msg = snd <$> fm

localizationScript :: Kontrakcja m => String -> m Response
localizationScript _ = do
   script <- renderTemplate_ "javascriptLocalisation"
   ok $ toResponseBS (BS.fromString "text/javascript;charset=utf-8") $ BSL.fromString script
