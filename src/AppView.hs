{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   Defines the App level views.
-}
module AppView(
                renderFromBody
              , renderFromBodyWithFields
              , renderFromBodyThin
              , notFoundPage
              , internalServerErrorPage
              , simpleJsonResponse
              , simpleAesonResponse
              , simpleHtmlResponse
              , simpleHtmlResonseClrFlash
              , respondWithPDF
              , priceplanPage
              , unsupportedBrowserPage
              , standardPageFields
              , contextInfoFields
              , localizationScript
              , companyForPage
              , companyUIForPage
              , handleTermsOfService
              , enableCookiesPage
              ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Char
import Data.Maybe
import Data.String.Utils hiding (join)
import Happstack.Server.SimpleHTTP
import Text.JSON
import Text.StringTemplates.Templates
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import BrandedDomain.BrandedDomain
import Branding.Adler32
import Company.CompanyUI
import Company.Model
import DB
import FlashMessage
import Kontra
import ThirdPartyStats.Core
import User.Lang
import User.Model
import Utils.HTTP
import Utils.Monoid
import Version
import qualified Log

-- * Main Implementation

{- |
   Renders some page body xml into a complete reponse
-}
renderFromBody :: Kontrakcja m
               => String
               -> m Response
renderFromBody content = renderFromBodyWithFields content (return ())


{- |
   Renders some page body xml into a complete reponse. It can take aditional fields to be passed to a template
-}
renderFromBodyWithFields :: Kontrakcja m
               => String
               -> Fields m ()
               -> m Response
renderFromBodyWithFields content fields = do
  ctx <- getContext
  ad <- getAnalyticsData
  res <- simpleHtmlResponse =<< pageFromBody False ctx ad content fields
  clearFlashMsgs
  return res


{- |
   Renders some page body xml into a complete reponse
-}
renderFromBodyThin :: Kontrakcja m
               => String
               -> m Response
renderFromBodyThin content = do
  ctx <- getContext
  ad <- getAnalyticsData
  res <- simpleHtmlResponse =<< pageFromBody True ctx ad content (return ())
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
             -> Fields m ()
             -> m String
pageFromBody thin ctx ad bodytext fields = do
  mcompanyui <- companyUIForPage
  renderTemplate "wholePage" $ do
    F.value "content" bodytext
    F.value "thin" thin
    standardPageFields ctx mcompanyui ad
    F.valueM "httplink" $ getHttpHostpart
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

notFoundPage :: Kontrakcja m => m Response
notFoundPage = pageWhereLanguageCanBeInUrl $ do
  ctx <- getContext
  ad <- getAnalyticsData
  content <- if (bdMainDomain (ctxbrandeddomain ctx)||  isJust (ctxmaybeuser ctx))
   then renderTemplate "notFound" $ do
                    standardPageFields ctx Nothing ad
   else renderTemplate "notFoundWithoutHeaders" $ do
                    standardPageFields ctx Nothing ad
  simpleHtmlResonseClrFlash content

internalServerErrorPage :: Kontrakcja m => m Response
internalServerErrorPage =  pageWhereLanguageCanBeInUrl $ do
  ctx <- getContext
  ad <- getAnalyticsData
  content <- if (bdMainDomain (ctxbrandeddomain ctx)||  isJust (ctxmaybeuser ctx))
   then renderTemplate "internalServerError" $ do
                    standardPageFields ctx Nothing ad
   else renderTemplate "internalServerErrorWithoutHeaders" $ do
                    standardPageFields ctx Nothing ad
  simpleHtmlResonseClrFlash content

pageWhereLanguageCanBeInUrl :: Kontrakcja m => m Response -> m Response
pageWhereLanguageCanBeInUrl handler = do
  language <- fmap langFromCode <$> rqPaths <$> askRq
  case (language) of
       (Just lang:_) -> switchLang lang >> handler
       _ -> handler


priceplanPage :: Kontrakcja m => m Response
priceplanPage = do
  ctx <- getContext
  ad <- getAnalyticsData
  if( bdMainDomain $ ctxbrandeddomain ctx)
  then do
    content <- renderTemplate "priceplanPage" $ do
      standardPageFields ctx Nothing ad
    simpleHtmlResonseClrFlash content
  else respond404

unsupportedBrowserPage :: Kontrakcja m => m Response
unsupportedBrowserPage = do
  res <- renderTemplate "unsupportedBrowser" $ return ()
  simpleHtmlResponse res

enableCookiesPage :: Kontrakcja m => m Response
enableCookiesPage = do
  cookies <- rqCookies <$> askRq
  let cookieNames = show $ map fst cookies
      mixpanel event = asyncLogEvent (NamedEvent event) [SomeProp "cookies" $ PVString cookieNames]
  Log.mixlog_ $ show cookies
  case cookies of
    [] -> do
      -- there are still no cookies, client probably disabled them
      mixpanel "Enable cookies page load"
      ctx <- getContext
      ad <- getAnalyticsData
      content <- renderTemplate "enableCookies" $ do
        standardPageFields ctx Nothing ad
      simpleHtmlResponse content
    _ -> do
      -- there are some cookies after all, so no point in telling them to enable them
      mixpanel "Enable cookies page load attempt with cookies"
      -- internalServerError is a happstack function, it's not our internalError
      -- this will not rollback the transaction
      pageWhereLanguageCanBeInUrl $ renderTemplate_ "sessionTimeOut" >>= renderFromBody >>= internalServerError

handleTermsOfService :: Kontrakcja m => m Response
handleTermsOfService = withAnonymousContext $ do
  ctx <- getContext
  ad <- getAnalyticsData
  content <- if (bdMainDomain $ ctxbrandeddomain ctx)
                then do
                  renderTemplate "termsOfService" $ do
                    standardPageFields ctx Nothing ad
                else do
                  renderTemplate "termsOfServiceWithBranding" $ do
                    standardPageFields ctx Nothing ad
  simpleHtmlResonseClrFlash content

standardPageFields :: (TemplatesMonad m, MonadDB m, MonadThrow m) => Context -> Maybe CompanyUI -> AnalyticsData -> Fields m ()
standardPageFields ctx mcompanyui ad = do
  F.value "langcode" $ codeFromLang $ ctxlang ctx
  contextInfoFields ctx
  F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
  F.object "analytics" $ analyticsTemplates ad
  F.value "homebase" $ ctxhomebase ctx
  F.valueM "brandinghash" $ brandingAdler32 ctx mcompanyui
  F.value "title" $ case (join$ nothingIfEmpty <$> strip <$> (join $ companyBrowserTitle <$> mcompanyui)) of
                      Just ctitle -> ctitle ++ " - " ++ (bdBrowserTitle $ ctxbrandeddomain ctx)
                      Nothing -> (bdBrowserTitle $ ctxbrandeddomain ctx)

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

jsonContentType :: BS.ByteString
jsonContentType = "text/html; charset=utf-8"

simpleJsonResponse :: (JSON a, FilterMonad Response m) => a -> m Response
simpleJsonResponse = ok . toResponseBS jsonContentType . BSL.fromString . encode

simpleAesonResponse :: (A.ToJSON a, FilterMonad Response m) => a -> m Response
simpleAesonResponse = ok . toResponseBS jsonContentType . A.encode . A.toJSON

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

respondWithPDF :: Bool -> BS.ByteString -> Response
respondWithPDF forceDownload contents =
  setHeaderBS "Content-Type" "application/pdf" $
  (if forceDownload then setHeaderBS "Content-Disposition" "attachment" else id) $
  Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing

{- |
   Defines some standard context information as fields handy for substitution
   into templates.
-}
contextInfoFields :: TemplatesMonad m => Context -> Fields m ()
contextInfoFields ctx@Context{ ctxlang } = do
  F.value "logged" $ isJust (ctxmaybeuser ctx)
  F.value "padlogged" $ isJust (ctxmaybepaduser ctx)
  F.objects "flashmessages" $ map flashMessageFields $ ctxflashmessages ctx
  F.value "hostpart" $ ctxhostpart ctx
  F.value "resourcehostpart" $ ctxresourcehostpart ctx
  F.value "production" (ctxproduction ctx)
  F.value "ctxlang" $ codeFromLang ctxlang


flashMessageFields :: (Monad m) => FlashMessage -> Fields m ()
flashMessageFields flash = do
  F.value "type" $  case flashType flash of
    OperationDone   -> ("success" :: String)
    OperationFailed -> ("error" :: String)
  F.value "message" $ replace "\"" "'" $ filter (not . isControl) $ flashMessage flash

localizationScript :: Kontrakcja m => String -> m Response
localizationScript _ = do
   Context{ctxlang} <- getContext
   script <- renderTemplate "javascriptLocalisation" $ F.value "code" $ codeFromLang ctxlang
   ok $
     setHeaderBS "Cache-Control" "max-age=31536000"  $
     toResponseBS (BS.fromString "text/javascript;charset=utf-8") $
       BSL.fromString script
