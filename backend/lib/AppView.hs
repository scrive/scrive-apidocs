{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   Defines the App level views.
-}
module AppView(
                renderFromBody
              , renderFromBodyWithFields
              , notFoundPage
              , internalServerErrorPage
              , linkInvalidPage
              , simpleJsonResponse
              , simpleAesonResponse
              , simpleUnjsonResponse
              , simpleHtmlResponse
              , respondWithPDF
              , respondWithZipFile
              , unsupportedBrowserPage
              , standardPageFields
              , entryPointFields
              , userGroupUIForPage
              , enableCookiesPage
              ) where

import Control.Arrow (second)
import Data.String.Utils hiding (join)
import Data.Unjson
import Happstack.Server.SimpleHTTP
import Log
import Text.StringTemplates.Templates
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.JSON as JSON
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import BrandedDomain.BrandedDomain
import Branding.Adler32
import DB
import Kontra
import ThirdPartyStats.Core
import User.Lang
import User.Model
import UserGroup.Data
import UserGroup.Data.Subscription
import UserGroup.Model
import Utils.HTTP
import Utils.Monoid
import VersionTH

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
  ad  <- getAnalyticsData
  res <- simpleHtmlResponse =<< pageFromBody ctx ad content fields
  return res


{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: Kontrakcja m
             => Context
             -> AnalyticsData
             -> String
             -> Fields m ()
             -> m String
pageFromBody ctx ad bodytext fields = do
  mugidandui <- userGroupUIForPage
  renderTemplate "wholePage" $ do
    F.value "content" bodytext
    standardPageFields ctx mugidandui ad
    F.valueM "httplink" $ getHttpHostpart
    fields

userGroupForPage  :: Kontrakcja m => m (Maybe UserGroup)
userGroupForPage = do
  ctx <- getContext
  case (get ctxmaybeuser ctx) of
       Nothing -> return Nothing
       Just user -> fmap Just $ dbQuery $ UserGroupGetByUserID (userid user)

userGroupUIForPage  :: Kontrakcja m => m (Maybe (UserGroupID, UserGroupUI))
userGroupUIForPage = do
  ctx <- getContext
  case (get ctxmaybeuser ctx) of
       Just user -> do
         ug <- dbQuery . UserGroupGetByUserID . userid $ user
         return . Just $ (get ugID ug, get ugUI ug)
       _ -> return Nothing

currentSubscriptionJSON :: Kontrakcja m => m (Maybe A.Value)
currentSubscriptionJSON = do
  mug <- userGroupForPage
  case mug of
    Just ug -> Just . unjsonToJSON unjsonDef <$> getSubscription ug
    Nothing -> return Nothing

notFoundPage :: Kontrakcja m => m Response
notFoundPage = pageWhereLanguageCanBeInUrl $ do
  ctx <- getContext
  ad <- getAnalyticsData
  simpleHtmlResponse =<< renderTemplate "notFound" (standardPageFields ctx Nothing ad)

linkInvalidPage :: Kontrakcja m => m Response
linkInvalidPage = pageWhereLanguageCanBeInUrl $ do
  ctx <- getContext
  ad <- getAnalyticsData
  simpleHtmlResponse =<< renderTemplate "linkInvalid" (standardPageFields ctx Nothing ad)

internalServerErrorPage :: Kontrakcja m => m Response
internalServerErrorPage =  pageWhereLanguageCanBeInUrl $ do
  ctx <- getContext
  ad <- getAnalyticsData
  simpleHtmlResponse =<< renderTemplate "internalServerError" (standardPageFields ctx Nothing ad)

pageWhereLanguageCanBeInUrl :: Kontrakcja m => m Response -> m Response
pageWhereLanguageCanBeInUrl handler = do
  language <- fmap langFromCode <$> rqPaths <$> askRq
  case (language) of
       (Just lang:_) -> switchLang lang >> handler
       _ -> handler

unsupportedBrowserPage :: Kontrakcja m => m Response
unsupportedBrowserPage = do
  res <- renderTemplate "unsupportedBrowser" $ return ()
  simpleHtmlResponse res

enableCookiesPage :: Kontrakcja m => m Response
enableCookiesPage = do
  rq <- askRq
  let cookies = rqCookies rq
      headers = rqHeaders rq
      hostname = fst $ rqPeer rq
      ua = case Map.lookup "user-agent" headers of
             Just (HeaderPair _ (x:_)) -> BS.toString x
             _ -> "<unknown>"
  let cookieNames = show $ map fst cookies
      mixpanel event = asyncLogEvent (NamedEvent event) [ SomeProp "cookies" $ PVString cookieNames
                                                        , SomeProp "browser" $ PVString ua
                                                        , SomeProp "host" $ PVString hostname
                                                        ]
                                     EventMixpanel
  logInfo "Current cookies" $ object [
      "cookies" .= map (second cookieToJson) cookies
    ]
  ctx <- getContext
  ad <- getAnalyticsData
  case cookies of
    [] -> do
      -- there are still no cookies, client probably disabled them
      mixpanel "Enable cookies page load"
      content <- renderTemplate "enableCookies" $ do
        standardPageFields ctx Nothing ad
      simpleHtmlResponse content
    _ -> do
      -- there are some cookies after all, so no point in telling them to enable them
      mixpanel "Enable cookies page load attempt with cookies"
      -- internalServerError is a happstack function, it's not our internalError
      -- this will not rollback the transaction
      let fields = standardPageFields ctx Nothing ad
      internalServerError =<< pageWhereLanguageCanBeInUrl (simpleHtmlResponse =<< renderTemplate "sessionTimeOut" fields)
  where
    cookieToJson Cookie{..} = object [
        "version"   .= cookieVersion
      , "path"      .= cookiePath
      , "domain"    .= cookieDomain
      , "name"      .= cookieName
      , "value"     .= cookieValue
      , "secure"    .= secure
      , "http_only" .= httpOnly
      ]

standardPageFields :: (Kontrakcja m) => Context -> Maybe (UserGroupID, UserGroupUI) -> AnalyticsData -> Fields m ()
standardPageFields ctx mugidandui ad = do
  F.value "langcode"  $ codeFromLang $ get ctxlang ctx
  F.value "logged"    $ isJust (get ctxmaybeuser ctx)
  F.value "padlogged" $ isJust (get ctxmaybepaduser ctx)
  F.value "hostpart"  $ get ctxDomainUrl ctx
  F.value "production" (get ctxproduction ctx)
  F.value "brandingdomainid" (show $ get (bdid . ctxbrandeddomain) ctx)
  F.value "brandinguserid" (fmap (show . userid)
                            (get ctxmaybeuser ctx `mplus` get ctxmaybepaduser ctx))
  F.value "ctxlang" $ codeFromLang $ get ctxlang ctx
  F.object "analytics" $ analyticsTemplates ad
  F.value "trackjstoken" (get ctxtrackjstoken ctx)
  F.valueM "brandinghash" $ brandingAdler32 ctx mugidandui
  F.valueM "b64subscriptiondata" $  fmap (B64.encode . A.encode) <$> currentSubscriptionJSON
  F.value  "subscriptionuseriscompanyadmin" $
    case fmap useriscompanyadmin (get ctxmaybeuser ctx) of
      Nothing -> "undefined"
      Just True -> "true"
      Just False -> "false"
  F.value "title" $ case emptyToNothing . strip . T.unpack =<< get uguiBrowserTitle . snd =<< mugidandui of
                      Just ctitle -> ctitle ++ " - " ++
                                     (get (bdBrowserTitle . ctxbrandeddomain) ctx)
                      Nothing -> (get (bdBrowserTitle . ctxbrandeddomain) ctx)
  entryPointFields ctx

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
jsonContentType = "text/plain; charset=utf-8"

simpleJsonResponse :: (JSON.JSON a, FilterMonad Response m) => a -> m Response
simpleJsonResponse = ok . toResponseBS jsonContentType . BSL.fromString . JSON.encode

simpleAesonResponse :: (A.ToJSON a, FilterMonad Response m) => a -> m Response
simpleAesonResponse = ok . toResponseBS jsonContentType . A.encode . A.toJSON

simpleUnjsonResponse :: (FilterMonad Response m) => UnjsonDef a -> a -> m Response
simpleUnjsonResponse unjson a = ok $ toResponseBS jsonContentType $ unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjson a


{- |
   Changing our pages into reponses
-}
simpleHtmlResponse :: FilterMonad Response m => String -> m Response
simpleHtmlResponse s = ok $
  toResponseBS (BS.fromString "text/html;charset=utf-8") $ BSL.fromString s

respondWithPDF :: Bool -> BS.ByteString -> Response
respondWithPDF = respondWithDownloadContents "application/pdf"

respondWithZipFile :: Bool -> BS.ByteString -> Response
respondWithZipFile = respondWithDownloadContents "application/zip"

respondWithDownloadContents :: BS.ByteString -> Bool -> BS.ByteString -> Response
respondWithDownloadContents mimeType forceDownload contents =
  setHeaderBS "Content-Type" mimeType $
  (if forceDownload then setHeaderBS "Content-Disposition" "attachment" else id) $
  Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing

{- |
   JavaScript entry points require version and cdnbaseurl to work.
   This variables are also required by standardHeaderContents template.
-}
entryPointFields :: TemplatesMonad m => Context -> Fields m ()
entryPointFields ctx =  do
  F.value "cdnbaseurl" (get ctxcdnbaseurl ctx)
  F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
