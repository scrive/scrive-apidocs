{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   Defines the App level views.
-}
module AppView( kontrakcja
              , renderFromBody
              , notFoundPage
              , internalServerErrorPage
              , simpleJsonResponse
              , simpleHtmlResponse
              , simpleHtmlResonseClrFlash
              , firstPage
              , sitemapPage
              , priceplanPage
              , securityPage
              , legalPage
              , privacyPolicyPage
              , termsPage
              , aboutPage
              , partnersPage
              , clientsPage
              , contactUsPage
              , apiPage
              , scriveByMailPage
              , featuresPage
              , standardPageFields
              , contextInfoFields
              , renderTemplateAsPage
              , renderTemplateAsPageWithFields
              ) where

import FlashMessage
import Kontra
import KontraLink

import Control.Applicative
import Data.List
import Data.Maybe
import Happstack.Server.SimpleHTTP
import Templates.Templates
import User.Lang
import qualified Templates.Fields as F
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified Data.ByteString.UTF8 as BS (fromString,toString)
import qualified Static.Resources as SR
import qualified Data.ByteString.Base16 as B16
import Data.Char
import Data.String.Utils
import Version
import Text.JSON

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
  htmlPage <- (isSuffixOf ".html") . concat . rqPaths <$> askRq
  ctx <- getContext
  let showCreateAccount = htmlPage && (isNothing $ ctxmaybeuser ctx)
  res <- simpleHtmlResponse =<< pageFromBody ctx showCreateAccount title content
  clearFlashMsgs
  return res


{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: TemplatesMonad m
             => Context
             -> Bool
             -> String
             -> String
             -> m String
pageFromBody ctx showCreateAccount title bodytext =
  renderTemplate "wholePage" $ do
    F.value "content" bodytext
    standardPageFields ctx title Nothing showCreateAccount

notFoundPage :: Kontrakcja m => m Response
notFoundPage = renderTemplate_ "notFound" >>= renderFromBody kontrakcja

internalServerErrorPage :: Kontrakcja m => m Response
internalServerErrorPage = renderTemplate_ "internalServerError" >>= renderFromBody kontrakcja

sitemapPage :: Kontrakcja m => m String
sitemapPage = do
  hostpart <- ctxhostpart <$> getContext
  renderTemplate "sitemapPage" $ do
    F.value "hostpart" $ case hostpart of
                            ('h':'t':'t':'p':'s':xs) -> "http" ++ xs
                            xs -> xs
    F.objects "langs" $ map staticLinksFields allLangs

priceplanPage :: Kontra String
priceplanPage = getContext >>= \ctx -> renderTemplateAsPage ctx "priceplanPage" (Just LinkPriceplan) True

securityPage :: Kontra String
securityPage = getContext >>= \ctx -> renderTemplateAsPage ctx "securityPage" (Just LinkSecurity) True

legalPage :: Kontra String
legalPage = getContext >>= \ctx -> renderTemplateAsPage ctx "legalPage" (Just LinkLegal) True

privacyPolicyPage :: Kontra String
privacyPolicyPage = getContext >>= \ctx -> renderTemplateAsPage ctx "privacyPolicyPage" (Just LinkPrivacyPolicy) True

termsPage :: Kontra String
termsPage = getContext >>= \ctx -> renderTemplateAsPage ctx "termsPage" (Just LinkTerms) True

aboutPage :: Kontra String
aboutPage = getContext >>= \ctx -> renderTemplateAsPage ctx "aboutPage" (Just LinkAbout) True

partnersPage :: Kontra String
partnersPage = getContext >>= \ctx -> renderTemplateAsPage ctx "partnersPage" (Just LinkPartners) True

clientsPage :: Kontra String
clientsPage = getContext >>= \ctx -> renderTemplateAsPage ctx "clientsPage" (Just LinkClients) True

contactUsPage:: Kontra String
contactUsPage = getContext >>= \ctx -> renderTemplateAsPage ctx "contactUsPage" (Just LinkContactUs) True

apiPage:: Kontra String
apiPage = getContext >>= \ctx -> renderTemplateAsPage ctx "apiPage" (Just LinkAPIPage) True

scriveByMailPage :: Kontra String
scriveByMailPage = getContext >>= \ctx -> renderTemplateAsPage ctx "scrivebymailPage" (Just LinkScriveByMailPage) True

featuresPage :: Kontra String
featuresPage = getContext >>= \ctx -> renderTemplateAsPage ctx "featuresPage" (Just LinkFeaturesPage) True

{- |
    Render a template as an entire page.
-}
renderTemplateAsPage :: Kontrakcja m => Context -> String -> Maybe (Lang -> KontraLink) -> Bool -> m String
renderTemplateAsPage ctx templateName mpubliclink showCreateAccount =
    renderTemplateAsPageWithFields ctx templateName mpubliclink showCreateAccount []

renderTemplateAsPageWithFields :: Kontrakcja m => Context -> String -> Maybe (Lang -> KontraLink) -> Bool -> [(String, String)] -> m String
renderTemplateAsPageWithFields ctx templateName mpubliclink showCreateAccount extraFields = do
  let showCreateAccount2 = showCreateAccount && (isNothing $ ctxmaybeuser ctx)
  wholePage <- renderTemplate templateName $ do
    contextInfoFields ctx
    mainLinksFields $ ctxlang ctx
    staticLinksFields $ ctxlang ctx
    langSwitcherFields ctx mpubliclink
    F.value "staticResources" $ SR.htmlImportList "firstPage" (ctxstaticresources ctx)
    F.value "showCreateAccount" showCreateAccount2
    F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
    mapM_ (uncurry F.value) extraFields
  return wholePage

standardPageFields :: TemplatesMonad m => Context -> String -> Maybe (Lang -> KontraLink) -> Bool -> Fields m ()
standardPageFields ctx title mpubliclink showCreateAccount = do
  F.value "title" title
  F.value "showCreateAccount" showCreateAccount
  mainLinksFields $ ctxlang ctx
  staticLinksFields $ ctxlang ctx
  langSwitcherFields ctx mpubliclink
  contextInfoFields ctx
  F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
  F.value "staticResources" $ SR.htmlImportList "systemPage" (ctxstaticresources ctx)

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
   The landing page contents.  Read from template.
-}
firstPage :: TemplatesMonad m => Context -> m String
firstPage ctx = renderTemplate "firstPage" $ do
  contextInfoFields ctx
  mainLinksFields $ ctxlang ctx
  staticLinksFields $ ctxlang ctx
  langSwitcherFields ctx (Just LinkHome)
  F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
  F.value "staticResources" $ SR.htmlImportList "firstPage" (ctxstaticresources ctx)

{- |
   Defines the main links as fields handy for substituting into templates.
-}
mainLinksFields :: Monad m => Lang -> Fields m ()
mainLinksFields lang = do
  F.value "linkaccount"          $ show (LinkAccount)
  F.value "linkforgotenpassword" $ show LinkForgotPassword
  F.value "linkissue"            $ show LinkArchive
  F.value "linklogin"            $ show (LinkLogin lang LoginTry)
  F.value "linklogout"           $ show LinkLogout
  F.value "linkquestion"         $ show LinkAskQuestion
  F.value "linksignup"           $ show LinkSignup

langSwitcherFields :: Monad m => Context -> Maybe (Lang -> KontraLink) -> Fields m ()
langSwitcherFields Context{ctxlang} mlink = do
  F.value "langswedish" $ getLang ctxlang == LANG_SV
  F.value "langenglish" $ getLang ctxlang == LANG_EN
  F.value "linklangswitch" $ show LinkLangSwitch
  F.value "linksv" $ fmap (\l -> show $ l LANG_SV) mlink
  F.value "linken" $ fmap (\l -> show $ l LANG_EN) mlink

{- |
    Defines the static links which are language sensitive.
-}
staticLinksFields :: Monad m => Lang -> Fields m ()
staticLinksFields lang = do
  F.value "linkhome"  $ show $ LinkHome lang
  F.value "linkpriceplan"  $ show $ LinkPriceplan lang
  F.value "linksecurity"  $ show $ LinkSecurity lang
  F.value "linklegal"  $ show $ LinkLegal lang
  F.value "linkprivacypolicy"  $ show $ LinkPrivacyPolicy lang
  F.value "linkterms"  $ show $ LinkTerms lang
  F.value "linkabout"  $ show $ LinkAbout lang
  F.value "linkpartners"  $ show $ LinkPartners lang
  F.value "linkclients"  $ show $ LinkClients lang
  F.value "linkcontactus" $ show $ LinkContactUs lang

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
