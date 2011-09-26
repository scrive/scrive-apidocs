{- |
   Defines the App level views.
-}
module AppView( TopMenu(..)
              , kontrakcja
              , renderFromBody
              , embeddedPage
              , signupPageView
              , signupVipPageView
              , pageLogin
              , simpleResponse
              , ajaxError
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
              , modalError
              , embeddedErrorPage
              ) where

import API.Service.Model
import FlashMessage
import Kontra
import KontraLink
import Misc

import Control.Applicative
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Happstack.Server.SimpleHTTP
import Templates.Templates
import User.Lang
import User.Region
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified Data.ByteString.UTF8 as BS (fromString)


{- |
   Defines the different sorts of things we can have at the top of the page
-}
data TopMenu = TopNew | TopDocument | TopAccount | TopNone | TopEmpty
             deriving (Eq,Ord)

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
               => TopMenu
               -> String
               -> String
               -> m Response
renderFromBody _topmenu title content = do
    htmlPage <- (isSuffixOf ".html") . concat . rqPaths <$> askRq
    loginOn <- getLoginOn
    loginreferer <- getLoginReferer
    ctx <- getContext
    let showCreateAccount = htmlPage && (isNothing $ ctxmaybeuser ctx)
    res <- simpleResponse =<< pageFromBody ctx loginOn loginreferer Nothing showCreateAccount title content
    clearFlashMsgs
    return res


{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: TemplatesMonad m
             => Context
             -> Bool
             -> Maybe String
             -> Maybe String
             -> Bool
             -> String
             -> String
             -> m String
pageFromBody ctx
             loginOn
             referer
             email
             showCreateAccount
             title
             bodytext = do
    renderTemplateFM "wholePage" $ do
        field "content" bodytext
        standardPageFields ctx title Nothing showCreateAccount loginOn referer email

embeddedPage :: String -> Kontra Response
embeddedPage pb = do
    ctx <- getContext
    bdy <- renderTemplateFM "embeddedPage" $ do
        field "content" pb
        serviceFields (ctxservice ctx) (ctxlocation ctx)
        standardPageFields ctx "" Nothing False False Nothing Nothing
    res <- simpleResponse bdy
    clearFlashMsgs
    return res

embeddedErrorPage :: Kontra Response
embeddedErrorPage = do
    ctx <- getContext
    content <- renderTemplateFM "embeddedErrorPage" $ do
        serviceFields (ctxservice ctx) (ctxlocation ctx)
    simpleResponse content

serviceFields :: MonadIO m => Maybe Service -> String -> Fields m
serviceFields (Just service) location = do
    field "location" location
    field "buttons" $ isJust $ servicebuttons $ serviceui service
    field "buttonBodyLink"  $ show $ LinkServiceButtonsBody $ serviceid service
    field "buttonRestLink"  $ show $ LinkServiceButtonsRest $  serviceid service
    field "buttonstextcolor"  $ servicebuttonstextcolor $ serviceui service
    field "background"  $ servicebackground $ serviceui service
    field "overlaybackground"  $ serviceoverlaybackground $ serviceui service
    field "barsbackground"  $ servicebarsbackground $ serviceui service
    field "logo" $ isJust $ servicelogo $ serviceui service
    field "logoLink"  $ show $ LinkServiceLogo $ serviceid service
serviceFields Nothing location =
    field "location" location

sitemapPage :: Kontrakcja m => m String
sitemapPage = do
    hostpart <- ctxhostpart <$> getContext
    renderTemplateFM "sitemapPage" $ do
        field "hostpart" $ case hostpart of
                                ('h':'t':'t':'p':'s':xs) -> "http" ++ xs
                                xs -> xs
        fieldFL "locales" $ map (\r -> staticLinksFields r (defaultRegionLang r)) allValues

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

{- |
    Render a template as an entire page.
-}
renderTemplateAsPage :: Kontrakcja m => Context -> String -> Maybe (Region -> Lang -> KontraLink) -> Bool -> m String
renderTemplateAsPage ctx templateName mpubliclink showCreateAccount = do
    loginOn <- getLoginOn
    loginreferer <- getLoginReferer
    let showCreateAccount2 = showCreateAccount && (isNothing $ ctxmaybeuser ctx)
    wholePage <- renderTemplateFM templateName $ do
        standardPageFields ctx kontrakcja mpubliclink showCreateAccount2 loginOn loginreferer Nothing
    return wholePage

getLoginOn :: Kontrakcja m => m Bool
getLoginOn = do
    loginOn <- isFieldSet "logging"
    return loginOn

getLoginReferer :: Kontrakcja m => m (Maybe String)
getLoginReferer = do
    curr <- rqUri <$> askRq
    referer <- getField "referer"
    qstr <- querystring
    let loginreferer = Just $ fromMaybe (curr ++ qstr) referer
    return loginreferer

standardPageFields :: TemplatesMonad m => Context -> String -> Maybe (Region -> Lang -> KontraLink) -> Bool -> Bool -> Maybe String -> Maybe String -> Fields m
standardPageFields ctx title mpubliclink showCreateAccount loginOn referer email = do
    field "title" title
    field "showCreateAccount" showCreateAccount
    mainLinksFields (ctxregion ctx) (ctxlang ctx)
    staticLinksFields (ctxregion ctx) (ctxlang ctx)
    localeSwitcherFields ctx mpubliclink
    contextInfoFields ctx
    publicSafeFlagField ctx loginOn (isJust mpubliclink)
    loginModal loginOn referer email

{- |
   The contents of the signup page.  This is read from a template.
-}
signupPageView :: KontrakcjaTemplates -> IO String
signupPageView templates = renderTemplate templates "signupPageView" ()

signupVipPageView :: KontrakcjaTemplates -> IO String
signupVipPageView templates = renderTemplate templates "signupVipPageView" ()

{- |
   The contents of the login page.  This is read from a template.
-}
pageLogin :: TemplatesMonad m => Maybe String -> Maybe String -> m String
pageLogin referer email =
  renderTemplateFM "pageLogin" $ do
      field "referer" referer
      field "email" email

{- |
   Changing our pages into reponses, and clearing flash messages.
-}
simpleResponse :: Kontrakcja m => String -> m Response
simpleResponse s = ok $ toResponseBS (BS.fromString "text/html;charset=utf-8") (BSL.fromString s)
    -- change this to HtmlString from helpers package
    -- (didn't want to connect it one day before prelaunch)

ajaxError :: Kontra Response
ajaxError = simpleResponse "<script>window.location='/'</script>"
{- |
   The landing page contents.  Read from template.
-}
firstPage :: TemplatesMonad m => Context -> Bool -> Maybe String -> Maybe String -> m String
firstPage ctx loginOn referer email =
    renderTemplateFM "firstPage" $ do
        contextInfoFields ctx
        publicSafeFlagField ctx loginOn True
        mainLinksFields (ctxregion ctx) (ctxlang ctx)
        staticLinksFields (ctxregion ctx) (ctxlang ctx)
        localeSwitcherFields ctx (Just LinkHome)
        loginModal loginOn referer email

{- |
   Defines the main links as fields handy for substituting into templates.
-}
mainLinksFields :: MonadIO m => Region -> Lang -> Fields m
mainLinksFields region lang = do
    field "linkaccount"          $ show LinkAccount
    field "linkforgotenpassword" $ show LinkForgotPassword
    field "linkinvite"           $ show LinkInvite
    field "linkissue"            $ show LinkContracts
    field "linklogin"            $ show (LinkLogin region lang LoginTry)
    field "linklogout"           $ show LinkLogout
    field "linkupload"           $ show LinkUpload
    field "linkquestion"         $ show LinkAskQuestion
    field "linksignup"           $ show LinkSignup

localeSwitcherFields :: MonadIO m => Context -> Maybe (Region -> Lang -> KontraLink) -> Fields m
localeSwitcherFields Context{ctxlang} mlink = do
  field "localesweden" $ ctxlang == LANG_SE
  field "localebritain" $ ctxlang == LANG_EN
  field "langsv" $ ctxlang == LANG_SE
  field "langen" $ ctxlang == LANG_EN
  case mlink of
    Just link -> do
      field "linksesv" $ show $ link REGION_SE LANG_SE
      field "linkgben" $ show $ link REGION_GB LANG_EN
    Nothing -> do
      field "linklocaleswitch" $ show LinkLocaleSwitch

{- |
    Defines the static links which are region and language sensitive.
-}
staticLinksFields :: MonadIO m => Region -> Lang -> Fields m
staticLinksFields ctxregion ctxlang = do
    field "linkhome"  $ show $ LinkHome ctxregion ctxlang
    field "linkpriceplan"  $ show $ LinkPriceplan ctxregion ctxlang
    field "linksecurity"  $ show $ LinkSecurity ctxregion ctxlang
    field "linklegal"  $ show $ LinkLegal ctxregion ctxlang
    field "linkprivacypolicy"  $ show $ LinkPrivacyPolicy ctxregion ctxlang
    field "linkterms"  $ show $ LinkTerms ctxregion ctxlang
    field "linkabout"  $ show $ LinkAbout ctxregion ctxlang
    field "linkpartners"  $ show $ LinkPartners ctxregion ctxlang
    field "linkclients"  $ show $ LinkClients ctxregion ctxlang

{- |
   Defines some standard context information as fields handy for substitution
   into templates.
-}
contextInfoFields :: TemplatesMonad m => Context -> Fields m
contextInfoFields ctx = do
    field "logged" $ isJust (ctxmaybeuser ctx)
    fieldFL "flashmessages" $ map flashMessageFields $ ctxflashmessages ctx
    field "protocol" $ if (ctxproduction ctx) then "https:" else "http:"
    field "prefix" ""
    field "production" (ctxproduction ctx)
    field "ctxregion" $ codeFromRegion (ctxregion ctx)
    field "ctxlang" $ codeFromLang (ctxlang ctx)

{- |
    Only public safe is explicitely set as a public page,
    and nobody is trying to login, and a user isn't logged in.
    The flag means that things like snoobi won't be contacted.
-}
publicSafeFlagField :: MonadIO m => Context -> Bool -> Bool -> Fields m
publicSafeFlagField ctx loginOn publicpage = do
    field "publicsafe" $ publicpage
                           && (not loginOn)
                           && (isNothing $ ctxmaybeuser ctx)

flashMessageFields :: TemplatesMonad m => FlashMessage -> Fields m
flashMessageFields flash = do
    fieldM "type" $ (\t ->
        case t of
             SigningRelated  -> "blue"
             OperationDone   -> "green"
             OperationFailed -> "red"
             _               -> "") <$> ftype
    fieldM "message" $ jsText <$> msg
    fieldM "isModal" $ (== Modal) <$> ftype
    where
        fm :: TemplatesMonad m => m (FlashType, String)
        fm = fromJust . unFlashMessage <$> instantiate flash
        ftype :: TemplatesMonad m => m FlashType
        ftype = fst <$> fm
        msg :: TemplatesMonad m => m String
        msg = snd <$> fm

loginModal :: MonadIO m => Bool -> Maybe String -> Maybe String -> Fields m
loginModal on referer email = do
    field "loginModal" $ on
    field "referer"    $ referer
    field "email"      $ email

modalError :: TemplatesMonad m => m FlashMessage
modalError = toModal <$> renderTemplateM "modalError" ()
