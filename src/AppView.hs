{- |
   Defines the App level views.
-}
module AppView( kontrakcja
              , renderFromBody
              , notFoundPage
              , signupPageView
              , signupVipPageView
              , pageLogin
              , simpleResponse
              , simpleResonseClrFlash
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
              , contactUsPage
              , apiPage
              , scriveByMailPage
              , modalError
              , standardPageFields
              ) where

import FlashMessage
import Kontra
import KontraLink
import Happstack.Fields

import Control.Applicative
import Data.List
import Data.Maybe
import Happstack.Server.SimpleHTTP
import Templates.Templates
import User.Lang
import User.Locale
import User.Region
import qualified Codec.Binary.Url as URL
import qualified Templates.Fields as F
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified Data.ByteString.UTF8 as BS (fromString)
import qualified Static.Resources as SR
import Data.Char
import Data.String.Utils

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
pageFromBody ctx loginOn referer email showCreateAccount title bodytext =
  renderTemplate "wholePage" $ do
    F.value "content" bodytext
    standardPageFields ctx title Nothing showCreateAccount loginOn referer email

notFoundPage :: Kontrakcja m => m Response
notFoundPage = renderTemplate_ "notFound" >>= renderFromBody kontrakcja

sitemapPage :: Kontrakcja m => m String
sitemapPage = do
  hostpart <- ctxhostpart <$> getContext
  renderTemplate "sitemapPage" $ do
    F.value "hostpart" $ case hostpart of
                            ('h':'t':'t':'p':'s':xs) -> "http" ++ xs
                            xs -> xs
    F.objects "locales" $ map staticLinksFields targetedLocales

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

{- |
    Render a template as an entire page.
-}
renderTemplateAsPage :: Kontrakcja m => Context -> String -> Maybe (Locale -> KontraLink) -> Bool -> m String
renderTemplateAsPage ctx templateName mpubliclink showCreateAccount = do
  loginOn <- getLoginOn
  loginreferer <- getLoginReferer
  let showCreateAccount2 = showCreateAccount && (isNothing $ ctxmaybeuser ctx)
  wholePage <- renderTemplate templateName $ do
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
  where
    querystring = qs <$> queryString lookPairs
      where
        qs qsPairs =
          let encodeString  = URL.encode . map (toEnum . ord)
              relevantPairs = [ (k, v) | (k, Right v) <- qsPairs ]
              empties       = [ encodeString k | (k, "") <- relevantPairs ]
              withValues    = [ encodeString k ++ "=" ++ encodeString v | (k, v) <- relevantPairs, length v > 0 ]
          in if Data.List.null relevantPairs
              then ""
              else "?" ++ intercalate "&" (empties ++ withValues)

standardPageFields :: TemplatesMonad m => Context -> String -> Maybe (Locale -> KontraLink) -> Bool -> Bool -> Maybe String -> Maybe String -> Fields m ()
standardPageFields ctx title mpubliclink showCreateAccount loginOn referer email = do
  F.value "title" title
  F.value "showCreateAccount" showCreateAccount
  mainLinksFields $ ctxlocale ctx
  staticLinksFields $ ctxlocale ctx
  localeSwitcherFields ctx mpubliclink
  contextInfoFields ctx
  publicSafeFlagField ctx loginOn (isJust mpubliclink)
  loginModal loginOn referer email
  F.value "staticResources" $ SR.htmlImportList "systemPage" (ctxstaticresources ctx)

{- |
   The contents of the signup page.  This is read from a template.
-}
signupPageView :: TemplatesMonad m => m String
signupPageView = renderTemplate_ "signupPageView"

signupVipPageView :: TemplatesMonad m => m String
signupVipPageView = renderTemplate_ "signupVipPageView"

{- |
   The contents of the login page.  This is read from a template.
-}
pageLogin :: TemplatesMonad m => Maybe String -> Maybe String -> m String
pageLogin referer email = renderTemplate "pageLogin" $ do
  F.value "referer" referer
  F.value "email" email

{- |
   Changing our pages into reponses, and clearing flash messages.
-}
simpleResponse :: Kontrakcja m => String -> m Response
simpleResponse = ok . toResponseBS (BS.fromString "text/html;charset=utf-8") . BSL.fromString
    -- change this to HtmlString from helpers package
    -- (didn't want to connect it one day before prelaunch)

{- | Sames as simpleResponse, but clears also flash messages and modals -}
simpleResonseClrFlash :: Kontrakcja m => String -> m Response
simpleResonseClrFlash rsp = do
  res <- simpleResponse rsp
  clearFlashMsgs
  return res
    
ajaxError :: Kontra Response
ajaxError = simpleResponse "<script>window.location='/'</script>"
{- |
   The landing page contents.  Read from template.
-}
firstPage :: TemplatesMonad m => Context -> Bool -> Maybe String -> Maybe String -> m String
firstPage ctx loginOn referer email = renderTemplate "firstPage" $ do
  contextInfoFields ctx
  publicSafeFlagField ctx loginOn True
  mainLinksFields $ ctxlocale ctx
  staticLinksFields $ ctxlocale ctx
  localeSwitcherFields ctx (Just LinkHome)
  loginModal (loginOn && null (filter isModal $ ctxflashmessages ctx)) referer email
  F.value "staticResources" $ SR.htmlImportList "firstPage" (ctxstaticresources ctx)

{- |
   Defines the main links as fields handy for substituting into templates.
-}
mainLinksFields :: Monad m => Locale -> Fields m ()
mainLinksFields locale = do
  F.value "linkaccount"          $ show (LinkAccount)
  F.value "linkforgotenpassword" $ show LinkForgotPassword
  F.value "linkissue"            $ show LinkArchive
  F.value "linklogin"            $ show (LinkLogin locale LoginTry)
  F.value "linklogout"           $ show LinkLogout
  F.value "linkupload"           $ show LinkUpload
  F.value "linkquestion"         $ show LinkAskQuestion
  F.value "linksignup"           $ show LinkSignup

localeSwitcherFields :: Monad m => Context -> Maybe (Locale -> KontraLink) -> Fields m ()
localeSwitcherFields Context{ctxlocale} mlink = do
  F.value "localesweden" $ getLang ctxlocale == LANG_SE
  F.value "localebritain" $ getLang ctxlocale == LANG_EN
  F.value "langsv" $ getLang ctxlocale == LANG_SE
  F.value "langen" $ getLang ctxlocale == LANG_EN
  F.value "linklocaleswitch" $ show LinkLocaleSwitch
  F.value "linksesv" $ fmap (\l -> show $ l (mkLocale REGION_SE LANG_SE)) mlink
  F.value "linkgben" $ fmap (\l -> show $ l (mkLocale REGION_GB LANG_EN)) mlink

{- |
    Defines the static links which are region and language sensitive.
-}
staticLinksFields :: Monad m => Locale -> Fields m ()
staticLinksFields locale = do
  F.value "linkhome"  $ show $ LinkHome locale
  F.value "linkpriceplan"  $ show $ LinkPriceplan locale
  F.value "linksecurity"  $ show $ LinkSecurity locale
  F.value "linklegal"  $ show $ LinkLegal locale
  F.value "linkprivacypolicy"  $ show $ LinkPrivacyPolicy locale
  F.value "linkterms"  $ show $ LinkTerms locale
  F.value "linkabout"  $ show $ LinkAbout locale
  F.value "linkpartners"  $ show $ LinkPartners locale
  F.value "linkclients"  $ show $ LinkClients locale
  F.value "linkcontactus" $ show $ LinkContactUs locale

{- |
   Defines some standard context information as fields handy for substitution
   into templates.
-}
contextInfoFields :: TemplatesMonad m => Context -> Fields m ()
contextInfoFields ctx@Context{ ctxlocale } = do
  F.value "logged" $ isJust (ctxmaybeuser ctx)
  F.objects "flashmessages" $ map flashMessageFields $ ctxflashmessages ctx
  F.value "hostpart" $ ctxhostpart ctx
  F.value "resourcehostpart" $ ctxresourcehostpart ctx
  F.value "production" (ctxproduction ctx)
  F.value "ctxregion" $ codeFromRegion (getRegion ctxlocale)
  F.value "ctxlang" $ codeFromLang (getLang ctxlocale)

{- |
    Only public safe is explicitely set as a public page,
    and nobody is trying to login, and a user isn't logged in.
    The flag means that things like snoobi won't be contacted.
-}
publicSafeFlagField :: Monad m => Context -> Bool -> Bool -> Fields m ()
publicSafeFlagField ctx loginOn publicpage = do
  F.value "publicsafe" $ publicpage
                      && (not loginOn)
                      && (isNothing $ ctxmaybeuser ctx)

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

loginModal :: Monad m => Bool -> Maybe String -> Maybe String -> Fields m ()
loginModal on referer email = do
  F.value "loginModal" $ on
  F.value "referer"    $ referer
  F.value "email"      $ email

modalError :: TemplatesMonad m => m FlashMessage
modalError = toModal <$> renderTemplate_ "modalError"
