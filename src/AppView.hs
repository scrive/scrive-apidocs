{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   Defines the App level views.
-}
module AppView( kontrakcja
              , renderFromBody
              , notFoundPage
              , simpleResponse
              , simpleResonseClrFlash
              , firstPage
              , sitemapPage
              , priceplanPage
              , securityPage
              , legalPage
              , privacyPolicyPage
              , termsPage
              , jobsPage
              , aboutPage
              , partnersPage
              , clientsPage
              , contactUsPage
              , apiPage
              , scriveByMailPage
              , featuresPage
              , modalError
              , standardPageFields
              , contextInfoFields
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
import qualified Codec.Binary.Url as URL
import qualified Templates.Fields as F
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified Data.ByteString.UTF8 as BS (fromString,toString)
import qualified Static.Resources as SR
import qualified Data.ByteString.Base16 as B16
import Data.Char
import Data.String.Utils
import Version


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

jobsPage :: Kontra String
jobsPage = getContext >>= \ctx -> renderTemplateAsPage ctx "jobsPage" (Just LinkJobs) True

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
renderTemplateAsPage ctx templateName mpubliclink showCreateAccount = do
  loginOn <- getLoginOn
  loginreferer <- getLoginReferer
  let showCreateAccount2 = showCreateAccount && (isNothing $ ctxmaybeuser ctx)
  wholePage <- renderTemplate templateName $ do
    contextInfoFields ctx
    mainLinksFields $ ctxlocale ctx
    staticLinksFields $ ctxlocale ctx
    localeSwitcherFields ctx mpubliclink
    loginModal (loginOn && null (filter isModal $ ctxflashmessages ctx)) loginreferer Nothing
    F.value "staticResources" $ SR.htmlImportList "firstPage" (ctxstaticresources ctx)
    F.value "showCreateAccount" showCreateAccount2
    F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
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

standardPageFields :: TemplatesMonad m => Context -> String -> Maybe (Lang -> KontraLink) -> Bool -> Bool -> Maybe String -> Maybe String -> Fields m ()
standardPageFields ctx title mpubliclink showCreateAccount loginOn referer email = do
  F.value "title" title
  F.value "showCreateAccount" showCreateAccount
  mainLinksFields $ ctxlang ctx
  staticLinksFields $ ctxlang ctx
  langSwitcherFields ctx mpubliclink
  contextInfoFields ctx
  loginModal loginOn referer email
  F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID
  F.value "staticResources" $ SR.htmlImportList "systemPage" (ctxstaticresources ctx)

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

{- |
   The landing page contents.  Read from template.
-}
firstPage :: TemplatesMonad m => Context -> Bool -> Maybe String -> Maybe String -> m String
firstPage ctx loginOn referer email = renderTemplate "firstPage" $ do
  contextInfoFields ctx
  mainLinksFields $ ctxlang ctx
  staticLinksFields $ ctxlang ctx
  langSwitcherFields ctx (Just LinkHome)
  loginModal (loginOn && null (filter isModal $ ctxflashmessages ctx)) referer email
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
  F.value "linkjobs"  $ show $ LinkJobs lang
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

loginModal :: Monad m => Bool -> Maybe String -> Maybe String -> Fields m ()
loginModal on referer email = do
  F.value "loginModal" $ on
  F.value "referer"    $ referer
  F.value "email"      $ email

modalError :: TemplatesMonad m => m FlashMessage
modalError = toModal <$> renderTemplate_ "modalError"
