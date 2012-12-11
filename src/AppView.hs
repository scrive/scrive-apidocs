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
              ) where

import FlashMessage
import Kontra
import KontraLink

import Control.Applicative
import Data.Maybe
import Happstack.Server.SimpleHTTP
import Templates.Templates
import Company.Model
import User.Lang
import User.Model
import User.UserView
import qualified Templates.Fields as F
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified Data.ByteString.UTF8 as BS (fromString,toString)
import qualified Static.Resources as SR
import qualified Data.ByteString.Base16 as B16
import Data.Char
import Data.String.Utils
import Version
import Text.JSON
import Utils.HTTP
import DB

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
  let muser = ctxmaybeuser ctx
  mcompany <- case usercompany <$> muser of
    Just (Just cid) -> dbQuery $ GetCompany cid
    _ -> return Nothing

  res <- simpleHtmlResponse =<< pageFromBody False ctx muser mcompany title content
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
  let muser = ctxmaybeuser ctx
  mcompany <- case usercompany <$> muser of
    Just (Just cid) -> dbQuery $ GetCompany cid
    _ -> return Nothing

  res <- simpleHtmlResponse =<< pageFromBody True ctx muser mcompany title content
  clearFlashMsgs
  return res


{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: Kontrakcja m
             => Bool
             -> Context
             -> Maybe User
             -> Maybe Company
             -> String
             -> String
             -> m String
pageFromBody thin ctx muser mcompany title bodytext =
  renderTemplate "wholePage" $ do
    F.value "content" bodytext
    F.value "thin" thin
    standardPageFields ctx title Nothing
    F.valueM "httplink" $ getHttpHostpart
    case muser of
      Nothing -> return ()
      Just user -> F.object "user" $ userBasicFields user mcompany

notFoundPage :: Kontrakcja m => m Response
notFoundPage = renderTemplate_ "notFound" >>= renderFromBody kontrakcja

internalServerErrorPage :: Kontrakcja m => m Response
internalServerErrorPage = renderTemplate_ "internalServerError" >>= renderFromBody kontrakcja

priceplanPage :: Kontrakcja m => m Response
priceplanPage = renderTemplate_ "priceplanPage" >>= renderFromBody kontrakcja

{- |
    Render a template as an entire page.
-}
renderTemplateAsPage :: Kontrakcja m => Context -> String -> Maybe (Lang -> KontraLink) -> Bool -> m String
renderTemplateAsPage ctx templateName mpubliclink showCreateAccount = renderTemplate templateName $ do
    contextInfoFields ctx
    mainLinksFields $ ctxlang ctx
    staticLinksFields $ ctxlang ctx
    langSwitcherFields ctx mpubliclink
    F.value "staticResources" $ SR.htmlImportList "systemPage" (ctxstaticresources ctx)
    F.value "showCreateAccount" $ showCreateAccount && (isNothing $ ctxmaybeuser ctx)
    F.value "versioncode" $ BS.toString $ B16.encode $ BS.fromString versionID

standardPageFields :: TemplatesMonad m => Context -> String -> Maybe (Lang -> KontraLink) -> Fields m ()
standardPageFields ctx title mpubliclink = do
  F.value "title" title
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
  F.value "linkpriceplan"  $ show $ LinkPriceplan lang

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
    