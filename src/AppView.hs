{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
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
import API.Service.ServiceState
import FlashMessage
import Kontra
import KontraLink
import ListUtil
import Misc

import Control.Monad.State (get)
import Control.Monad.Trans
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server.SimpleHTTP
import Templates.Templates
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
kontrakcja = "SkrivaPå" 

-- * Main Implementation

{- |
   Renders some page body xml into a complete reponse
-}
renderFromBody :: TopMenu 
               -> String 
               -> String
               -> Kontra Response
renderFromBody _topmenu title content = do
    htmlPage <- fmap ((isSuffixOf ".html") . concat . rqPaths)  askRq
    loginOn <- getLoginOn
    loginreferer <- getLoginReferer
    ctx <- get
    let showCreateAccount = htmlPage && (isNothing $ ctxmaybeuser ctx) 
    res <-  simpleResponse =<< (liftIO $ pageFromBody ctx loginOn loginreferer Nothing showCreateAccount title content)
    clearFlashMsgs
    return res
    

{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: Context
             -> Bool
             -> Maybe String
             -> Maybe String
             -> Bool
             -> String 
             -> String
             -> IO String
 
pageFromBody ctx@Context{ ctxtemplates }
             loginOn
             referer
             email
             showCreateAccount 
             title 
             bodytext = do
    renderTemplate ctxtemplates "wholePage" $ do
        field "content" bodytext
        standardPageFields ctx title False showCreateAccount loginOn referer email
    


embeddedPage :: String -> Kontra Response
embeddedPage pb = do
    ctx <- get
    bdy <- renderTemplateM "embeddedPage" $ do 
            field "content" pb
            serviceFields (ctxservice ctx) (ctxlocation ctx)
            standardPageFields ctx "" False False False Nothing Nothing
    res <- simpleResponse bdy 
    clearFlashMsgs
    return res
    
embeddedErrorPage :: Kontra Response
embeddedErrorPage = do
    ctx <- get
    content <- renderTemplateM "embeddedErrorPage" $ do
        serviceFields (ctxservice ctx) (ctxlocation ctx)
    simpleResponse content

serviceFields:: Maybe Service -> String -> Fields
serviceFields _ location = field "location" location


sitemapPage :: Kontra String
sitemapPage = do
    ctx <- get
    liftIO $ renderTemplate (ctxtemplates ctx) "sitemapPage" $ do
        field "hostpart" $ case ctxhostpart ctx of
                                ('h':'t':'t':'p':'s':xs) -> "http" ++ xs
                                xs -> xs

priceplanPage :: Kontra String
priceplanPage = get >>= \ctx -> renderTemplateAsPage ctx "priceplanPage" True True

securityPage :: Kontra String
securityPage = get >>= \ctx -> renderTemplateAsPage ctx "securityPage" True True

legalPage :: Kontra String
legalPage = get >>= \ctx -> renderTemplateAsPage ctx "legalPage" True True

privacyPolicyPage :: Kontra String
privacyPolicyPage = get >>= \ctx -> renderTemplateAsPage ctx "privacyPolicyPage" True True

termsPage :: Kontra String
termsPage = get >>= \ctx -> renderTemplateAsPage ctx "termsPage" True True

aboutPage :: Kontra String
aboutPage = get >>= \ctx -> renderTemplateAsPage ctx "aboutPage" True True

partnersPage :: Kontra String
partnersPage = get >>= \ctx -> renderTemplateAsPage ctx "partnersPage" True True

clientsPage :: Kontra String
clientsPage = get >>= \ctx -> renderTemplateAsPage ctx "clientsPage" True True

{- |
    Render a template as an entire page.
-}
renderTemplateAsPage :: Context -> String -> Bool -> Bool -> Kontra String
renderTemplateAsPage ctx@Context{ctxtemplates} templateName publicpage showCreateAccount = do
    loginOn <- getLoginOn
    loginreferer <- getLoginReferer
    let showCreateAccount2 = showCreateAccount && (isNothing $ ctxmaybeuser ctx)
    wholePage <- liftIO $ renderTemplate ctxtemplates templateName $ do
        standardPageFields ctx kontrakcja publicpage showCreateAccount2 loginOn loginreferer Nothing
    return wholePage

getLoginOn :: Kontra Bool
getLoginOn = do
    loginOn <- isFieldSet "logging"
    return loginOn   

getLoginReferer :: Kontra (Maybe String)
getLoginReferer = do
    curr <- rqUri <$> askRq
    referer <- getField "referer"
    qstr <- querystring
    let loginreferer = Just $ fromMaybe (curr ++ qstr) referer
    return loginreferer

standardPageFields :: Context -> String -> Bool -> Bool -> Bool -> Maybe String -> Maybe String -> Fields
standardPageFields ctx title publicpage showCreateAccount loginOn referer email = do
    field "title" title
    field "showCreateAccount" showCreateAccount
    mainLinksFields 
    contextInfoFields ctx
    publicSafeFlagField ctx loginOn publicpage
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
pageLogin :: Context -> Maybe String -> Maybe String -> IO String
pageLogin ctx referer email =
  renderTemplate (ctxtemplates ctx) "pageLogin" $ do
      field "referer" referer
      field "email" email

{- |
   Changing our pages into reponses, and clearing flash messages.
-}
simpleResponse::String -> Kontra Response
simpleResponse s = ok $ toResponseBS   (BS.fromString "text/html;charset=utf-8") (BSL.fromString s) 
    -- change this to HtmlString from helpers package 
    -- (didn't want to connect it one day before prelaunch)

ajaxError::Kontra Response
ajaxError = simpleResponse "<script>window.location='/'</script>"
{- |
   The landing page contents.  Read from template.
-}
firstPage :: Context -> Bool -> Maybe String -> Maybe String ->  IO String
firstPage ctx loginOn referer email = 
    renderTemplate (ctxtemplates ctx) "firstPage"  $ do 
        contextInfoFields ctx
        publicSafeFlagField ctx loginOn True
        mainLinksFields
        loginModal loginOn referer email

{- |
   Defines the main links as fields handy for substituting into templates.
-}
mainLinksFields::Fields 
mainLinksFields = do
    field "linkaccount"          $ show LinkAccount
    field "linkforgotenpassword" $ show LinkForgotPassword
    field "linkinvite"           $ show LinkInvite
    field "linkissue"            $ show (LinkContracts emptyListParams)
    field "linklogin"            $ show (LinkLogin LoginTry)
    field "linklogout"           $ show LinkLogout
    field "linkmain"             $ show LinkMain
    field "linkquestion"         $ show LinkAskQuestion
    field "linksignup"           $ show LinkSignup

{- |
   Defines some standard context information as fields handy for substitution
   into templates.
-}
contextInfoFields::Context -> Fields
contextInfoFields ctx = do
    field "logged" $ isJust (ctxmaybeuser ctx)
    field "flashmessages" $ map (flashMessageFields $ ctxtemplates ctx) (ctxflashmessages ctx)
    field "protocol" $ if (ctxproduction ctx) then "https:" else "http:"
    field "prefix" ""
    field "production" (ctxproduction ctx)
{- |
    Only public safe is explicitely set as a public page,
    and nobody is trying to login, and a user isn't logged in.
    The flag means that things like snoobi won't be contacted.
-}
publicSafeFlagField :: Context -> Bool -> Bool -> Fields
publicSafeFlagField ctx loginOn publicpage = do
    field "publicsafe" $ publicpage 
                           && (not loginOn) 
                           && (isNothing $ ctxmaybeuser ctx)

flashMessageFields :: KontrakcjaTemplates -> FlashMessage -> Fields
flashMessageFields templates flash = do
    field "type" $ (\t ->
        case t of
             SigningRelated  -> "blue"
             OperationDone   -> "green"
             OperationFailed -> "red" 
             _               -> "") <$> ftype
    field "message" msg
    field "isModal" $ (== Modal) <$> ftype
    where
        fm = fromJust . unFlashMessage <$> instantiate templates flash
        ftype = fst <$> fm
        msg = snd <$> fm

loginModal::Bool -> Maybe String -> Maybe String -> Fields
loginModal on referer email = do
    field "loginModal" $ on
    field "referer" $ referer
    field "email"   $ email

modalError :: KontrakcjaTemplates -> KontraModal
modalError templates =
    lift $ renderTemplate templates "modalError" ()
