{-# OPTIONS_GHC -F -pgmFtrhsx -Wall#-}
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

import Control.Arrow (first)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import HSP hiding (Request)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.SimpleHTTP
import Kontra
import Misc
import KontraLink
import Data.Maybe
import Templates.Templates
import Control.Monad.Trans
import Data.List
import ListUtil
import Data.Functor
import API.Service.ServiceState
import FlashMessage

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
renderFromBody :: (EmbedAsChild (HSPT' IO) xml) 
               => TopMenu 
               -> String 
               -> xml 
               -> Kontra Response
renderFromBody topmenu title xml = do
    htmlPage <- fmap ((isSuffixOf ".html") . concat . rqPaths)  askRq
    loginOn <- getLoginOn
    loginreferer <- getLoginReferer
    ctx <- get
    let showCreateAccount = htmlPage && (isNothing $ ctxmaybeuser ctx) 
    res <- webHSP $ pageFromBody ctx loginOn loginreferer Nothing showCreateAccount title xml
    clearFlashMsgs
    return res
    

{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: (EmbedAsChild (HSPT' IO) xml) 
             => Context
             -> Bool
             -> Maybe String
             -> Maybe String
             -> Bool
             -> String 
             -> xml
             -> HSP XML
 
pageFromBody ctx@Context{ ctxmaybeuser
                        , ctxtemplates
                        }
             loginOn
             referer
             email
             showCreateAccount 
             title 
             body = do
    content <- liftIO $ renderHSPToString <div class="mainContainer"><% body %></div>
    wholePage <- liftIO $ renderTemplate ctxtemplates "wholePage" $ do
        field "content" content
        standardPageFields ctx title showCreateAccount loginOn referer email
    return $ cdata wholePage


embeddedPage :: String -> Kontra Response
embeddedPage pb = do
    ctx <- get
    bdy <- renderTemplateM "embeddedPage" $ do 
            field "content" pb
            serviceFields (ctxservice ctx)
            standardPageFields ctx "" False False Nothing Nothing
    res <- simpleResponse bdy 
    clearFlashMsgs
    return res
    
embeddedErrorPage :: Kontra Response
embeddedErrorPage = do
    service <- ctxservice <$> get
    content <- renderTemplateM "embeddedErrorPage" $ do
        serviceFields service
    simpleResponse content

serviceFields:: Maybe (Service,String) -> Fields
serviceFields (Just (_, location)) = field "location" location
serviceFields Nothing = field "location" "" -- should never happend 

sitemapPage :: Kontra String
sitemapPage = do
    ctx <- get
    liftIO $ renderTemplate (ctxtemplates ctx) "sitemapPage" $ do
        field "hostpart" $ case ctxhostpart ctx of
                                ('h':'t':'t':'p':'s':xs) -> "http" ++ xs
                                xs -> xs

priceplanPage :: Kontra String
priceplanPage = get >>= \ctx -> renderTemplateAsPage ctx "priceplanPage" True

securityPage :: Kontra String
securityPage = get >>= \ctx -> renderTemplateAsPage ctx "securityPage" True

legalPage :: Kontra String
legalPage = get >>= \ctx -> renderTemplateAsPage ctx "legalPage" True

privacyPolicyPage :: Kontra String
privacyPolicyPage = get >>= \ctx -> renderTemplateAsPage ctx "privacyPolicyPage" True

termsPage :: Kontra String
termsPage = get >>= \ctx -> renderTemplateAsPage ctx "termsPage" True

aboutPage :: Kontra String
aboutPage = get >>= \ctx -> renderTemplateAsPage ctx "aboutPage" True

partnersPage :: Kontra String
partnersPage = get >>= \ctx -> renderTemplateAsPage ctx "partnersPage" True

clientsPage :: Kontra String
clientsPage = get >>= \ctx -> renderTemplateAsPage ctx "clientsPage" True

{- |
    Render a template as an entire page.
-}
renderTemplateAsPage :: Context -> String -> Bool -> Kontra String
renderTemplateAsPage ctx@Context{ctxtemplates} templateName showCreateAccount = do
    loginOn <- getLoginOn
    loginreferer <- getLoginReferer
    let showCreateAccount2 = showCreateAccount && (isNothing $ ctxmaybeuser ctx)
    wholePage <- liftIO $ renderTemplate ctxtemplates templateName $ do
        standardPageFields ctx kontrakcja showCreateAccount2 loginOn loginreferer Nothing
    return wholePage

getLoginOn :: Kontra Bool
getLoginOn = do
    loginOn <- isFieldSet "logging"
    return loginOn   

getLoginReferer :: Kontra (Maybe String)
getLoginReferer = do
    curr <- rqUri <$> askRq
    referer <- getField "referer"
    qs <- querystring
    let loginreferer = Just $ fromMaybe (curr ++ qs) referer
    return loginreferer

standardPageFields :: Context -> String -> Bool -> Bool -> Maybe String -> Maybe String -> Fields
standardPageFields ctx title showCreateAccount loginOn referer email = do
    field "title" title
    field "showCreateAccount" showCreateAccount
    mainLinksFields 
    contextInfoFields ctx
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
simpleResponse s = ok $ toResponse $ cdata s 
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
    field "linkrequestaccount"   $ show LinkRequestAccount

{- |
   Defines some standard context information as fields handy for substitution
   into templates.
-}
contextInfoFields::Context -> Fields
contextInfoFields ctx = do
    field "logged" $ isJust (ctxmaybeuser ctx)
    field "flashmessages" $ for (ctxflashmessages ctx) flashMessageFields
    field "protocol" $ if (ctxproduction ctx) then "https:" else "http:"
    field "prefix" ""
    field "production" (ctxproduction ctx)
    field "publicsafe" (isNothing $ ctxmaybeuser ctx)


flashMessageFields fm = do
    field "type" $ 
        case fst (unFlashMessage fm) of
            SigningRelated  -> "blue"
            OperationDone   -> "green"
            OperationFailed -> "red" 
            _               -> ""
    field "message" $ snd (unFlashMessage fm)   
    field "isModal" $ fst (unFlashMessage fm) == Modal
    
loginModal::Bool -> Maybe String -> Maybe String -> Fields
loginModal on referer email = do
    field "loginModal" $ on
    field "referer" $ referer
    field "email"   $ email

modalError :: KontrakcjaTemplates -> KontraModal
modalError templates =
    lift $ renderTemplate templates "modalError" ()
