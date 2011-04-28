{-# OPTIONS_GHC -F -pgmFtrhsx -Wall#-}
{- |
   Defines the App level views.
-}
module AppView( TopMenu(..)
              , kontrakcja
              , renderFromBody
              , renderFromBodyWithQueryColumn
              , signupPageView
              , signupVipPageView
              , pageLogin
              , simpleResponse 
              , ajaxError
              , firstPage
              , modalError
              ) where 

import Control.Arrow (first)
import Control.Monad.Reader (ask)
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
               => Context 
               -> TopMenu 
               -> String 
               -> xml 
               -> Kontra Response
renderFromBody = renderFromBody' False

{- |
    Renders some page body xml into a complete response
    alongside a question form in the left hand columns.
-}
renderFromBodyWithQueryColumn :: (EmbedAsChild (HSPT' IO) xml) 
               => Context 
               -> TopMenu 
               -> String 
               -> xml 
               -> Kontra Response
renderFromBodyWithQueryColumn = renderFromBody' True


renderFromBody' :: (EmbedAsChild (HSPT' IO) xml) 
               => Bool 
               -> Context 
               -> TopMenu 
               -> String
               -> xml 
               -> Kontra Response
renderFromBody' columns ctx topmenu title xml = do
    htmlPage <- fmap ((isSuffixOf ".html") . concat . rqPaths)  askRq
    loginOn <- isFieldSet "logging"
    curr <- rqUri <$> askRq
    referer <- getField "referer"
    qs <- querystring
    liftIO $ print curr
    liftIO $ print referer
    let loginreferer = Just $ fromMaybe (curr ++ qs) referer
    let showCreateAccount = (not columns) && htmlPage && (isNothing $ ctxmaybeuser ctx) 
    res <- webHSP $ pageFromBody ctx columns loginOn loginreferer Nothing showCreateAccount title xml
    clearFlashMsgs
    return res
    

{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: (EmbedAsChild (HSPT' IO) xml) 
             =>  Context 
             -> Bool
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
             columns 
             loginOn
             referer
             email
             showCreateAccount 
             title 
             body = do
    content <- liftIO $ renderHSPToString <div class="mainContainer"><% body %></div>
    wholePage <- liftIO $ renderTemplate ctxtemplates "wholePage" $ do
        field "content" content
        field "title" title
        field "columns" columns
        field "showCreateAccount" showCreateAccount
        mainLinksFields 
        contextInfoFields ctx
        loginModal loginOn referer email
    return $ cdata wholePage

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
