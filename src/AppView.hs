{-# OPTIONS_GHC -F -pgmFtrhsx -Wall#-}
{- |
   Defines the App level views.
-}
module AppView( TopMenu(..)
              , kontrakcja
              , renderFromBody
              , pageForgotPassword
              , pageForgotPasswordConfirm
              , signupPageView
              , signupVipPageView
              , pageLogin
              , simpleResponse 
              , firstPage
              ) where 

import Control.Arrow (first)
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
{- |
   Defines the different sorts of things we can have at the top of the page
-}
data TopMenu = TopNew | TopDocument | TopAccount | TopNone | TopEmpty
             deriving (Eq,Ord)

{- |
   The name of our application (the codebase is known as kontrakcja,
   and this is the pretty public name)
-}
kontrakcja :: [Char]
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
renderFromBody ctx topmenu title xml = do
                                        htmlPage <- fmap ((isSuffixOf ".html") . concat . rqPaths)  askRq
                                        let showCreateAccount = htmlPage && (isNothing $ ctxmaybeuser ctx)
                                        columns <- isFieldSet "columns"
                                        res <- webHSP $ pageFromBody ctx columns showCreateAccount title xml
                                        clearFlashMsgs
                                        return res
{- |
   Renders some page body xml into a complete page of xml
-}
pageFromBody :: (EmbedAsChild (HSPT' IO) xml) 
             =>  Context 
             -> Bool
             -> Bool
             -> String 
             -> xml
             -> HSP XML
pageFromBody ctx@Context { ctxmaybeuser
                      , ctxtemplates
                      }
             columns showCreateAccount title body = do
                    content <- liftIO $ renderHSPToString <div class="mainContainer"><% body %></div>
                    wholePage <- liftIO $ renderTemplate ctxtemplates "wholePage" $ do
                                  field "content" content
                                  field "title" title
                                  field "columns" columns
                                  field "showCreateAccount" showCreateAccount
                                  mainLinksFields 
                                  contextInfoFields ctx
                    return $ cdata wholePage

{- |
   The contents of the signup page.  This is read from a template.
-}        
signupPageView :: KontrakcjaTemplates -> IO String
signupPageView templates = renderTemplate templates "signupPageView" ()

signupVipPageView :: KontrakcjaTemplates -> IO String
signupVipPageView templates = renderTemplate templates "signupVipPageView" ()

{- |
   The contents of the password reset page.  This is read from a template.
-} 

pageForgotPassword :: KontrakcjaTemplates -> IO String
pageForgotPassword templates = do
  renderTemplate templates "pageForgotPassword" ()

{- |
   The contents of the password reset confirmation.  This is read from a template.
-}
pageForgotPasswordConfirm :: KontrakcjaTemplates -> IO String
pageForgotPasswordConfirm templates = do
  renderTemplate templates "pageForgotPasswordConfirm" ()

{- |
   The contents of the login page.  This is read from a template.
-}
pageLogin :: Context -> Maybe String -> Maybe String -> IO String
pageLogin ctx referer email =
  renderTemplate (ctxtemplates ctx) "pageLogin" $
      setAttribute "referer" referer
    . setAttribute "email" email

{- |
   Changing our pages into reponses, and clearing flash messages.
-}
simpleResponse::String -> Kontra Response
simpleResponse s = do 
                   res <- ok $ toResponse $ cdata s -- change this to HtmlString from helpers package (didn't want to connect it one day before prelaunch)
                   clearFlashMsgs
                   return res

{- |
   The landing page contents.  Read from template.
-}
firstPage::Context-> IO String
firstPage ctx =  renderTemplate (ctxtemplates ctx) "firstPage"  $ do 
                              contextInfoFields ctx
                              mainLinksFields

{- |
   Defines the main links as fields handy for substituting into templates.
-}
mainLinksFields::Fields 
mainLinksFields = do
                     field "linklogin" $ show (LinkLogin NoReason)
                     field "linklogout" $ show LinkLogout
                     field "linkforgotenpassword" $ show LinkForgotPassword
                     field "linkrequestaccount" $ show LinkRequestAccount
                     field "linkquestion" $ show LinkAskQuestion
                     field "linkaccount" $ show LinkAccount
                     field "linkmain" $ show LinkMain
                     field "linkissue" $ show $ LinkContracts emptyListParams
                     field "linkinvite" $ show LinkInvite

{- |
   Defines some standard context information as fields handy for substitution
   into templates.
-}
contextInfoFields::Context -> Fields
contextInfoFields ctx = do
                         field "logged" $ isJust (ctxmaybeuser ctx)
                         field "flashmessages" $ for (ctxflashmessages ctx) $ \fm -> do
                             field "type" $ case fst (unFlashMessage  fm) of
                                 SigningRelated -> "blue"
                                 OperationDone -> "green"
                                 OperationFailed -> "red" 
                             field "message" $ snd (unFlashMessage fm)
                         field "protocol" $ if (ctxproduction ctx) then "https:" else "http:"
                         field "prefix" ""
                         field "production" (ctxproduction ctx)
                         
