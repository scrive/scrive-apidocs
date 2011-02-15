{-# OPTIONS_GHC -F -pgmFtrhsx -Wall#-}
module AppView( TopMenu(..)
              , kontrakcja
              , htmlHeadBodyWrapIO
              , poweredBySkrivaPaPara
              -- , loginBox
              , renderFromBody
              , pageForgotPassword
              , pageForgotPasswordConfirm
              , signupPageView
              , signupConfirmPageView
              , pageLogin
              , pageFromBody'
              , simpleResponse 
              , firstPage
              , staticTemplate
              ) where 

import HSP hiding (Request)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.SimpleHTTP
import qualified HSX.XMLGenerator as HSX (XML)
import qualified HSX.XMLGenerator
import qualified Data.ByteString.UTF8 as BSC
import Kontra
import qualified Data.Map as Map
import Misc
import KontraLink
import Data.Maybe
import Templates.Templates
import Control.Monad.Trans

poweredBySkrivaPaPara :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
poweredBySkrivaPaPara hostpart = 
    <p>
     <small>Med vänliga hälsningar<%"\n"%><br/><a href=hostpart>SkrivaPå</a></small>
    </p>

htmlHeadBodyWrap :: (XMLGenerator m,EmbedAsChild m a {- ,EmbedAsChild m b -})
                 => a
                 -> XMLGenT m (HSX.XMLGenerator.XML m) --b
                 -> XMLGenT m (HSX.XMLGenerator.XML m)
htmlHeadBodyWrap title content =     
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
      <title><% title %></title>
     </head>
     <body>
      <% content %>
     </body>
    </html>
htmlHeadBodyWrapIO :: (EmbedAsChild (HSPT' IO) a) => a -> XMLGenT (HSPT' IO) (HSX.XMLGenerator.XML (HSPT' IO))   -> IO BSC.ByteString
htmlHeadBodyWrapIO title content = do
  let xml = htmlHeadBodyWrap title content 
  renderHSPToByteString xml

data TopMenu = TopNew | TopDocument | TopAccount | TopNone | TopEmpty
             deriving (Eq,Ord)

kontrakcja :: [Char]
kontrakcja = "SkrivaPå" 

-- * Main Implementation


renderFromBody :: (EmbedAsChild (HSPT' IO) xml) 
               => Context 
               -> TopMenu 
               -> String 
               -> xml 
               -> Kontra Response
renderFromBody ctx topmenu title xml = do
                                        res <- webHSP $ pageFromBody ctx topmenu title xml
                                        clearFlashMsgs
                                        return res

topnavi :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
        => Bool 
        -> String 
        -> KontraLink 
        -> XMLGenT m (HSX.XML m)
topnavi active title link = 
    <a href=link class=(if active then "active" else "")><% title %></a>


pageFromBody :: (EmbedAsChild (HSPT' IO) xml) 
             =>  Context 
             -> TopMenu 
             -> String 
             -> xml
             -> HSP XML
pageFromBody = pageFromBody' ""

pageFromBody' :: (EmbedAsChild (HSPT' IO) xml) 
              => String
              -> Context 
              -> TopMenu 
              -> String 
              -> xml
              -> HSP XML
pageFromBody' prefix 
              ctx@Context { ctxmaybeuser
                      , ctxflashmessages
                      , ctxproduction
                      , ctxtemplates
                      }
                  topMenu title body = do
                    content <- liftIO $ renderHSPToString <div id="mainContainer"><% body %></div>
                    wholePage <- liftIO $ renderTemplate ctxtemplates "wholePage" $ do
                                  field "production" ctxproduction
                                  field "uploadTab" $ uploadTabInfo ctxmaybeuser topMenu
                                  field "documentTab" $ documentTabInfo ctxmaybeuser topMenu
                                  field "content" content
                                  field "prefix" prefix
                                  field "protocol" $ if prefix=="" then "" else "http:"
                                  field "title" title
                                  field "userfullname" $ fmap userfullname ctxmaybeuser
                                  mainLinksFields 
                                  contextInfoFields ctx
                    return $ cdata wholePage


signupConfirmPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) =>  XMLGenT m (HSX.XML m)
signupConfirmPageView  =  <div>Ditt konto har skapats! Vi har skickat ett mail med dina användaruppgifter till din inkorg.</div>
        
signupPageView :: KontrakcjaTemplates -> IO String
signupPageView templates = renderTemplate templates "signupPageView" ()

pageForgotPassword :: KontrakcjaTemplates -> IO String
pageForgotPassword templates = do
  renderTemplate templates "pageForgotPassword" ()

pageForgotPasswordConfirm :: KontrakcjaTemplates -> IO String
pageForgotPasswordConfirm templates = do
  renderTemplate templates "pageForgotPasswordConfirm"()

pageLogin :: Context -> Maybe String -> IO String
pageLogin ctx referer = do
  renderTemplate (ctxtemplates ctx) "pageLogin" $
                            (setAttribute "referer" referer)



                                                       
               


uploadTabInfo::Maybe User -> TopMenu ->  Maybe (String,Bool)
uploadTabInfo Nothing _= Nothing
uploadTabInfo _ menu = Just (show LinkMain,(menu== TopNew))

documentTabInfo::Maybe User -> TopMenu -> Maybe (String,Bool)
documentTabInfo Nothing _ = Nothing
documentTabInfo _ menu = Just (show LinkIssue,(menu== TopDocument))

simpleResponse::String -> Kontra Response
simpleResponse s = do 
                   res <- ok $ toResponse $ cdata s -- change this to HtmlString from helpers package (didn't wan't to connect it one day before prelaunch)
                   clearFlashMsgs
                   return res

firstPage::Context-> IO String
firstPage ctx =  renderTemplate (ctxtemplates ctx) "firstPage"  $ do 
                              contextInfoFields ctx
                              mainLinksFields
                              
staticTemplate::Context -> Bool ->  String -> IO String
staticTemplate ctx nocolumns content = renderTemplate (ctxtemplates ctx) "staticTemplate"  $ do 
                              contextInfoFields ctx
                              mainLinksFields
                              field "content" $ content
                              field "nocolumns" nocolumns


mainLinksFields::Fields 
mainLinksFields = do
                     field "linklogin" $ show LinkLogin 
                     field "linklogout" $ show LinkLogout
                     field "linkforgotenpassword" $ show LinkForgotPassword
                     field "linkrequestaccount" $ show LinkRequestAccount
                     field "linkquestion" $ show LinkAskQuestion
                     field "linkaccount" $ show LinkAccount
                     field "linkmain" $ show LinkMain
                     field "linkissue" $ show LinkIssue
                     field "linkinvite" $ show LinkInvite
                     


contextInfoFields::Context -> Fields
contextInfoFields ctx = do 
                         field "logged" $ isJust (ctxmaybeuser ctx)
                         field "flashmessages" $ map (BSC.toString . unFlashMessage) (ctxflashmessages ctx)
             