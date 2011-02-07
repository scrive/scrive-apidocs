{-# OPTIONS_GHC -F -pgmFtrhsx -Wall#-}
module AppView( TopMenu(..)
              , kontrakcja
              , htmlHeadBodyWrapIO
              , poweredBySkrivaPaPara
              -- , loginBox
              , pageErrorReport
              , renderFromBody
              , pageForgotPassword
              , pageForgotPasswordConfirm
              , signupPageView
              , signupConfirmPageView
              , pageLogin
              , pageFromBody'
              , uploadPage
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

{-
loginBox :: (EmbedAsAttr m (Attr [Char] [Char]),EmbedAsAttr m (Attr [Char] KontraLink)) => Maybe String -> XMLGenT m (HSX.XMLGenerator.XML m)
loginBox referer=
   <div>
    <div id="login">
     <form action=LinkLogin method="post">
      <table>
	<tr>
          <td>E-mail:</td> 
          <td><input type="email" name="email" autocomplete="off" class="noflash"/></td> 
        </tr>
	<tr> 
          <td>Lösenord:</td> 
          <td><input type="password" name="password" autocomplete="off"/></td> 
        </tr>
    <tr>
          <td>
          </td>
          
          <td style="display: none">
            <input type="checkbox" id="rememberme" name="rememberme"/>
            <label for="rememberme">Kom ihåg mig</label>
          </td>
    </tr>
	<tr> 
          <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in"/>
              <input type="hidden" name="referer" value=(fromMaybe "" referer)/>
           </td>
          <td>
           <a href=LinkForgotPassword> Glömt lösenord</a>
          </td>
	</tr>
      </table>
    </form>
    </div>
   </div>
-}


pageErrorReport :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
            => Context 
            -> Request 
            -> XMLGenT m (HSX.XML m)
pageErrorReport (Context {ctxmaybeuser}) request = 
  <div>
   <p>Ett fel har uppstått. Det beror inte på dig. Det beror på oss. Vi tar 
      hand om problemet så snart vi kan. Tills vi fixat problemet, vänligen 
      försök igen genom att börja om från <a href="/">startsidan</a>.</p>
   <hr/>
   <p>Information useful to developers:</p>
   <% case ctxmaybeuser of
           Just user -> <p>Logged in as: <% user %></p>
           Nothing -> <p>Not logged in</p>
   %>
   <p><% request %></p>
   <p>HTTP Headers:</p>
   <p><% Map.elems (rqHeaders request) %></p> 
   <p>HTTP Cookies:</p>
   <p><% map show $ rqCookies request %></p> 
  </div>  


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
              Context { ctxmaybeuser
                      , ctxflashmessages
                      , ctxproduction
                      , ctxtemplates
                      }
                  topMenu title body = do
                    content <- liftIO $ renderHSPToString <div id="mainContainer"><% body %></div>
                    wholePage <- liftIO $ renderTemplateComplex ctxtemplates "wholePage" $
                                 (setAttribute "production" ctxproduction) .
                                 (setAttribute "uploadTab" $ uploadTabInfo ctxmaybeuser topMenu) .
                                 (setAttribute "documentTab" $ documentTabInfo ctxmaybeuser topMenu) .
                                 (setAttribute "content" content) .
                                 (setAttribute "prefix" prefix) .
                                 (setAttribute "flashmessages" (map (BSC.toString . unFlashMessage) ctxflashmessages)) .
                                 (if prefix=="" then id else setAttribute "protocol" "http:") .
                                 (setAttribute "title" title) .
                                 (maybe (setAttribute "userfullname" False) 
                                        (\user -> setAttribute "userfullname" (userfullname user)) 
                                        ctxmaybeuser)
                    return $ cdata wholePage


signupConfirmPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) =>  XMLGenT m (HSX.XML m)
signupConfirmPageView  =  <div>Ditt konto har skapats! Vi har skickat ett mail med dina användaruppgifter till din inkorg.</div>
        
signupPageView :: KontrakcjaTemplates -> IO String
signupPageView templates = renderTemplate templates "signupPageView" []

pageForgotPassword :: KontrakcjaTemplates -> IO String
pageForgotPassword templates = do
  renderTemplateComplex templates "pageForgotPassword" $
                            id

pageForgotPasswordConfirm :: KontrakcjaTemplates -> IO String
pageForgotPasswordConfirm templates = do
  renderTemplateComplex templates "pageForgotPasswordConfirm" $
                            id

pageLogin :: Context -> Maybe String -> IO String
pageLogin ctx referer = do
  renderTemplateComplex (ctxtemplates ctx) "pageLogin" $
                            (setAttribute "referer" referer)


uploadPage:: KontrakcjaTemplates -> IO String
uploadPage templates = do
                   uploadBox <- liftIO $ renderTemplate templates "uploadPageContent" []
                   liftIO $ renderTemplateComplex templates "creatingDocumentFrame" $ 
                                                       (setAttribute  "steps" uploadBox) .
                                                       (setAttribute  "step1" True) .
                                                       (setAttribute  "submitFormOnNext" True) 
                                                       
               


uploadTabInfo::Maybe User -> TopMenu ->  Maybe (String,Bool)
uploadTabInfo Nothing _= Nothing
uploadTabInfo _ menu = Just (show LinkMain,(menu== TopNew))

documentTabInfo::Maybe User -> TopMenu -> Maybe (String,Bool)
documentTabInfo Nothing _ = Nothing
documentTabInfo _ menu = Just (show LinkIssue,(menu== TopDocument))
