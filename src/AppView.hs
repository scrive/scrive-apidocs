{-# LANGUAGE FlexibleContexts, FlexibleInstances, IncoherentInstances,
             MultiParamTypeClasses, NamedFieldPuns, CPP #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module AppView( TopMenu(..),kontrakcja,htmlHeadBodyWrapIO,poweredBySkrivaPaPara,loginBox,
welcomeBody,errorReport,renderFromBody,forgotPasswordPageView,forgotPasswordConfirmPageView,signupPageView,
SignupForm,signupEmail,signupPassword,signupPassword2,databaseContents,showAdminOnly,pageAllUsersTable,
signupFirstname, signupLastname, signupConfirmPageView, loginPageView,statsPageView) where

import HSP hiding (Request)
import System.Locale (defaultTimeLocale)
import Happstack.Server (Response)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.SimpleHTTP
import qualified HSX.XMLGenerator as HSX (XML)
import qualified HSX.XMLGenerator
import Data.Object.Json as Json
import Data.Object as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSCL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.UTF8 as BSC
import User
import Network.HTTP (urlEncode)
import Data.Time
import qualified Data.Map as Map
import Misc
import HSP.XML
import KontraLink
import System.Directory
import Happstack.State (update,query)
import Data.Data
import "mtl" Control.Monad.Trans

poweredBySkrivaPaPara :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
poweredBySkrivaPaPara hostpart = 
    <p>
     <small>Med vänliga hälsningar<br/>
     <a href=hostpart>SkrivaPå</a></small>
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

htmlHeadBodyWrapIO title content = do
  let xml = htmlHeadBodyWrap title content
  renderHSPToByteString xml

daveShowGeneric :: (Data d,XMLGenerator m) => d -> GenChildList m
daveShowGeneric obj = do
    let con = toConstr obj
    let s = constrFields con
    <% show con %> 
    <% <ul>
         <% case constrRep con of
              AlgConstr {} ->  zipWith ($) (gmapQ (\x -> \k -> <li><% k %>: <% daveShowGeneric x %></li>) obj) 
                    (constrFields con)
              _ -> gmapQ (\x -> <li><% daveShowGeneric x %></li>) obj 
          %>
       </ul>
     %>

instance (XMLGenerator m) => (EmbedAsChild m HeaderPair) where
  asChild (HeaderPair name value) = 
     <% <p> 
         <% BS.toString name ++ ": " ++ show value  %> 
        </p> 
      %>	


data TopMenu = TopNew | TopDocument | TopAccount | TopNone | TopEmpty
             deriving (Eq,Ord)

instance (XMLGenerator m) => (EmbedAsChild m User) where
  asChild user = <% BS.toString (userfullname user) ++ " <" ++ 
                 BS.toString (unEmail $ useremail user) ++ ">"  %>	

#if MIN_VERSION_happstack_server(0,5,1)
rqInputs rq = rqInputsQuery rq ++ rqInputsBody rq
#endif

instance (XMLGenerator m) => (EmbedAsChild m Request) where
  asChild rq = <% <code>
                  <% show (rqMethod rq) %> <% rqUri rq ++ rqQuery rq %><br/>
                  <% map asChild1 (rqInputs rq) %>
                 </code> %>
    where asChild1 (name,input) = 
              <% <span><% name %>: <% input %><br/></span> %>

instance (XMLGenerator m) => (EmbedAsChild m Input) where
  asChild (Input _value (Just filename) _contentType) = 
       <% "File " ++ show filename %>
  asChild (Input value _maybefilename _contentType) = 
       <% show (concatMap BSC.toString (BSCL.toChunks value)) %>

instance (XMLGenerator m) => (EmbedAsChild m Json.JsonScalar) where
  asChild (JsonString x) = <% BS.toString x %>	
  asChild (JsonNumber x) = <% show x %>	
  asChild (JsonBoolean x) = <% show x %>
  asChild JsonNull = <% "null" %>

s (k, v) = <li><% BS.toString k %>: <% v %></li>

y v = <li><% v %></li>



instance (EmbedAsChild m HSP.XML.XML) => (EmbedAsChild m FlashMessage) where
  asChild (FlashMessage msg) = 
      <% cdata $ BSC.toString msg %>


instance (XMLGenerator m) => 
    (EmbedAsChild m (Json.Object BS.ByteString Json.JsonScalar)) where
  asChild (Json.Mapping xs) =
    <%
      <ul>
        <% map s xs %>
      </ul>
    %>
  asChild (Json.Sequence xs) =
    <%
     <ol>
      <% map y xs %>
     </ol>
    %>
  asChild (Json.Scalar val) =
    <%
     <span><% val %></span>
    %>

kontrakcja :: [Char]
kontrakcja = "SkrivaPå"

-- small letter ascii only version on app name
kontrakcjaAscii :: [Char]
kontrakcjaAscii = "skriva"


loginBox ctx =
   <div>
    <div id="login">
     <form action=LinkLogin method="post">
      <table>
	<tr>
          <td>E-mail:</td> 
          <td><input type="email" name="email" autocomplete="off"/></td> 
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
          <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in"/></td>
          <td>
           <a href=LinkForgotPassword> Glömt lösenord</a>
          </td>
	</tr>
      </table>
    </form>
    </div>
    {-
    <div id="register" style="display: none;">
     <form action="/tutorial/actions/newuser" method="post">
      <table>
 	<tr>
          <td>Email:</td>
          <td><input type="textfield" name="email" class="emailvalidation"/></td>
        </tr>
	<tr>
          <td>Password:</td>
          <td><input type="password" name="password"/></td>
        </tr>
	<tr>
          <td>Verify Password:</td>
          <td><input type="password" name="password2"/></td>
        </tr>
	<tr>
          <td><input type="submit" name="create" value="Create Account"/></td>
          <td><a href="#" onClick="$('#register').hide(); $('#login').show(); return false;">login</a></td>
        </tr>
      </table>
     </form>
    </div>
    -}
   </div>

welcomeBody :: Context 
            -> XMLGenT (HSPT' IO)  (HSX.XML (HSPT' IO) )
welcomeBody (Context {ctxmaybeuser = Just _, ctxhostpart}) = 
  <div class="centerdivnarrow" style="margin-top: 100px;">
   
   <form action=LinkIssue method="post" enctype="multipart/form-data">
    <span class="small">Välj dokument (endast PDF)</span><br/>
    <input class="multi" maxlength="1" type="file" name="doc" accept="pdf"/>
    <input class="bigbutton" type="submit" value="Ladda upp"/>
   </form>
  </div>


welcomeBody ctx@(Context {ctxmaybeuser = Nothing}) = 
 <div>
    <div id="firstPageLeft">
         <h1>Snabbt, smidigt och säkert</h1>
         <p>
                            SkrivaPå är en tjänst för elektronisk signering och arkivering. 
                            Spara tid och minska onödiga kostnader med enklare och smidigare avtalshantering. 
                            Kontakta oss redan idag för att testa tjänsten.
         </p>
    </div>
    <div id="firstPageRight">
         <object>
             <embed src="https://secure.vimeo.com/moogaloop.swf?clip_id=15894469&server=vimeo.com&show_title=0&show_byline=0&show_portrait=0&color=00ADEF&fullscreen=1" type="application/x-shockwave-flash" allowfullscreen="true" allowscriptaccess="always" width="520" height="319">
             </embed>
         </object>
  
        <p> </p>
     </div>
 </div>


errorReport :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
            => Context 
            -> Request 
            -> XMLGenT m (HSX.XML m)
errorReport (Context {ctxmaybeuser}) request = 
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

-- * Convenience Functions

dateStr :: UTCTime -> String
dateStr ct =
  formatTime
    defaultTimeLocale
    "%a, %B %d, %Y at %H:%M:%S (UTC)" ct

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
        -> Context
        -> String 
        -> KontraLink 
        -> XMLGenT m (HSX.XML m)
topnavi active ctx title link = 
    <a href=link class=(if active then "active" else "")><% title %></a>

#define _DATESTR(x) #x
#define DATESTR _DATESTR(__DATE__)

globalScriptsAndStyles = 
      [ <link rel="stylesheet" type="text/css" href=("/theme/style.css?"  ++ __DATE__) media="screen" />
      , <link rel="stylesheet" type="text/css" 
            href="//ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/ui-lightness/jquery-ui.css" 
            -- href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/flick/jquery-ui.css"
            -- href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/redmond/jquery-ui.css"
            -- href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/start/jquery-ui.css"
            media="screen" />
      , <script src="//ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.js"/>
      -- we loaded the min version but at some point google stopped serving this one
      -- , <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"/>
      , <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/jquery-ui.min.js"/>
      {- Local versions of the same, but locally
      , <script src="/js/jquery-1.4.2.min.js"/>
      , <script src="/js/jquery-ui-1.8.custom.min.js"/>
      -}
      , <script src="/js/jquery.MultiFile.js"/>
      , <script src=("/js/global.js?" ++ __DATE__)/>
      ]

pageFromBody :: (EmbedAsChild (HSPT' IO) xml) 
             => Context 
             -> TopMenu 
             -> String 
             -> xml 
             -> HSP XML
pageFromBody ctx@(Context {ctxmaybeuser,ctxhostpart,ctxflashmessages}) 
             topMenu title body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <% globalScriptsAndStyles %>
     </head>
     <body>
     <div id="headerWide"/>
     <div id="mainContainer960">
     <%
       case ctxflashmessages of
         [] -> <% () %>
         _ -> <% <div class="flashmsgbox">
               <% ctxflashmessages %>
              </div> %>
       %>
      <div id="headerContainer">
      <a href="">
        <img id="logosmall" src="/theme/images/logosmall.png" alt="Liten logga"/>
       </a> 
  

           <% case ctxmaybeuser of
             Just User{userfullname} -> 
                 <span id="userMenu"><% userfullname %> | <a href="account">Konto</a> | <a href="/logout">Logga ut</a></span>
             Nothing -> 
               <div id="loginContainer"> {- new id -}
	         <form action="/login" method="post"> 
		    <div> 
			{- email and password need some javascriptwatermark-effect for the text -}
			{- changed from type="textfield" to type="text" (type="textfield" is not valid) -}
			<input type="email" infotext="Användarnamn" name="email" autocomplete="off" /> 
			<input type="password" name="password" infotext="password" autocomplete="off" /><br /> 
		        <a href=LinkForgotPassword> Glömt lösenord</a> 
                        <input type="submit" value="Logga in" name="login" class="button" /> 
		    </div> 
	         </form> 
               </div> 
           %>

        
         <div id="nav">
          <% case ctxmaybeuser of 
               Just _ ->
                 <ul>
                   <li><% topnavi (topMenu== TopNew) ctx "Skapa avtal" LinkMain %></li>
                   <li><% topnavi (topMenu== TopDocument) ctx "Arkiv" LinkIssue %></li>
                 </ul>
               _ -> <span/>
           %>
         </div>
     
        <div class="clearboth"/>
      </div>
      <div id="mainContainer">
          <% body %>
      </div>
      </div>
      
      
      <div id="footerContainer">
       <div id="footerContainer2">
        <ul>
          <li class="footerCategoryHeader"> 
           Tjänsten
          </li>
          <li>
           <a href="/why.html">Varför SkrivaPå?</a>
          </li>
          <li>
           <a href="/features.html">Funktioner och fördelar</a>
          </li>
          <li>
           <a href="/pricing.html">Prisplan</a>
          </li>
        </ul>

        <ul>
          <li class="footerCategoryHeader"> 
           Trygghet och villkor
          </li>
          <li>
           <a href="/security.html">Säkerhet</a>
          </li>
          <li>
           <a href="/legal.html">Juridik</a>
          </li>
          <li>
           <a href="/privacypolicy.html">Sekretesspolicy</a>
          </li>
          <li>
           <a href="/termsofuse.html">Allmäna Villkor</a>
          </li>
        </ul>

        <ul>  
          <li class="footerCategoryHeader"> 
           SkrivaPå
          </li>
          <li>
           <a href="/contact.html">Kontakt</a>
          </li>
        </ul>
		
		<div id="copy"><% cdata "&copy;" %> 2010 SkrivaPå</div> 
       </div>
      </div>

      <script type="text/javascript">
        var _gaq = _gaq || [];
        _gaq.push(['_setAccount', 'UA-6387711-9']);
        _gaq.push(['_trackPageview']);

        (function() {
          var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
          ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
          var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
          })();

      </script>
		
     </body>
    </html>


showUserOption :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] BSC.ByteString)) 
                  => User -> XMLGenT m (HSX.XML m)
showUserOption user =
    <option value=(show $ userid user) title=(unEmail $ useremail user)>
          <% userfullname user %> <% unEmail $ useremail user %>
    </option>
 
statsPageView :: Int -> Int -> [User] -> BS.ByteString -> HSP XML
statsPageView nusers ndocuments users df =
    developmentWrapper "Stats page" []
     <div>
      <h1>Stats page</h1>
      <table>
       <tr><td>Users</td><td><% show nusers %></td></tr>
       <tr><td>Documents</td><td><% show ndocuments %></td></tr>
      </table>
      <br/>
      <form method="post" action="/createuser">
       Create user:<br/> 
       <table>
        <tr>
         <td>Full name:</td>
         <td><input type="text" name="fullname"/></td>
        </tr>
        <tr>
         <td>Email address:</td>
         <td><input type="email" name="email"/></td>
        </tr>
       </table><br/>
       <input type="submit" value="Create user"/>
      </form>
      <br/>
      Disk status:<br/>
      <pre><% df %></pre>
     </div>

signupConfirmPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
               => Context -> XMLGenT m (HSX.XML m)
signupConfirmPageView ctx =
    <div>Please check your email for your password.</div>

data SignupForm = SignupForm {
    signupFirstname::String,
    signupLastname::String,
    signupEmail::String,
    signupPassword::String,
    signupPassword2::String }
    
instance FromData SignupForm where
    fromData = do
        firstname <- look "firstname"
        lastname <- look "lastname"
        email <- look "email"
        password <- look "password"
        password2 <- look "password2"
        return $ SignupForm {
            signupFirstname = firstname,
            signupLastname = lastname,
            signupEmail = email,
            signupPassword = password,
            signupPassword2 = password2
        }
        
getFormValue :: (Maybe a) -> (a -> String) -> String
getFormValue Nothing _ = ""
getFormValue (Just x) f = f x

signupPageFlashMessageBox :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) => [String] -> XMLGenT m (HSX.XML m)
signupPageFlashMessageBox [] = <div></div>
signupPageFlashMessageBox errors =
    <div class="flashmsgbox"><% errors %></div>

signupPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
               => [String] -> Maybe SignupForm -> XMLGenT m (HSX.XML m)
signupPageView errors form =
    <div>
        <% signupPageFlashMessageBox errors %>
        <form action=LinkSignup method="post">
            <table>
                <tr>
                    <td>First name</td>
                    <td><input name="firstname" value=(getFormValue form signupFirstname) /></td>
                </tr>
                <tr>
                    <td>Last name</td>
                    <td><input name="lastname" value=(getFormValue form signupLastname) /></td>
                </tr>
                <tr>
                    <td>Email</td>
                    <td><input type="email" name="email" value=(getFormValue form signupEmail) /></td>
                </tr>
                <tr>
                    <td>Password</td>
                    <td><input name="password" type="password" /></td>
                </tr>
                <tr>
                    <td>Password again</td>
                    <td><input name="password2" type="password" /></td>
                </tr>
            </table>
            <input type="submit" value="Create Account" />
        </form>
    </div>

forgotPasswordPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
               => XMLGenT m (HSX.XML m)
forgotPasswordPageView =
  <div class="centerdivnarrow">
    <form action=LinkForgotPassword method="post">
      <table>
        <tr>
          <td>E-mail</td>
          <td><input name="email" type="email"/></td>
        </tr>
      </table>
      <input type="submit" value="Skicka nytt lösenord" />
    </form>
  </div>

forgotPasswordConfirmPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
                                 => Context
                              -> XMLGenT m (HSX.XML m)
forgotPasswordConfirmPageView ctx =
  <div class="centerdivnarrow">
    <p>Ett nytt lösenord har skickats till din e-post. Du kan nu logga in med dina nya uppgifter.</p>
    <% loginBox ctx %>
  </div>

loginPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
               => Context -> XMLGenT m (HSX.XML m)
loginPageView ctx = 
  <div class="centerdivnarrow">

   <p class="headline">Logga in SkrivaPå!</p>

   <% loginBox ctx %>

  </div>


developmentWrapper :: (EmbedAsChild (HSPT' IO) body) 
                   => String 
                   -> [FlashMessage]
                   -> body 
                   -> HSP XML
developmentWrapper title ctxflashmessages body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <% globalScriptsAndStyles %>
     </head>
     <body>
       <%
         -- (lift get) >>= \(Context {ctxflashmessages}) -> 
           case ctxflashmessages of
             [] -> <% () %>
             _ -> <% 
                   <div class="flashmsgbox">
                     <% ctxflashmessages %>
                   </div> 
                   %>
       %>
      <% body %>
     </body>
    </html>

databaseContents :: [String] -> HSP XML
databaseContents contents = developmentWrapper "All database files" []
  <div>
     <h1>All database files</h1>
     <ul>
      <% map showOneFile contents %>
     </ul>
  </div>
  where showOneFile file = <li><a href=file><% file %></a></li>


showUserSelect users = 
  <select name="user">
    <option value="" title="-- select user --">
      <% "-- select user --" %>
    </option>
    <% map showUserOption users %>
  </select>
  
showAdminOnly users ctxflashmessages =
    developmentWrapper "SkrivaPa admin only page" ctxflashmessages
    <div>
     <h3>Database files</h3>
     <a href="/adminonly/db/">Show list of database files</a><br/>
     <form method="post" action="/adminonly/cleanup">
     Database incrementally stores backups of itself. Old copies aren't useful in normal situations,
     just in case of failure. It is good to remove old files from time to time.<br/>
     <input type="submit" value="Cleanup"/><br/>
     </form>
     <h3>Users</h3>
      You can see <a href="/adminonly/alluserstable">big table of all users</a>.<br/>
      <form method="post" action="/adminonly/become">
       Here you can see the world in the same way as a user sees it. You cannot do any 
       actions on behalf of that user though: 
       <table>
        <tr>
         <td>
          <% showUserSelect users %>
         </td>
         <td>
          <input type="submit" value="Monitor user"/>
         </td>
        </tr>
       </table>
      </form>
      <br/>
      <form method="post" action="/adminonly/takeoverdocuments">
       Here you can take over documents from other account. This action reassigns all documents of the user you 
       select to your own accout. Watch out, this operation cannot be reversed! This should probably be moved
       to all accounts when people whould like to merge accounts. As a password you need to user the other account
       password or 'skrivapaadmin' when you are SkrivaPa administrator.
       <br/> 
          User: <% showUserSelect users %><br/>
          Password: <input type="password" name="password"/><br/>
          <input type="submit" value="Take over documents from account"/>
      </form>
      <br/>
      <form method="post" action="/adminonly/deleteaccount">
       Here you can remove an account from the system. For safety reasons you can remove only accounts that
       have no documents in them (use Take over documents above to free accounts). This action cannot be reversed
       so beware!
       <br/> 
          User: <% showUserSelect users %><br/>
          <input type="submit" value="Annihilate account"/>
      </form>
    </div>


userInfo (user,docs) = <tr><td><% userfullname user %></td><td><% unEmail $ useremail user %></td><td><% show docs %></td></tr>

pageAllUsersTable :: [(User,Int)] -> HSP XML
pageAllUsersTable users =
    developmentWrapper "Alla SkrivaPå anwender" []
     <div>
      <h1>Alla SkrivaPå anwender</h1>
      <table>
       <thead><td>Username</td><td>Email</td><td>Docs</td></thead>
       <% map userInfo users %>
      </table>
     </div>

acceptTermsOfServicePage :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
                            => String -> XMLGenT m (HSX.XML m)
acceptTermsOfServicePage red = 
                       <form method="post" action="/tos">
                        <input type="hidden" name="redirect" id="redirect" value=red />
                        <input type="checkbox" name="tos" id="tos">I accept and agree to the Terms of Service</input>
                        <input type="submit" />
                       </form>






